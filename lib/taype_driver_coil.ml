open Taype_driver
open Containers
open Sexplib

type nf = In of int | Var of int | Const of int
type bexp = Eq of nf * nf | Lt of nf * nf

type aexp =
  | Nf of nf
  | Mux of bexp * nf * nf
  | Add of nf * nf
  | Sub of nf * nf
  | Mul of nf * nf

let in_c = ref 0
let var_c = ref 0
let ctx : (int, aexp) List.Assoc.t ref = ref []

let mk_var () =
  let v = !var_c in
  var_c := v + 1;
  v

let mk_in () =
  let v = !in_c in
  in_c := v + 1;
  v

let extend_ctx x =
  let v = mk_var () in
  ctx := (v, x) :: !ctx;
  Var v

let zero = Const 0
let one = Const 1
let ite s m n = Mux (Lt (zero, s), m, n) |> extend_ctx

module OInt = struct
  type t = nf

  let setup_driver _ _ _ = function
    | Party.Public ->
        in_c := 0;
        var_c := 0;
        ctx := []
    | Party.Trusted -> raise Unsupported
    | Party.Private _ -> raise Unknown_party

  let finalize_driver () = ()
  let collect_stat () = raise Unsupported
  let report_stat () = raise Unsupported

  let make x = function
    | Party.Public -> Nf (Const x) |> extend_ctx
    | _ -> raise Unsupported

  let arbitrary = function
    | Party.Public -> Nf (Const 0) |> extend_ctx
    | Party.Trusted -> In (mk_in ())
    | Party.Private _ -> raise Unsupported

  let reveal_int _ = raise Unsupported
  let mux s m n = ite s m n
  let add m n = Add (m, n) |> extend_ctx
  let sub m n = Sub (m, n) |> extend_ctx
  let mul m n = Mul (m, n) |> extend_ctx
  let div _ _ = raise Unsupported
  let eq m n = Mux (Eq (m, n), one, zero) |> extend_ctx
  let le m n = Mux (Lt (n, m), zero, one) |> extend_ctx
  let band m n = ite m (ite n one zero) zero
  let bor m n = ite m one (ite n one zero)
  let bnot n = ite n zero one
end

module Driver = Make (OInt)

let pp_let pp_var pp_exp =
  Format.dprintf "@[<hv>let %t =@;<1 2>%t@ in@]" pp_var pp_exp

let pp_nf = function
  | In n -> Format.dprintf "i%d" n
  | Var n -> Format.dprintf "x%d" n
  | Const n -> Format.dprintf "%d" n

let pp_input n fmt =
  for i = 0 to n - 1 do
    Format.fprintf fmt "%t@." @@ pp_let (pp_nf (In i)) (Format.dprintf "&%d" i)
  done

let pp_output =
  Format.dprintf "@[<2>[ %a ]@]"
    (Format.array (Fun.flip pp_nf) ~sep:(Format.return ";@ "))

let pp_bin op m n = Format.dprintf "(%t %s %t)" (pp_nf m) op (pp_nf n)

let pp_bexp = function
  | Eq (m, n) -> pp_bin "==" m n
  | Lt (m, n) -> pp_bin "<" m n

let pp_aexp = function
  | Nf nf -> pp_nf nf
  | Mux (s, m, n) ->
      Format.dprintf "@[<hv>if %t {@;<1 2>%t@ } else {@;<1 2>%t@ }@]"
        (pp_bexp s) (pp_nf m) (pp_nf n)
  | Add (m, n) -> pp_bin "+" m n
  | Sub (m, n) -> pp_bin "-" m n
  | Mul (m, n) -> pp_bin "*" m n

let pp_ctx ctx fmt =
  let pp (i, e) =
    Format.fprintf fmt "%t@." @@ pp_let (pp_nf (Var i)) (pp_aexp e)
  in
  List.iter pp ctx

let nf_inline_const tbl = function
  | Var x ->
      let rec trace x =
        match tbl.(x) with Nf (Var y) -> trace y | Nf nf -> nf | _ -> Var x
      in
      trace x
  | nf -> nf

let aexp_inline_const tbl e =
  let go_nf = nf_inline_const tbl in
  let go_bexp = function
    | Eq (m, n) -> Eq (go_nf m, go_nf n)
    | Lt (m, n) -> Lt (go_nf m, go_nf n)
  in
  match e with
  | Nf nf -> Nf nf
  | Mux (s, m, n) -> Mux (go_bexp s, go_nf m, go_nf n)
  | Add (m, n) -> Add (go_nf m, go_nf n)
  | Sub (m, n) -> Sub (go_nf m, go_nf n)
  | Mul (m, n) -> Mul (go_nf m, go_nf n)

let optimize_mux tbl e =
  let bexp_reify = function
    | Eq (Const m, Const n) -> `Zero (m = n)
    | Eq (Const m, Var x) -> `One (x, fun n -> m = n)
    | Eq (Var x, Const n) -> `One (x, fun m -> m = n)
    | Lt (Const m, Const n) -> `Zero (m < n)
    | Lt (Const m, Var x) -> `One (x, fun n -> m < n)
    | Lt (Var x, Const n) -> `One (x, fun m -> m < n)
    | _ -> `Nah
  in
  match e with
  | Mux (s, m, n) -> (
      match bexp_reify s with
      | `Zero b -> Nf (if b then m else n)
      | `One (x, f) -> (
          match tbl.(x) with
          | Mux (s', Const a, Const b) ->
              Mux (s', (if f a then m else n), if f b then m else n)
          | _ -> e)
      | `Nah -> e)
  | e -> e

let nf_vars = function Var x -> [ x ] | _ -> []

let bexp_vars = function
  | Eq (m, n) -> nf_vars m @ nf_vars n
  | Lt (m, n) -> nf_vars m @ nf_vars n

let aexp_vars = function
  | Nf nf -> nf_vars nf
  | Mux (s, m, n) -> bexp_vars s @ nf_vars m @ nf_vars n
  | Add (m, n) -> nf_vars m @ nf_vars n
  | Sub (m, n) -> nf_vars m @ nf_vars n
  | Mul (m, n) -> nf_vars m @ nf_vars n

let mark tbl reachable =
  let rec go = function
    | [] -> ()
    | x :: l ->
        if reachable.(x) then go l
        else (
          reachable.(x) <- true;
          go (aexp_vars tbl.(x) @ l))
  in
  go

(* We do not need to do a thorough optimization; just to ease the coil
   optimizer. *)
(* The output array [a] may be modified. *)
let optimize ctx a =
  let v_tbl = Array.make !var_c (Nf (Const 0)) in
  let exp_tbl : (aexp, int) Hashtbl.t = Hashtbl.create 1024 in
  let go (x, e) =
    let e = optimize_mux v_tbl (aexp_inline_const v_tbl e) in
    (* Common expression elimination *)
    let e =
      match Hashtbl.get exp_tbl e with
      | Some y -> Nf (Var y)
      | None ->
          Hashtbl.add exp_tbl e x;
          e
    in
    v_tbl.(x) <- e
  in
  List.iter go ctx;
  Array.map_inplace (nf_inline_const v_tbl) a;
  (* Dead code elimination *)
  let reachable = Array.make !var_c false in
  Array.iter (fun v -> mark v_tbl reachable (nf_vars v)) a;
  List.filter_map
    (fun (x, _) -> if reachable.(x) then Some (x, v_tbl.(x)) else None)
    ctx

let pp_coil ?(optimization = true) a =
  let a = Driver.obliv_array_to_array a in
  let ctx = List.rev !ctx in
  let ctx = if optimization then optimize ctx a else ctx in
  Format.dprintf "%t@.%t@.%t@." (pp_input !in_c) (pp_ctx ctx) (pp_output a)

let print_coil ?(optimization = true) a =
  Format.printf "%t" (pp_coil a ~optimization)

let write_coil ?(optimization = true) name a =
  Format.to_file (name ^ ".pita") "%t" (pp_coil a ~optimization)

let call_coil cmd name =
  let rc = Sys.command @@ Printf.sprintf "./runcoil %s %S" cmd name in
  if rc <> 0 then failwith ("external program exited with " ^ string_of_int rc)

module Ser = struct
  type 'a t = 'a -> Driver.obliv_array * Sexp.t

  let simple a = (a, Conv.sexp_of_unit ())
  let int = simple
  let bool = simple

  let pair t1 t2 (x1, x2) =
    let a1, s1 = t1 x1 in
    let a2, s2 = t2 x2 in
    (Driver.obliv_array_concat a1 a2, Conv.sexp_of_pair Fun.id Fun.id (s1, s2))

  let oadt to_sexp to_size (v, a) =
    (a, Conv.sexp_of_pair to_sexp Conv.sexp_of_int (v, to_size v))
end

module Deser = struct
  type 'a t = Driver.Plaintext.obliv_array -> Sexp.t -> 'a * int

  let simple r a s =
    let () = Conv.unit_of_sexp s in
    (r (Driver.Plaintext.obliv_array_slice a 0 1), 1)

  let int = simple Driver.Plaintext.obliv_int_r
  let bool = simple Driver.Plaintext.obliv_bool_r

  let pair t1 t2 a s =
    let s1, s2 = Conv.pair_of_sexp Fun.id Fun.id s in
    let x1, n1 = t1 a s1 in
    let x2, n2 =
      t2
        (Driver.Plaintext.obliv_array_slice a n1
           (Driver.Plaintext.obliv_array_length a - n1))
        s2
    in
    ((x1, x2), n1 + n2)

  let oadt of_sexp r a s =
    let v, n = Conv.pair_of_sexp of_sexp Conv.int_of_sexp s in
    (r (v, Driver.Plaintext.obliv_array_slice a 0 n), n)
end

let compile_coil ?(optimization = true) name x ser =
  let a, s = ser x in
  write_coil name a ~optimization;
  Sexp.save (name ^ ".view") s;
  call_coil "compile" name

let execute_coil name ia =
  let in_file = name ^ ".input" in
  let write_input oc = Array.iter (Printf.fprintf oc "%d\n") ia in
  IO.with_out in_file write_input;
  call_coil "run" name;
  let out_file = name ^ ".output" in
  let rec loop ic l =
    try
      let n = Scanf.bscanf ic "%d " Fun.id in
      loop ic (n :: l)
    with End_of_file -> l
  in
  let l =
    IO.with_in out_file (fun ic -> loop (Scanf.Scanning.from_channel ic) [])
  in
  Array.of_list (List.rev l)

let run_coil name l deser =
  let ia = List.map Driver.Plaintext.to_array l |> Array.concat in
  let a = execute_coil name ia |> Driver.Plaintext.of_array in
  let s = Sexp.load_sexp (name ^ ".view") in
  deser a s |> fst
