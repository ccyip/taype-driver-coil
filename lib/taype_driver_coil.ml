open Taype_driver
open Containers

type var = In of int | Var of int
type bexp = Eq of var * var | Lt of var * var

type aexp =
  | Enc of int
  | Mux of bexp * var * var
  | Add of var * var
  | Sub of var * var
  | Mul of var * var

let in_c = ref 0
let var_c = ref 0
let ctx : (int, aexp) List.Assoc.t ref = ref []

let init () =
  in_c := 0;
  var_c := 2;
  ctx := [ (1, Enc 1); (0, Enc 0) ]

let zero = Var 0
let one = Var 1

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

let ite s m n = Mux (Lt (zero, s), m, n) |> extend_ctx

module OInt = struct
  type t = var

  let setup_driver _ _ _ = function
    | Party.Public -> init ()
    | Party.Trusted -> raise Unsupported
    | Party.Private _ -> raise Unknown_party

  let finalize_driver () = ()
  let collect_stat () = raise Unsupported
  let report_stat () = raise Unsupported

  let make x = function
    | Party.Public -> Enc x |> extend_ctx
    | _ -> raise Unsupported

  let arbitrary = function
    | Party.Public -> Enc 0 |> extend_ctx
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
  let band m n = mul m n
  let bor m n = ite (add m n) one zero
  let bnot n = ite n zero one
end

module Driver = Make (OInt)

let pp_let pp_var pp_exp =
  Format.dprintf "@[<hv>let %t =@;<1 2>%t@ in@]" pp_var pp_exp

let pp_var = function
  | In n -> Format.dprintf "i%d" n
  | Var n -> Format.dprintf "x%d" n

let pp_input n fmt =
  for i = 0 to n - 1 do
    Format.fprintf fmt "%t@." @@ pp_let (pp_var (In i)) (Format.dprintf "&%d" i)
  done

let pp_output a =
  Format.dprintf "@[<2>[ %a ]@]"
    (Format.array (Fun.flip pp_var) ~sep:(Format.return ";@ "))
    (Driver.obliv_array_to_array a)

let pp_bin op m n = Format.dprintf "%t %s %t" (pp_var m) op (pp_var n)

let pp_bexp = function
  | Eq (m, n) -> pp_bin "==" m n
  | Lt (m, n) -> pp_bin "<" m n

let pp_aexp = function
  | Enc n -> Format.dprintf "%d" n
  | Mux (s, m, n) ->
      Format.dprintf "@[<hv>if (%t) {@;<1 2>%t@ } else {@;<1 2>%t@ }@]"
        (pp_bexp s) (pp_var m) (pp_var n)
  | Add (m, n) -> pp_bin "+" m n
  | Sub (m, n) -> pp_bin "-" m n
  | Mul (m, n) -> pp_bin "*" m n

let pp_ctx ctx fmt =
  let pp (i, e) =
    Format.fprintf fmt "%t@." @@ pp_let (pp_var (Var i)) (pp_aexp e)
  in
  List.iter pp ctx

let print_coil a =
  Format.printf "%t@.%t@.%t@." (pp_input !in_c)
    (pp_ctx (List.rev !ctx))
    (pp_output a)
