open Taype_driver
open Containers

type var = In of int | Var of int

type ast =
  | Enc of int
  | Mux of var * var * var
  | Add of var * var
  | Sub of var * var
  | Mul of var * var
  | Div of var * var
  | Eq of var * var
  | Le of var * var
  | And of var * var
  | Or of var * var
  | Not of var

let in_c = ref 0
let var_c = ref 0
let ctx : ast list ref = ref []

let make_var () =
  let v = !var_c in
  var_c := v + 1;
  v

let make_in () =
  let v = !in_c in
  in_c := v + 1;
  v

let extend_ctx x =
  let v = make_var () in
  ctx := x :: !ctx;
  Var v

module OInt = struct
  type t = var

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
    | Party.Public -> Enc x |> extend_ctx
    | _ -> raise Unsupported

  let arbitrary = function
    | Party.Public -> Enc 0 |> extend_ctx
    | Party.Trusted -> In (make_in ())
    | Party.Private _ -> raise Unsupported

  let reveal_int _ = raise Unsupported
  let mux s m n = Mux (s, m, n) |> extend_ctx
  let add m n = Add (m, n) |> extend_ctx
  let sub m n = Sub (m, n) |> extend_ctx
  let mul m n = Mul (m, n) |> extend_ctx
  let div m n = Div (m, n) |> extend_ctx
  let eq m n = Eq (m, n) |> extend_ctx
  let le m n = Le (m, n) |> extend_ctx
  let band m n = And (m, n) |> extend_ctx
  let bor m n = Or (m, n) |> extend_ctx
  let bnot n = Not n |> extend_ctx
end

module Driver = Make (OInt)

let string_of_var = function
  | In x -> "i" ^ string_of_int x
  | Var x -> "x" ^ string_of_int x

let sexp_of_var v = Sexp.atom (string_of_var v)
let sexp_of_vars = List.map sexp_of_var

let sexp_of_ast : ast -> Sexp.t = function
  | Enc n -> Sexp.of_variant "enc" [ Sexp.of_int n ]
  | Mux (s, m, n) -> Sexp.of_variant "mux" (sexp_of_vars [ s; m; n ])
  | Add (m, n) -> Sexp.of_variant "+" (sexp_of_vars [ m; n ])
  | Sub (m, n) -> Sexp.of_variant "-" (sexp_of_vars [ m; n ])
  | Mul (m, n) -> Sexp.of_variant "*" (sexp_of_vars [ m; n ])
  | Div (m, n) -> Sexp.of_variant "/" (sexp_of_vars [ m; n ])
  | Eq (m, n) -> Sexp.of_variant "=" (sexp_of_vars [ m; n ])
  | Le (m, n) -> Sexp.of_variant "<=" (sexp_of_vars [ m; n ])
  | And (m, n) -> Sexp.of_variant "and" (sexp_of_vars [ m; n ])
  | Or (m, n) -> Sexp.of_variant "or" (sexp_of_vars [ m; n ])
  | Not n -> Sexp.of_variant "not" (sexp_of_vars [ n ])

let sexp_of_coil : Driver.obliv_array -> Sexp.t =
 fun a ->
  let ctx =
    List.mapi
      (fun i ast -> Sexp.list [ sexp_of_var (Var i); sexp_of_ast ast ])
      (List.rev !ctx)
  in
  let output =
    Array.to_list (Driver.obliv_array_to_array a) |> sexp_of_vars |> Sexp.list
  in
  Sexp.list [ Sexp.atom "let"; Sexp.list ctx; output ]

let print_coil a = Sexp.to_chan stdout (sexp_of_coil a)
