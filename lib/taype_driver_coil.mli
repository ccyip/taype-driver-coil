open Sexplib
module Driver : Taype_driver.S

val print_coil : ?optimization:bool -> Driver.obliv_array -> unit
val write_coil : ?optimization:bool -> string -> Driver.obliv_array -> unit

module Ser : sig
  type 'a t = 'a -> Driver.obliv_array * Sexp.t

  val simple : Driver.obliv_array t
  val int : Driver.obliv_array t
  val bool : Driver.obliv_array t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val oadt : ('a -> Sexp.t) -> ('a -> int) -> ('a * Driver.obliv_array) t
end

module Deser : sig
  type 'a t = Driver.Plaintext.obliv_array -> Sexp.t -> 'a * int

  val simple : (Driver.Plaintext.obliv_array -> 'a) -> 'a t
  val int : int t
  val bool : bool t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val oadt : (Sexp.t -> 'a) -> ('a * Driver.Plaintext.obliv_array -> 'b) -> 'b t
end

val compile_coil : ?optimization:bool -> string -> 'a -> 'a Ser.t -> unit
val execute_coil : string -> int array -> int array
val run_coil : string -> Driver.Plaintext.obliv_array list -> 'a Deser.t -> 'a
