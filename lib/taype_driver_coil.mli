module Driver : Taype_driver.S

val print_coil : ?optimization:bool -> Driver.obliv_array -> unit
val write_coil : ?optimization:bool -> string -> Driver.obliv_array -> unit

val compile_coil :
  ?optimization:bool ->
  string ->
  'a * Driver.obliv_array ->
  ('a -> out_channel -> unit) ->
  unit

val compile_coil_simple :
  ?optimization:bool -> string -> Driver.obliv_array -> unit

val execute_coil : string -> int array -> int array

val run_coil :
  string ->
  Driver.Plaintext.obliv_array list ->
  (in_channel -> 'a) ->
  'a * Driver.Plaintext.obliv_array

val run_coil_simple :
  string -> Driver.Plaintext.obliv_array list -> Driver.Plaintext.obliv_array
