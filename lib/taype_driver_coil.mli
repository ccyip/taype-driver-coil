module Driver : Taype_driver.S

val print_coil : Driver.obliv_array -> unit
val write_coil : string -> Driver.obliv_array -> unit
val compile_coil : string -> Driver.obliv_array -> unit
val execute_coil : string -> int array -> int array

val run_coil :
  string -> Driver.Plaintext.obliv_array list -> Driver.Plaintext.obliv_array
