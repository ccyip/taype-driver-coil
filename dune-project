(lang dune 3.7)

(generate_opam_files true)

(name taype-driver-coil)
(license "MIT")
(maintainers "Qianchuan Ye <yeqianchuan@gmail.com>")
(authors "Qianchuan Ye")
(source (github ccyip/taype-driver-coil))

(package
 (name taype-driver-coil)
 (depends taype-driver-lib containers)
 (synopsis "COIL driver for the Taype language")
)
