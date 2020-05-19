(lang dune 2.5)
(name ocplib-jsutils)
(allow_approximate_merlin)

(generate_opam_files true)

(source (github ocamlpro/ocplib-jsutils))
(license GPL-3.0-or-later)
(authors
 "Cagdas Bozman <cagdas.bozman@ocamlpro.com>"
 "Michael Laporte <michael.laporte@ocamlpro.com>"
 "Fabrice Le Fessant <fabrice.le_fessant@ocamlpro.com>"
 "Maxime Levillain <maxime.levillain@ocamlpro.com>"
 "Steven de Oliveira <steven.de_oliveira@ocamlpro.com>")
(maintainers "Fabrice Le Fessant <fabrice.le_fessant@ocamlpro.com>")

(package
 (name ocplib-jsutils)
 (synopsis "A collection of useful libraries to build sites with js_of_ocaml")
 (description "A collection of useful libraries to build sites with js_of_ocaml")
 (documentation "https://ocamlpro.github.io/ocplib-jsutils/doc")
 (depends
  (js_of_ocaml-tyxml (>= 3.4.0))
  (base64 (>= 3.2.0))
  (lwt (>= 4.0.0))
  (stdlib-shims (>= 0.1.0))))
