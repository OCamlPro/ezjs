(lang dune 2.5)
(allow_approximate_merlin)
(name ezjs_utils)

(generate_opam_files true)

(source (github ocamlpro/ezjs))
(license GPL-3.0-or-later)
(authors
 "Cagdas Bozman <cagdas.bozman@ocamlpro.com>"
 "Michael Laporte <michael.laporte@ocamlpro.com>"
 "Fabrice Le Fessant <fabrice.le_fessant@ocamlpro.com>"
 "Maxime Levillain <maxime.levillain@ocamlpro.com>"
 "Steven de Oliveira <steven.de_oliveira@ocamlpro.com>")
(maintainers "Fabrice Le Fessant <fabrice.le_fessant@ocamlpro.com>")
(documentation "https://ocamlpro.github.io/ezjs/doc")

(package
 (name ezjs_utils)
 (synopsis "A collection of useful libraries to build sites with js_of_ocaml")
 (depends
  (js_of_ocaml-ppx (>= 3.4.0))
  stdlib-shims)
 (depopts
  (lwt (>= 5.3.0))
  (base64 (>= 3.2.0))
  (js_of_ocaml-tyxml (>= 5.5.0))))

(package
 (name ezjs_amcharts3)
 (synopsis "Amcharts3 binding")
 (depends ezjs_utils))

(package
 (name ezjs_bs4)
 (synopsis "Bootstrap4 helpers")
 (depends
  ezjs_utils ezjs_cleave ezjs_jquery))

(package
 (name ezjs_disqus)
 (synopsis "Binding for Disqus")
 (depends (js_of_ocaml-ppx (>= 5.5.0))))

(package
 (name ezjs_gauth)
 (synopsis "Binding for Google Sign in")
 (depends (ezjs_min (>= 0.2))))
