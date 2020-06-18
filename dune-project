(lang dune 2.5)
(allow_approximate_merlin)
(name ezjs-min)

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
(documentation "https://ocamlpro.github.io/ocplib-jsutils/doc")

(package
 (name ezjs-utils)
 (synopsis "A collection of useful libraries to build sites with js_of_ocaml")
 (depends
  (js_of_ocaml-tyxml (>= 3.4.0))
  (lwt (>= 5.3.0))
  stdlib-shims)
 (depopts (base64 (>= 3.2.0))))

(package
 (name ezjs-min)
 (synopsis "A minimal helper for js and dom object")
 (depends
  (js_of_ocaml-ppx (>= 3.4.0))
  stdlib-shims))

(package
 (name ezjs-ace)
 (synopsis "Ace binding")
 (depends (js_of_ocaml-ppx (>= 3.4.0))))

(package
 (name ezjs-amcharts3)
 (synopsis "Amcharts3 binding")
 (depends ezjs-utils))

 (package
 (name ezjs-bs4)
 (synopsis "Bootstrap4 helpers")
 (depends
  (js_of_ocaml-ppx (>= 3.4.0))
  ezjs-cleave ezjs-jquery))

(package
 (name ezjs-cleave)
 (synopsis "Binding for Cleave JS")
 (depends (js_of_ocaml-ppx (>= 3.4.0))))

(package
 (name ezjs-cytoscape)
 (synopsis "Binding for Cytoscape")
 (depends ezjs-utils))

(package
 (name ezjs-d3pie)
 (synopsis "Binding for D3Pie")
 (depends (js_of_ocaml-ppx (>= 3.4.0))))

(package
 (name ezjs-disqus)
 (synopsis "Binding for Disqus")
 (depends ezjs-utils))

(package
 (name ezjs-extension)
 (synopsis "Binding for chrome and firefox extension API")
 (depends ezjs-min)
 (depopts (lwt (>= 5.3.0))))

(package
 (name ezjs-jquery)
 (synopsis "Binding for JQuery")
 (depends ezjs-utils))

(package
 (name ezjs-odometer)
 (synopsis "Binding for Odometer JS")
 (depends (js_of_ocaml-ppx (>= 3.4.0))))

(package
 (name ezjs-push)
 (synopsis "Binding for Push Notification JS")
 (depends (js_of_ocaml-ppx (>= 3.4.0))))

(package
 (name ezjs-qrcode)
 (synopsis "Binding for Qrcode JS")
 (depends (js_of_ocaml-ppx (>= 3.4.0))))

(package
 (name ezjs-recaptcha)
 (synopsis "Binding for Client Side Recaptcha")
 (depends (js_of_ocaml-ppx (>= 3.4.0))))

(package
 (name ezjs-blockies)
 (synopsis "Binding for Blockies")
 (depends (js_of_ocaml-ppx (>= 3.4.0))))

(package
 (name ezjs-timeline)
 (synopsis "Binding for TimelineJS")
 (depends (js_of_ocaml-ppx (>= 3.4.0))))

(package
 (name ezjs-fetch)
 (synopsis "Binding for Fetch")
 (depends ezjs-min))
