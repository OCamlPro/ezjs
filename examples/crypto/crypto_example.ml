open Ezjs_min.Js
open Ezjs_crypto

let () =
  generate_key ~algo:(GAES (CTR, 256)) ~usages:[`DeriveKey] @@ function
  | Error e -> js_log e
  | Ok (CKey k) -> log_str "key"; js_log @@ to_crypto_key k
  | Ok (CKeyPair p) -> log_str "keypair"; js_log @@ to_crypto_keypair p
