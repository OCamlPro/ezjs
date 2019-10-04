open Js_types

let (>>=) = Lwt.(>>=)
let return = Lwt.return
let async = Lwt.async
let return_unit = Lwt.return_unit

class type ['a] promise = object
  method _then : ('a -> unit) callback -> 'a promise t meth
  method catch : ('b t -> unit) callback -> 'a promise t meth
end

let to_lwt (p : 'a promise t) =
  let waiter, notifier = Lwt.wait () in
  (p##_then (wrap_callback (fun x -> Lwt.wakeup notifier (Ok x))))##catch
    (wrap_callback (fun x -> Lwt.wakeup notifier (Error x))) |> ignore;
  waiter

let to_lwt_exn (p : 'a promise t) =
  let waiter, notifier = Lwt.wait () in
  p##_then (wrap_callback (Lwt.wakeup notifier)) |> ignore;
  waiter

let to_lwt_cb f =
  let waiter, notifier = Lwt.wait () in
  f (wrap_callback (Lwt.wakeup notifier)); waiter

let to_lwt_cb_tr tr f =
  let waiter, notifier = Lwt.wait () in
  f (wrap_callback (fun x -> Lwt.wakeup notifier (tr x))); waiter

let to_lwt_cb_opt callback f = match callback with
  | Some callback ->
    let waiter, notifier = Lwt.wait () in
    f (def (wrap_callback (Lwt.wakeup notifier)));
    waiter >>= fun x -> return (Some (callback x))
  | None -> f undefined; Lwt.return_none
