open Js_types

let (>>=) = Lwt.(>>=)
let return = Lwt.return
let async = Lwt.async
let return_unit = Lwt.return_unit

class type ['a, 'b] promise0 = object
  method _then : ('a -> unit) callback -> ('a, 'b) promise0 t meth
  method catch : ('b -> unit) callback -> ('a, 'b) promise0 t meth
end

class type any = object
end

type 'a promise = ('a, any t) promise0

type ('a,'b) promise_cs =
  ((('a -> unit) -> ('b -> unit) -> unit) callback
   -> ('a, 'b) promise0 t) constr

let promise f =
  let cs : ('a, 'b) promise_cs = global##._Promise in
  new%js cs (wrap_callback f)

let to_lwt (p : 'a promise t) =
  let waiter, notifier = Lwt.wait () in
  (p##_then (wrap_callback (fun x -> Lwt.wakeup notifier (Ok x))))##catch
    (wrap_callback (fun x -> Lwt.wakeup notifier (Error x))) |> ignore;
  waiter

let to_lwt_exn (p : 'a promise t) =
  let waiter, notifier = Lwt.wait () in
  p##_then (wrap_callback (Lwt.wakeup notifier)) |> ignore;
  waiter

let to_lwt_cb0 f =
  let waiter, notifier = Lwt.wait () in
  f (Lwt.wakeup notifier); waiter

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
