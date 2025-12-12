open! Core
open Js_of_ocaml

(* Firebase Auth bindings *)
module Auth = struct
  type user_t = < > Js.t
  type auth_result = < > Js.t

  let sign_in_anonymously () : unit =
    let firebase_auth = Js.Unsafe.get Js.Unsafe.global "firebaseAuth" in
    let promise = Js.Unsafe.meth_call firebase_auth "signInAnonymously" [||] in
    ignore promise
  ;;

  (* Create account with email/password *)
  let create_user_with_email_and_password (email : string) (password : string) (callback : (string, string) result -> unit) : unit =
    let firebase_auth = Js.Unsafe.get Js.Unsafe.global "firebaseAuth" in
    let email_js = Js.string email in
    let password_js = Js.string password in
    let promise = Js.Unsafe.fun_call
      (Js.Unsafe.get firebase_auth "createUserWithEmailAndPassword")
      [| Js.Unsafe.inject firebase_auth; Js.Unsafe.inject email_js; Js.Unsafe.inject password_js |]
    in
    let then_callback = Js.wrap_callback (fun user_credential ->
      let user = Js.Unsafe.get user_credential "user" in
      let user_id = Js.to_string (Js.Unsafe.get user "uid") in
      callback (Ok user_id)
    ) in
    let catch_callback = Js.wrap_callback (fun error ->
      let error_message = Js.to_string (Js.Unsafe.get error "message") in
      callback (Error error_message)
    ) in
    ignore (Js.Unsafe.meth_call promise "then" [| Js.Unsafe.inject then_callback |]);
    ignore (Js.Unsafe.meth_call promise "catch" [| Js.Unsafe.inject catch_callback |])
  ;;

  (* Sign in with email/password *)
  let sign_in_with_email_and_password (email : string) (password : string) (callback : (string, string) result -> unit) : unit =
    let firebase_auth = Js.Unsafe.get Js.Unsafe.global "firebaseAuth" in
    let email_js = Js.string email in
    let password_js = Js.string password in
    let promise = Js.Unsafe.fun_call
      (Js.Unsafe.get firebase_auth "signInWithEmailAndPassword")
      [| Js.Unsafe.inject firebase_auth; Js.Unsafe.inject email_js; Js.Unsafe.inject password_js |]
    in
    let then_callback = Js.wrap_callback (fun user_credential ->
      let user = Js.Unsafe.get user_credential "user" in
      let user_id = Js.to_string (Js.Unsafe.get user "uid") in
      callback (Ok user_id)
    ) in
    let catch_callback = Js.wrap_callback (fun error ->
      let error_message = Js.to_string (Js.Unsafe.get error "message") in
      callback (Error error_message)
    ) in
    ignore (Js.Unsafe.meth_call promise "then" [| Js.Unsafe.inject then_callback |]);
    ignore (Js.Unsafe.meth_call promise "catch" [| Js.Unsafe.inject catch_callback |])
  ;;

  let on_auth_state_changed (callback : user_t Js.Opt.t -> unit) : unit =
    let firebase_auth = Js.Unsafe.get Js.Unsafe.global "firebaseAuth" in
    let js_callback = Js.wrap_callback (fun user -> callback user) in
    ignore (Js.Unsafe.meth_call firebase_auth "onAuthStateChanged"
      [| Js.Unsafe.inject js_callback |])
  ;;

  let get_current_user () : user_t Js.Opt.t =
    let firebase_auth = Js.Unsafe.get Js.Unsafe.global "firebaseAuth" in
    Js.Unsafe.meth_call firebase_auth "getCurrentUser" [||]
  ;;

  let get_user_id (user : user_t) : string =
    Js.to_string (Js.Unsafe.get user "uid")
  ;;
end

(* Firestore bindings *)
module Firestore = struct
  type document = < > Js.t
  type collection_ref = < > Js.t
  type doc_ref = < > Js.t
  type query_snapshot = < > Js.t
  type unsubscribe = unit -> unit

  let get_firestore () : < > Js.t =
    let firestore_obj = Js.Unsafe.get Js.Unsafe.global "firestore" in
    Js.Unsafe.get firestore_obj "db"
  ;;

  let collection (name : string) : collection_ref =
    let firestore_obj = Js.Unsafe.get Js.Unsafe.global "firestore" in
    Js.Unsafe.meth_call firestore_obj "collection"
      [| Js.Unsafe.inject (get_firestore ()); Js.Unsafe.inject (Js.string name) |]
  ;;

  let doc (collection_ref : collection_ref) (id : string) : doc_ref =
    let firestore_obj = Js.Unsafe.get Js.Unsafe.global "firestore" in
    Js.Unsafe.meth_call firestore_obj "doc"
      [| Js.Unsafe.inject collection_ref; Js.Unsafe.inject (Js.string id) |]
  ;;

  let set_doc (doc_ref : doc_ref) (data : < > Js.t) : unit =
    let firestore_obj = Js.Unsafe.get Js.Unsafe.global "firestore" in
    let promise = Js.Unsafe.meth_call firestore_obj "setDoc"
      [| Js.Unsafe.inject doc_ref; Js.Unsafe.inject data |]
    in
    ignore promise
  ;;

  let get_doc (doc_ref : doc_ref) (callback : document Js.Opt.t -> unit) : unit =
    let firestore_obj = Js.Unsafe.get Js.Unsafe.global "firestore" in
    let promise = Js.Unsafe.meth_call firestore_obj "getDoc"
      [| Js.Unsafe.inject doc_ref |]
    in
    let then_callback = Js.wrap_callback (fun doc_snap ->
      let exists = Js.to_bool (Js.Unsafe.meth_call doc_snap "exists" [||]) in
      if exists then
        callback (Js.Opt.return doc_snap)
      else
        callback Js.Opt.empty
    ) in
    ignore (Js.Unsafe.meth_call promise "then" [| Js.Unsafe.inject then_callback |])
  ;;

  let on_snapshot (doc_ref : doc_ref) (callback : document Js.Opt.t -> unit) : unsubscribe =
    let firestore_obj = Js.Unsafe.get Js.Unsafe.global "firestore" in
    let js_callback = Js.wrap_callback (fun doc_snap ->
      let exists = Js.to_bool (Js.Unsafe.meth_call doc_snap "exists" [||]) in
      if exists then
        callback (Js.Opt.return doc_snap)
      else
        callback Js.Opt.empty
    ) in
    let unsubscribe_fn = Js.Unsafe.meth_call firestore_obj "onSnapshot"
      [| Js.Unsafe.inject doc_ref; Js.Unsafe.inject js_callback |]
    in
    fun () -> Js.Unsafe.fun_call unsubscribe_fn [||]
  ;;

  let get_data (doc : document) : < > Js.t =
    Js.Unsafe.meth_call doc "data" [||]
  ;;

  let delete_doc (doc_ref : doc_ref) : unit =
    let firestore_obj = Js.Unsafe.get Js.Unsafe.global "firestore" in
    let promise = Js.Unsafe.meth_call firestore_obj "deleteDoc"
      [| Js.Unsafe.inject doc_ref |]
    in
    ignore promise
  ;;

  (* Helper to create a JavaScript object from OCaml values *)
  let create_js_object () : < > Js.t =
    Js.Unsafe.obj [||]
  ;;

  let set_field (obj : < > Js.t) (key : string) (value : 'a) : unit =
    Js.Unsafe.set obj (Js.string key) value
  ;;

  let get_field (obj : < > Js.t) (key : string) : 'a =
    Js.Unsafe.get obj (Js.string key)
  ;;

  let get_string_field (obj : < > Js.t) (key : string) : string =
    Js.to_string (get_field obj key)
  ;;

  let get_int_field (obj : < > Js.t) (key : string) : int =
    Js.parseInt (get_field obj key)
  ;;

  (* Add a document to a collection with auto-generated ID *)
  let add_doc (collection_ref : collection_ref) (data : < > Js.t) (callback : string -> unit) : unit =
    let firestore_obj = Js.Unsafe.get Js.Unsafe.global "firestore" in
    let promise = Js.Unsafe.meth_call firestore_obj "addDoc"
      [| Js.Unsafe.inject collection_ref; Js.Unsafe.inject data |]
    in
    let then_callback = Js.wrap_callback (fun doc_ref ->
      let doc_id = Js.to_string (Js.Unsafe.get doc_ref "id") in
      callback doc_id
    ) in
    ignore (Js.Unsafe.meth_call promise "then" [| Js.Unsafe.inject then_callback |])
  ;;

  (* Query collection *)
  let query_collection (collection_ref : collection_ref) (field : string) (op : string) (value : string) : < > Js.t =
    let firestore_obj = Js.Unsafe.get Js.Unsafe.global "firestore" in
    let where_fn = Js.Unsafe.get firestore_obj "where" in
    let query_fn = Js.Unsafe.get firestore_obj "query" in
    Js.Unsafe.fun_call query_fn
      [| Js.Unsafe.inject collection_ref
       ; Js.Unsafe.fun_call where_fn 
           [| Js.Unsafe.inject (Js.string field)
            ; Js.Unsafe.inject (Js.string op)
            ; Js.Unsafe.inject (Js.string value)
           |]
      |]
  ;;

  (* Listen to query results *)
  let on_query_snapshot (query : < > Js.t) (callback : < > Js.t list -> unit) : unsubscribe =
    let firestore_obj = Js.Unsafe.get Js.Unsafe.global "firestore" in
    let js_callback = Js.wrap_callback (fun query_snap ->
      let docs_array = Js.Unsafe.get query_snap "docs" in
      let docs = Js.to_array docs_array in
      let doc_list = List.map ~f:(fun doc -> Js.Unsafe.coerce doc) (Array.to_list docs) in
      callback doc_list
    ) in
    let unsubscribe_fn = Js.Unsafe.meth_call firestore_obj "onSnapshot"
      [| Js.Unsafe.inject query; Js.Unsafe.inject js_callback |]
    in
    fun () -> Js.Unsafe.fun_call unsubscribe_fn [||]
  ;;
end