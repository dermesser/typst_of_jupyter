(* Read more here: https://lewinb.net/posts/20_functional_json_access/ *)

module Json = Yojson.Basic
module Json_util = Json_util

(* A Json Result type [t] along with error indications provided as [op list]. *)
module JR : sig
  type op
  type 'a t
end

(* A Json document. *)
type doc = Json.t

(* Type [t] represents an operation on a Json document that can
   potentially fail (not found, wrong type, etc.). The failure
   path can be tracked. Operations can be composed. *)
type ('a, 'b) t

exception Json_object_error of string

(* Run the given operation on the supplied Json. *)
val extract_exn : (doc, 'a) t -> doc -> 'a

(* Run the given operation on the supplied Json, returning an error string if it failed. *)
val extract : (doc, 'a) t -> doc -> ('a, string) result

(* Extract or return provided default value.*)
val extract_or : default:'a -> (doc, 'a) t -> doc -> 'a

(* Compose two operations, with the result of the first feeding the second. *)
val ( >> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

(* Compose two operations, with the result of the first feeding the second. In case of an error during the first operation,
   the second will still be called. (Currently not really used) *)
val ( >>? ) : ('a, 'b) t -> ('b JR.t, 'c) t -> ('a, 'c) t

(* Lift a function into [t] *)
val lift : ('a -> 'b) -> ('a, 'b) t

(* Transform the result of an operation. *)
val map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t

(* Convert a Json value into an integer. *)
val int : (doc, int) t

(* Convert a Json value into a float. *)
val float : (doc, float) t

(* Convert a Json value into a float. *)
val string : (doc, string) t

(* Convert a Json value into a boolean. *)
val bool : (doc, bool) t

(* Convert a Json value into an alist *)
val assoc : (doc, (string, doc) Base.List.Assoc.t) t

(* Extract keys from a dict. *)
val keys : (doc, string list) t

(* Extract values from a dict *)
val values : (doc, doc list) t

(* Assert that a Json value is a dict. *)
val dict : (doc, doc) t

val list_index : int -> (doc, 'a) t -> (doc, 'a) t

(* Convert a Json value into a list of the given type. For example [list_of int]. An error is returned if any conversion fails. *)
val list_of : (doc, 'a) t -> (doc, 'a list) t

(* Convert a Json value into a list of the given type. Like [list_of]. Values failing conversion will be ignored. *)
val list_filtered_of : (doc, 'a) t -> (doc, 'a list) t

(* Extract a dict entry with the given key from the specified object. *)
val key : string -> (doc, 'a) t -> (doc, 'a) t

(* Extract a dict entry of type dict with the given key. Shortcut for [key "..." dict]. *)
val inner : string -> (doc, doc) t

(* Run both operations or fail. *)
val both : ('i, 'a) t -> ('i, 'b) t -> ('i, 'a * 'b) t

(* Run either of two operations, preferring the first and attempting the second otherwise. Useful if e.g. there are two keys to a dict entry of interest. *)
val either : ('i, 'a) t -> ('i, 'b) t -> ('i, ('a, 'b) Base.Either.t) t

(* Try both operations. *)
val alternative : ('i, 'a) t -> ('i, 'a) t -> ('i, 'a) t

(* Operator for [both] *)
val ( <+> ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t

(* Operator for [either] *)
val ( <|*> ) : ('a, 'b) t -> ('a, 'c) t -> ('a, ('b, 'c) Base.Either.t) t

(* Operator for [alternative] *)
val ( <|> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

(* If a previous extractor failed, use a default instead. Use with [(>>?)] *)
val default : 'a -> ('a JR.t, 'a) t

(* Quickly traverse a nested dict by specifying a path of keys and the type of the final value. *)
val path : string list -> (doc, 'a) t -> (doc, 'a) t
