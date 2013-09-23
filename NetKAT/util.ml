
exception Quit
exception Undo

let starts_with (s : string) (t : string) : bool =
  Str.string_match (Str.regexp_string s) t 0
  
let ends_with (s : string) (t : string) : bool =
  let n = String.length t - String.length s in
  if n < 0 then false else
  Str.string_match (Str.regexp_string s) t n
  
let output_endline (out : out_channel) (s : string) : unit =
  output_string out s;
  output_char out '\n'
  
let copy_lines in_channel out_channel : unit =
  try
    while true do
      output_endline out_channel (input_line in_channel)
    done
  with End_of_file -> ()
  
let rec range (min : int) (max : int) : int list =
  if max <= min then [] else
  min :: range (min + 1) max  
  
let rec removeDuplicates list =
  match list with
    [] -> []
  | x :: t -> x :: removeDuplicates (List.filter (fun y -> y <> x) t)

(*****************************************************
 * HashSet -- like in Java
 *****************************************************)
module type HashSet = sig
  type 'a t
  val make : unit -> 'a t
  val add : 'a t -> 'a -> unit
  val remove : 'a t -> 'a -> unit
  val mem : 'a t -> 'a -> bool
  val size : 'a t -> int
  val values : 'a t -> 'a list
  val iter : ('a -> unit) -> 'a t -> unit
end

module HashSet : HashSet = struct
  type 'a t = ('a, 'a) Hashtbl.t
  let make() : 'a t = Hashtbl.create 11
  let mem (h : 'a t) (x : 'a) = Hashtbl.mem h x
  let add (h : 'a t) (x : 'a) =
    if mem h x then () else Hashtbl.add h x x
  let remove (h : 'a t) (x : 'a) =
    while mem h x do
      Hashtbl.remove h x
    done
  let size : 'a t -> int = Hashtbl.length
  let values (h : 'a t) : 'a list =
    Hashtbl.fold (fun x y v -> x :: v) h []
  let iter (f : 'a -> unit) : 'a t -> unit =
    Hashtbl.iter (fun x y -> f x)
end

(*****************************************************
 * Stream of strings in length-lexicographic order --
 * use to create new variable names
 *****************************************************)
module type LexStream = sig
  type t
  val make : unit -> t
  val next : t -> string
end

module LexStream : LexStream = struct
  type t = int list ref 
  
  let rec inc (s : int list) : int list =
    match s with
      | [] -> [Char.code 'a']
      | x :: t ->
          if x < Char.code 'z' then (x + 1) :: t
          else Char.code 'a' :: inc t
        
  let make() : t = ref [Char.code 'a']

  let next (h : t) : string =
    let l = !h in
    h := inc l;
    String.concat "" (List.map (String.make 1) (List.map Char.chr (List.rev l)))
end

(*****************************************************
 * A source of fresh variable names, avoiding a given
 * set of strings
 *****************************************************)
module type Fresh = sig
  type t
  val make : string HashSet.t -> t
  val avoid : t -> string HashSet.t
  val next : t -> string
end

module Fresh : Fresh = struct
  type t = (string HashSet.t * LexStream.t) ref
        
  let make (avoid : string HashSet.t) : t = ref (avoid, LexStream.make())

  let avoid (s : t) : string HashSet.t = fst (!s)

  let next (s : t) : string =
    let (avoid, stream) = !s in
    let rec check n = if HashSet.mem avoid n then check (LexStream.next stream) else n in
    check (LexStream.next stream)
end

(*****************************************************
 * Substitutions - we model these as OCaml Hashtbls
 *****************************************************)
module type Subst = sig
  type ('a, 'b) t
  val make : unit -> ('a, 'b) t
  val contains_key : ('a, 'b) t -> 'a -> bool
  val contains_value : ('a, 'b) t -> 'b -> bool
  val add : ('a, 'b) t -> 'a -> 'b -> unit
  val remove : ('a, 'b) t -> 'a -> unit
  val size : ('a, 'b) t -> int
  val keys : ('a, 'b) t -> 'a list
  val values : ('a, 'b) t -> 'b list
  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  val lookup : ('a, 'b) t -> 'a -> 'b
  val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  
  (* Check whether two substitutions are consistent (agree on their *)
  (* intersection) using structural equality on values.  Return their *)
  (* union if so, raise Failure if not *)
  val consis : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  
  (* Deep equality *)
  val equal : ('a, 'b) t -> ('a, 'b) t -> bool
end

module Subst : Subst = struct
  type ('a, 'b) t = ('a, 'b) Hashtbl.t
  let make() : ('a, 'b) t = Hashtbl.create 11
  let contains_key (h : ('a, 'b) t) (x : 'a) = Hashtbl.mem h x
  let contains_value (h : ('a, 'b) t) (x : 'b) = failwith "Not implemented"
  let add (h : ('a, 'b) t) (x : 'a) (v : 'b) = Hashtbl.replace h x v
  let remove (h : ('a, 'b) t) (x : 'a) =
    while contains_key h x do
      Hashtbl.remove h x
    done
  let size = Hashtbl.length
  let fold = Hashtbl.fold
  let lookup = Hashtbl.find
  let iter = Hashtbl.iter
    let keys (h : ('a, 'b) t) : 'a list =
    fold (fun x y v -> x :: v) h []
  let values (h : ('a, 'b) t) : 'b list =
    fold (fun x y v -> y :: v) h []
    
  let consis (h1 : ('a, 'b) t) (h2 : ('a, 'b) t) : ('a, 'b) t =
    (* first check consistency *)
    iter (fun key value ->
      if (not (contains_key h2 key)) || lookup h2 key = value then ()
      else failwith "Not consistent") h1;
    (* they are consistent - now create the union *)
    let h3 = Hashtbl.copy h1 in
    iter (fun key value ->
      if contains_key h3 key then ()
      else add h3 key value) h2;
    h3
    
  let equal (h1 : ('a, 'b) t) (h2 : ('a, 'b) t) : bool =
    try
      iter (fun key value ->
        if lookup h2 key = value then ()
        else raise Not_found) h1;
      iter (fun key value ->
        if lookup h1 key = value then ()
        else raise Not_found) h2;
      true
    with Not_found -> false
end

  
