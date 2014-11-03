module D = Decide_Deriv.DerivTerm
module S = Sexplib.Sexp
module Base = Decide_Base.Base
                
open Core.Std
open Sexplib.Conv
open Decide_Util

module rec Term : sig
  type t = 
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSet.t
    | Times of t list 
    | Not of t
    | Star of t
    | Zero 
    | One with compare, sexp
end = struct 
  type t = 
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSet.t
    | Times of t list 
    | Not of t
    | Star of t
    | Zero 
    | One with compare, sexp
end and TermSet : sig
  include Set.S with type Elt.t = Term.t
end = Set.Make (struct
      type t = Term.t with compare, sexp
    end)

type compact_derivative = {
  left_hand : Term.t;
  right_hand : Term.t;
} with compare, sexp

  
module CompactDerivSet = Set.Make (struct
    type t = compact_derivative with compare, sexp
  end)

type d_matrix = CompactDerivSet.t

open Term
  
let term_append t e = Times [t; e]
    
let right_app d e = { d with right_hand = term_append d.right_hand e }
let left_app e d = { d with left_hand = term_append d.left_hand e }

let d_right_app ds e = CompactDerivSet.map ds (fun x -> right_app x e)
let d_left_app e ds = CompactDerivSet.map ds (left_app e)
                             
let rec compact_e_of_term t = match t with
  | Plus ts -> Plus (TermSet.map ts compact_e_of_term)
  | Dup -> Zero
  | Times ts -> Times (List.map ts compact_e_of_term)
  | Star t -> Star (compact_e_of_term t)
  | _ -> t
    
let rec compact_d_of_term t = match t with
  | Dup -> CompactDerivSet.singleton ({ left_hand = One; right_hand = One })
  | Plus ts -> TermSet.fold ts ~f:(fun acc t -> CompactDerivSet.union (compact_d_of_term t) acc) ~init:CompactDerivSet.empty
  | Times (t::ts) -> CompactDerivSet.union (d_right_app (compact_d_of_term t) (Times ts))
                       (d_left_app (compact_e_of_term t) (compact_d_of_term (Times ts)))
  | Star t -> d_left_app (Star t) (d_right_app (compact_d_of_term t) (Star t))
  | _ -> CompactDerivSet.empty
