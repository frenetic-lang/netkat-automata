(* sparse matrix representation *)

open Util
open Ast
open Term
open Base

module M : Map.S = Map.Make (String)

(* a complete assignment or a complete test is represented by a variable->value map *)
(* these are row and column indices of matrices *)
module Index : Map.OrderedType = struct
  type t = string M.t
  let compare = M.compare String.compare
end

module IndexPair = struct
  type t = Index.t * Index.t
  let compare ((f1,f2) : t) ((g1,g2) : t) : int =
    let c = Index.compare f1 g1 in
    if c <> 0 then c
    else Index.compare f2 g2
end
  
(* a matrix with entries 'a is represented by a map from a pair of Index.t objects *)
module Matrix = Map.Make (IndexPair)
type 'a matrix = 'a Matrix.t

(* for representing a matrix in row- or column-major order *)
module Vector = Map.Make (Index)
type 'a vector = 'a Vector.t
type 'a curried = ('a vector) vector

(* convert to row-major order *)
let row_major (m : 'a matrix) : 'a curried =
  let f ((k1,k2) : Matrix.key) (a : 'a) (v : 'a curried) : 'a curried =
    let g = if Vector.mem k1 v then Vector.find k1 v else Vector.empty in
    let w = Vector.add k2 a g in
    Vector.add k1 w v in
  Matrix.fold f m Vector.empty

(* convert to column-major order *)
let column_major (m : 'a matrix) : 'a curried =
  let f ((k1,k2) : Matrix.key) (a : 'a) (v : 'a curried) : 'a curried =
    let g = if Vector.mem k2 v then Vector.find k2 v else Vector.empty in
    let w = Vector.add k1 a g in
    Vector.add k2 w v in
  Matrix.fold f m Vector.empty

(* sparse matrix addition *)
let add (add_values : 'a -> 'a -> 'a) : 'a matrix -> 'a matrix -> 'a matrix =
  let f (k : Matrix.key) (a : 'a option) (b : 'a option) : 'a option =
    match (a,b) with
    | (None, None) -> None
    | (Some _, None) -> a
    | (None, Some _) -> b
    | (Some s, Some t) -> Some (add_values s t) in
  Matrix.merge f

(* sparse matrix mutiplication *)
let mul (zero : 'a) (add_values : 'a -> 'a -> 'a) (mul_values : 'a -> 'a -> 'a)
        (m1 : 'a matrix) (m2 : 'a matrix) : 'a matrix =        
	let innerprod (v1 : 'a vector) (v2 : 'a vector) : 'a =
	  let f (k1 : Vector.key) (a : 'a option) (b : 'a option) : 'a option =
	    match (a,b) with
	    | (None, _) -> None
	    | (_, None) -> None
	    | (Some s, Some t) -> Some (mul_values s t) in
	  let v = Vector.merge f v1 v2 in
	  Vector.fold (fun k x a -> add_values x a) v zero in  
	let outerprod (v1 : 'a curried) (v2 : 'a curried) : 'a matrix =
	  let f (k1 : Index.t) (a1 : 'a vector) (k2 : Index.t) (a2 : 'a vector) : 'a matrix -> 'a matrix =
      let a = innerprod a1 a2 in
      if a = zero then (fun m -> m)
	    else Matrix.add (k1,k2) a in
	  let g (k1 : Index.t) (a1 : 'a vector) : 'a matrix -> 'a matrix =
	    Vector.fold (f k1 a1) v2 in
	  Vector.fold g v1 Matrix.empty in
  outerprod (row_major m1) (column_major m2)


let dadd : term matrix -> term matrix -> term matrix =
  add add_terms
  
let dmul : term matrix -> term matrix -> term matrix =
  mul Term.Zero add_terms mul_terms
      
let eadd : bool matrix -> bool matrix -> bool matrix =
  add (||)
      
let emul : bool matrix -> bool matrix -> bool matrix =
  mul false (||) (&&)
  
(* from a given universe, create a set of IndexPairs *)
(* let indexset (univ : S.t) :  *)
      
(* create a diagonal matrix with term e *)
(* indices are determined by a given universe of variable-value pairs *)
let diag (e : 'a) : 'a matrix = failwith ""
  
