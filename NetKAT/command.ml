open Ast
open Util
open Match
open State
open Extract
open Proof

let parse_formula (s : string) : Ast.formula =
  Parser.parse_formula Lexer.token (Lexing.from_string s)

let parse_term (s : string) : Ast.term =
  Parser.parse_term Lexer.token (Lexing.from_string s)

let noargs (args : string list) : unit =
  if args = [] then () else failwith "Too many args"

let get_term (args : string list) : term =
  match args with
    [] -> failwith "Must specify term to substitute"
  | _ -> parse_term (String.concat " " args)

let get_task (state : state) : int * premise list * formula * task list * proof * formula =
  let (tasks, proof, theorem) = state in
  match tasks with
    [] -> failwith "No tasks"
  | (name, premises, lemma) :: rest ->
      (name, premises, lemma, rest, proof, theorem)

let get_formula (args : string list) (state : state) : formula =
  match args with
    [] -> failwith "Must specify premise"
  | _ ->
    let arg = String.concat " " args in
    if starts_with "x" arg then
      let (_, premises, _, _, _, _) = get_task state in
      snd (try List.find (fun (n, e) -> arg = Extract.to_string (Var n)) premises
      with Not_found -> failwith "No such premise")
    else parse_formula arg

let iff_intro (args : string list) (state : state) : state =
  noargs args;
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let (p, q) =
    match lemma with
      | Iff (e1, e2) -> (e1, e2)
      | _ -> failwith "<->i does not apply" in
  let t1 = next_task_name() in
  let task0 = (t0, premises, Imp (p, q)) in
  let task1 = (t1, premises, Imp (q, p)) in
  let tasks = task0 :: task1 :: rest in
  let proof = subst (Iff_intro (lemma, Task t0, Task t1)) t0 proof in
  (tasks, proof, theorem)

let arrow_intro (args : string list) (state : state) : state =
  noargs args;
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let (p, q) =
    match lemma with
      | Imp (e1, e2) -> (e1, e2)
      | _ -> failwith "->i does not apply" in
  let x = next_premise_name() in
  let premises = (x, p) :: premises in
  let task = (t0, premises, q) in
  let tasks = task :: rest in
  let proof = subst (Arrow_intro (lemma, Assumption (x, p), Task t0)) t0 proof in
  (tasks, proof, theorem)

let arrow_elim (args : string list) (state : state) : state =
  let x = get_formula args state in
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let t1 = next_task_name() in
  let task0 = (t0, premises, x) in
  let task1 = (t1, premises, Imp (x, lemma)) in
  let tasks = task0 :: task1 :: rest in
  let proof = subst (Arrow_elim (lemma, Task t0, Task t1)) t0 proof in
  (tasks, proof, theorem)

let and_intro (args : string list) (state : state) : state =
  noargs args;
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let (p, q) =
    match lemma with
      | And (e1, e2) -> (e1, e2)
      | _ -> failwith "&i does not apply" in
  let t1 = next_task_name() in
  let task0 = (t0, premises, p) in
  let task1 = (t1, premises, q) in
  let tasks = task0 :: task1 :: rest in
  let proof = subst (And_intro (lemma, Task t0, Task t1)) t0 proof in
  (tasks, proof, theorem)

let and_elim (f : formula -> proof -> proof) (g : formula -> formula -> formula)
             (args : string list) (state : state) : state =
  let x = get_formula args state in
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let task = (t0, premises, g lemma x) in
  let tasks = task :: rest in
  let proof = subst (f lemma (Task t0)) t0 proof in
  (tasks, proof, theorem)

let and_elim_left = and_elim (fun x y -> And_elim_left (x, y)) (fun x y -> And (x, y))
let and_elim_right = and_elim (fun x y -> And_elim_right (x, y)) (fun x y -> And (y, x))

let or_intro (f : formula -> proof -> proof) (g : formula * formula -> formula) (s : string)
             (args : string list) (state : state) : state =
  noargs args;
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let x =
    match lemma with
      | Or (e1, e2) -> (e1, e2)
      | _ -> failwith (s ^ " does not apply") in
  let task = (t0, premises, g x) in
  let tasks = task :: rest in
  let proof = subst (f lemma (Task t0)) t0 proof in
  (tasks, proof, theorem)

let or_intro_left = or_intro (fun x y -> Or_intro_left (x, y)) fst "|il"
let or_intro_right = or_intro (fun x y -> Or_intro_right (x, y)) snd "|ir"

let or_elim (args : string list) (state : state) : state =
  let x = get_formula args state in
  let (p, q) =
    match x with
      | Or (e1, e2) -> (e1, e2)
      | _ -> failwith "Premise must be a disjunction" in
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let t1 = next_task_name() in
  let t2 = next_task_name() in
  let task0 = (t0, premises, x) in
  let task1 = (t1, premises, Imp (p, lemma)) in
  let task2 = (t2, premises, Imp (q, lemma)) in
  let tasks = task0 :: task1 :: task2 :: rest in
  let proof = subst (Or_elim (lemma, Task t0, Task t1, Task t2)) t0 proof in
  (tasks, proof, theorem)

let not_intro (args : string list) (state : state) : state =
  noargs args;
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let p =
    match lemma with
      | Not e -> e
      | _ -> failwith "~i does not apply" in
  let task = (t0, premises, Imp (p, False)) in
  let tasks = task :: rest in
  let proof = subst (Not_intro (lemma, Task t0)) t0 proof in
  (tasks, proof, theorem)

let not_elim (args : string list) (state : state) : state =
  noargs args;
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let p =
    match lemma with
      | Imp (e, False) -> e
      | _ -> failwith "~e does not apply" in
  let task = (t0, premises, Not p) in
  let tasks = task :: rest in
  let proof = subst (Not_elim (lemma, Task t0)) t0 proof in
  (tasks, proof, theorem)

let exists_intro (args : string list) (state : state) : state =
  let t = get_term args in
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let (x, e) =
    match lemma with
      | Exists (x, e) -> (x, e)
      | _ -> failwith "Ei does not apply" in
  let e = subst_in_formula t x e in
  let task = (t0, premises, e) in
  let tasks = task :: rest in
  let proof = subst (Exists_intro (t, lemma, Task t0)) t0 proof in
  (tasks, proof, theorem)

let exists_elim (args : string list) (state : state) : state =
  let e = get_formula args state in
  let (x, b) =
    match e with
      | Exists (x, b) -> (x, b)
      | _ -> failwith "Premise must be an existential formula" in
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let c = fresh (e :: lemma :: List.map snd premises) in
  let b = subst_in_formula c x b in
  let t1 = next_task_name() in
  let task0 = (t0, premises, e) in
  let task1 = (t1, premises, Imp (b, lemma)) in
  let tasks = task0 :: task1 :: rest in
  let proof = subst (Exists_elim (c, lemma, Task t0, Task t1)) t0 proof in
  (tasks, proof, theorem)

let forall_intro (args : string list) (state : state) : state =
  noargs args;
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let (x, e) =
    match lemma with
      | Forall (x, e) -> (x, e)
      | _ -> failwith "Ai does not apply" in
  let c = fresh (lemma :: List.map snd premises) in
  let e = subst_in_formula c x e in
  let task = (t0, premises, e) in
  let tasks = task :: rest in
  let proof = subst (Forall_intro (c, lemma, Task t0)) t0 proof in
  (tasks, proof, theorem)

let forall_elim (args : string list) (state : state) : state =
  let (x, e) =
    match get_formula args state with
      | Forall (x, e) -> (x, e)
      | _ -> failwith "Premise must be a universal formula" in
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  (* try to find a match for x in the current task *)
  let t =
    match match_in_formula x e lemma with
      | None -> Term (x, [])
      | Some t -> t in
  let task = (t0, premises, Forall (x, e)) in
  let tasks = task :: rest in
  let proof = subst (Forall_elim (t, lemma, Task t0)) t0 proof in
  (tasks, proof, theorem)

let efq (args : string list) (state : state) : state =
  noargs args;
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let task = (t0, premises, False) in
  let tasks = task :: rest in
  let proof = subst (Efq (lemma, Task t0)) t0 proof in
  (tasks, proof, theorem)

let magic (args : string list) (state : state) : state =
  noargs args;
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let (p, q) =
    match lemma with
      | Or (e1, Not e2) -> (e1, e2)
      | Or (e1, Imp (e2, False)) -> (e1, e2)
      | Or (Not e1, e2) -> (e1, e2)
      | Or (Imp (e1, False), e2) -> (e1, e2)
      | _ -> failwith "Magic does not apply" in
  if p <> q then failwith "Magic does not apply" else
  let proof = subst (Magic lemma) t0 proof in
  (rest, proof, theorem)

let raa (args : string list) (state : state) : state =
  noargs args;
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let x = next_premise_name() in
  let premises = (x, Not lemma) :: premises in
  let task = (t0, premises, False) in
  let tasks = task :: rest in
  let proof = subst (Raa (lemma, Assumption (x, Not lemma), Task t0)) t0 proof in
  (tasks, proof, theorem)

let unit (args : string list) (state : state) : state =
  noargs args;
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  if lemma <> True then failwith "Unit does not apply" else
  let proof = subst Unit t0 proof in
  (rest, proof, theorem)

let use (args : string list) (state : state) : state =
  let x =
    match args with
    | [] -> failwith "Use what?"
    | [x] -> x
    | _ -> failwith "Too many args" in
  let (t0, premises, lemma, rest, proof, theorem) = get_task state in
  let (n, e) =
    try List.find (fun (n, e) -> x = Extract.to_string (Var n)) premises
    with Not_found -> failwith "No such premise" in
  if e <> lemma then failwith "Premise does not match" else
  let proof = subst (Assumption (n, e)) t0 proof in
  (rest, proof, theorem)

let tasks (args : string list) (state : state) : state =
  noargs args;
  let (tasks, proof, theorem) = state in
  let tasks = List.map task_to_string tasks in
  List.iter print_endline tasks; state

let undo (args : string list) (_ : state) : state =
  noargs args;
  match !undo_stack with
    ([] | [_]) -> failwith "Nothing to undo"
  | _ :: state :: rest -> undo_stack := rest; state

let latex (args : string list) (state : state) : state =
  let filename =
    match args with
      [] -> failwith "Must specify filename"
    | [x] -> x
    | _ -> failwith "Too many args" in
  let out =
    try open_out_gen [Open_wronly; Open_creat; Open_excl] 0o644 filename
    with Sys_error x ->
      if not (ends_with "File exists" x) then failwith "Could not open file for writing" else
      print_string "File exists; overwrite? ";
      if read_line() <> "yes" then failwith "Nothing written" else
      try open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o644 filename
      with Sys_error _ -> failwith "Could not open file for writing" in
  let (_, proof, _) = state in
  let header = open_in "Resources/latexheader.txt" in
  let trailer = open_in "Resources/latextrailer.txt" in
  copy_lines header out;
  close_in header;
  output_endline out (to_latex proof);
  copy_lines trailer out;
  close_in trailer;
  (try close_out out
  with Sys_error _ -> failwith "Could not write to output file");
  state

(* toggle character encoding *)
let enc (args : string list) (state : state) : state =
  noargs args;
  Ast.utf8 := not !Ast.utf8;
  state

let rec help (_ : string list) (state : state) : state =
  print_endline "Commands are:";
  List.iter (fun (x, a, _, s) -> Printf.printf "%8s  %s%s\n" (x ^ a) s a) commands;
  state

and commands =
  [("<->i", "", iff_intro, "apply iff-intro");
   ("->i", "", arrow_intro, "apply arrow-intro");
   ("->e", " P", arrow_elim, "apply arrow-elim with assumption");
   ("&i", "", and_intro, "apply and-intro");
   ("&el", " Q", and_elim_left, "apply and-elim-left with rhs");
   ("&er", " P", and_elim_right, "apply and-elim-right with lhs");
   ("|il", "", or_intro_left, "apply or-intro-left");
   ("|ir", "", or_intro_right, "apply or-intro-right");
   ("|e", " P|Q", or_elim, "apply or-elim with disjunction");
   ("~i", "", not_intro, "apply not-intro");
   ("~e", "", not_elim, "apply not-elim");
   ("Ei", " t", exists_intro, "apply exists-intro with subst term");
   ("Ee", " P", exists_elim, "apply exists-elim with premise");
   ("Ai", "", forall_intro, "apply forall-intro");
   ("Ae", " P", forall_elim, "apply forall-elim with premise");
   ("efq", "", efq, "apply ex falso quodlibet");
   ("magic", "", magic, "apply law of excluded middle");
   ("raa", "", raa, "apply reductio ad absurdum");
   ("unit", "", unit, "apply unit");
   ("use", " x", use, "use assumption");
   ("tasks", "", tasks, "show list of tasks");
   ("undo", "", undo, "undo last command");
   ("latex", " f", latex, "write latex to file");
   ("enc", "", enc, "toggle ISO-8859-1/UTF-8 character encoding");
   ("reset", "", (fun _ _ -> raise Reset), "begin proof from scratch");
   ("init", "", (fun _ _ -> raise Init), "start new theorem");
   ("quit", "", (fun _ _ -> raise Quit), "quit");
   ("help", "", help, "show list of commands")
  ]

let do_command (input : string) (state : state) : state =
  undo_stack := state :: !undo_stack;
  let s = Str.split (Str.regexp "[ \t]+") input in
  match s with
    | [] -> state
    | cmd :: args ->
        let (_, _, f, _) =
          try List.find (fun (x, _, _, _) -> cmd = x) commands
          with Not_found -> failwith ("Unrecognized command " ^ cmd)
        in f args state
