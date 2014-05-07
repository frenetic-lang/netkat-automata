val lspines : Decide_Ast.term -> Decide_Ast.TermSet.t
val rspines : Decide_Ast.term -> Decide_Ast.TermSet.t
val lrspines : Decide_Ast.term -> Decide_Ast.TermSet.t 
val allLRspines : Decide_Ast.term -> (Decide_Ast.term, Decide_Ast.TermSet.t) Hashtbl.t
