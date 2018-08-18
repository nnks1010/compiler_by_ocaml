(* syntax tree *)
type id  = string
type op  = Plus | Minus | Times | Div

type stm = Stmts   of stm * stm
         | Assign  of id  * exp
         | Print   of exp
and exp  = Id      of id
         | Num     of int
         | StmtExp of stm * exp
         | Plus    of exp * exp
         | Minus   of exp * exp
         | Times   of exp * exp
         | Div     of exp * exp

(* environment *)
exception No_such_symbol
let e0 = fun _ -> raise No_such_symbol
let update var vl env = fun v -> if v = var then vl else env v

(* semantic analysis *)
let rec trans_stmt ast env =
    match ast with
        Stmts (s1, s2)  -> let env'     = trans_stmt s1 env in
                                            trans_stmt s2 env'
      | Assign (var, e) -> let vl, env' = trans_exp e env in
                                            update var vl env'
      | Print e         -> let vl, env' = trans_exp e env in
                                            (print_int vl; print_string "\n"; env')
and trans_exp ast env =
    match ast with
        Id  v -> (env v, env)
      | Num n -> (n, env)
      | StmtExp (s, e) -> let env' = trans_stmt s env in
                                       trans_exp e env'
      | Plus (e1, e2)  -> let vl1, env1 = trans_exp e1 env in
                            let vl2, env2 = trans_exp e2 env1 in
                              (vl1 + vl2, env2)
      | Minus (e1, e2) -> let vl1, env1 = trans_exp e1 env in
                            let vl2, env2 = trans_exp e2 env1 in
                              (vl1 - vl2, env2)
      | Times (e1, e2) -> let vl1, env1 = trans_exp e1 env in
                            let vl2, env2 = trans_exp e2 env1 in
                              (vl1 * vl2, env2)
      | Div (e1, e2)   -> let vl1, env1 = trans_exp e1 env1 in
                            let vl2, env2 = trans_exp e2 env2 in
                              (vl1 / vl2, env2)

(* x = 1 + 2 * 3; y = x / 4; print (y) //=> 1 *)
let prog = Stmts (Assign ("x", Plus (Num 1, Times (Num 2, Num 3))),
             Stmts (Assign ("y", Div (Id "x", Num 4)), Print (Id "y"))) ;;

(* y = (x = 1 + 2, x); print (y) //=> 3 *)
let prog1 = Stmts (Assign("y",
              StmtExp (Assign ("x", Plus (Num 1, Num 2)), Id "x")), Print (Id "y")) ;;

(* y = (x = 1 + 2, x); print (x) //=> 3 *)
let prog2 = Stmts (Assign("y",
              StmtExp (Assign ("x", Plus (Num 1, Num 2)), Id "x")), Print (Id "x")) ;;

let interp ast = trans_stmt ast e0

