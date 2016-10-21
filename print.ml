let indent_width = 4
let indent = ref 0

let rec padding = function
    | 0 -> ()
    | n when (n mod indent_width) = 0 -> print_string "|" ; padding (n - 1)
    | n -> print_string " " ; padding (n - 1)

let rec print_type = function
    | Type.Unit -> print_string "Unit"
    | Type.Bool -> print_string "Bool"
    | Type.Int -> print_string "Int"
    | Type.Float -> print_string "Float"
    | Type.Fun (argts, rett) -> (
        (*
        print_string "(" ;
        List.iter (fun t -> print_type t ; print_string ", ") argts ;
        print_string ") -> " ;
        *)
        print_type rett)
    | Type.Tuple ts -> (
        print_string "(" ;
        List.iter (fun t -> print_type t ; print_string ", ") ts)
    | Type.Array t -> (print_string "[" ; print_type t ; print_string "]")
    | Type.Var t -> (match !t with
        | None -> print_string "TypeVar"
        | Some t_ -> print_type t_)

let rec print_expr_impl (expr: Syntax.t) = (
    padding !indent ;
    match expr with
    | Syntax.Unit -> (
        print_endline "Unit")
    | Syntax.Bool b -> (
        let print_bool = function
            | true -> print_string "true"
            | false -> print_string "false"  in
        print_bool b ; print_endline ": Bool")
    | Syntax.Int i -> (
        print_int i ; print_endline ": Int")
    | Syntax.Float f -> (
        print_float f ; print_endline ": Float")
    | Syntax.Not e -> (
        print_endline "Not" ;
        indent := !indent + indent_width ;
        print_expr_impl e ;
        indent := !indent - indent_width)
    | Syntax.Neg e -> (
        print_endline "Neg" ;
        indent := !indent + indent_width ;
        print_expr_impl e ;
        indent := !indent - indent_width)
    | Syntax.Add (e1, e2) -> (
        print_endline "Add" ;
        indent := !indent + indent_width ;
        print_expr_impl e1 ;
        print_expr_impl e2 ;
        indent := !indent - indent_width)
    | Syntax.Sub (e1, e2) -> (
        print_endline "Sub" ;
        indent := !indent + indent_width ;
        print_expr_impl e1 ;
        print_expr_impl e2 ;
        indent := !indent - indent_width)
    | Syntax.FNeg e -> (
        print_endline "FNeg" ;
        indent := !indent + indent_width ;
        print_expr_impl e ;
        indent := !indent - indent_width)
    | Syntax.FAdd (e1, e2) -> (
        print_endline "FAdd" ;
        indent := !indent + indent_width ;
        print_expr_impl e1 ;
        print_expr_impl e2 ;
        indent := !indent - indent_width)
    | Syntax.FSub (e1, e2) -> (
        print_endline "FSub" ;
        indent := !indent + indent_width ;
        print_expr_impl e1 ;
        print_expr_impl e2 ;
        indent := !indent - indent_width)
    | Syntax.FMul (e1, e2) -> (
        print_endline "FMul" ;
        indent := !indent + indent_width ;
        print_expr_impl e1 ;
        print_expr_impl e2 ;
        indent := !indent - indent_width)
    | Syntax.FDiv (e1, e2) -> (
        print_endline "FDiv" ;
        indent := !indent + indent_width ;
        print_expr_impl e1 ;
        print_expr_impl e2 ;
        indent := !indent - indent_width)
    | Syntax.Eq (e1, e2) -> (
        print_endline "Eq" ;
        indent := !indent + indent_width ;
        print_expr_impl e1 ;
        print_expr_impl e2 ;
        indent := !indent - indent_width)
    | Syntax.LE (e1, e2) -> (
        print_endline "LE" ;
        indent := !indent + indent_width ;
        print_expr_impl e1 ;
        print_expr_impl e2 ;
        indent := !indent - indent_width)
    | Syntax.If (cond, then_, else_) -> (
        print_endline "If" ;
        indent := !indent + indent_width ;
        print_expr_impl cond ;
        print_expr_impl then_ ;
        print_expr_impl else_ ;
        indent := !indent - indent_width)
    | Syntax.Let ((id, t), body, e) -> (
        print_endline "Let" ;
        indent := !indent + indent_width ;
        padding !indent ; print_string (id ^ ": ") ;
        print_type t ; print_newline () ;
        print_expr_impl body ;
        print_expr_impl e ;
        indent := !indent - indent_width)
    | Syntax.Var t -> (
        print_string t ; print_endline ": Var")
    | Syntax.LetRec (fd, e) ->
        let (n, t) = fd.Syntax.name in (
        print_endline "LetRec" ;
        indent := !indent + indent_width ;
        padding !indent ; print_endline n ;
        List.iter (fun (n, t) -> (
            padding !indent ;
            print_string (n ^ ": ") ;
            print_type t ; print_newline ())) fd.Syntax.args ;
        padding !indent ; print_type t ; print_newline () ;
        print_expr_impl fd.Syntax.body ;
        print_expr_impl e ;
        indent := !indent - indent_width)
    | Syntax.App (f, args) -> (
        print_endline "App" ;
        indent := !indent + indent_width ;
        print_expr_impl f ;
        List.iter (fun a -> print_expr_impl a) args ;
        indent := !indent - indent_width)
    | Syntax.Tuple ts -> (
        print_endline "Tuple" ;
        indent := !indent + indent_width ;
        List.iter (fun t -> print_expr_impl t) ts ;
        indent := !indent - indent_width)
    | Syntax.LetTuple (pats, body, e) -> (
        print_endline "LetTuple" ;
        indent := !indent + indent_width ;
        List.iter (fun (n, t) -> (
            padding !indent ;
            print_string (n ^ ": ") ;
            print_type t ; print_newline ())) pats ;
        print_expr_impl body ;
        print_expr_impl e ;
        indent := !indent - indent_width)
    | Syntax.Array (n, t) -> (
        print_endline "Array" ;
        indent := !indent + indent_width ;
        print_expr_impl n ;
        print_expr_impl t ;
        indent := !indent - indent_width)
    | Syntax.Get (e1, e2) -> (
        print_endline "Get" ;
        indent := !indent + indent_width ;
        print_expr_impl e1 ;
        print_expr_impl e2 ;
        indent := !indent - indent_width)
    | Syntax.Put (e1, e2, e3) -> (
        print_endline "Put" ;
        indent := !indent + indent_width ;
        print_expr_impl e1 ;
        print_expr_impl e2 ;
        print_expr_impl e3 ;
        indent := !indent - indent_width))

let rec print_expr (expr: Syntax.t) = (
    indent := 0 ;
    print_endline "[ Syntax.t ] ==============================" ;
    print_expr_impl expr ;
    print_newline ())

let rec print_knormal_impl (kn: KNormal.t) = (
    padding !indent ;
    match kn with
    | KNormal.Unit -> (
        print_endline "Unit")
    | KNormal.Int i -> (
        print_int i ; print_endline ": Int")
    | KNormal.Float f -> (
        print_float f ; print_endline ": Float")
    | KNormal.Neg id -> (
        print_endline "Neg" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id ;
        indent := !indent - indent_width)
    | KNormal.Add (id1, id2) -> (
        print_endline "Add" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id1 ;
        padding !indent; print_endline id2 ;
        indent := !indent - indent_width)
    | KNormal.Sub (id1, id2) -> (
        print_endline "Sub" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id1 ;
        padding !indent; print_endline id2 ;
        indent := !indent - indent_width)
    | KNormal.FNeg id -> (
        print_endline "FNeg" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id ;
        indent := !indent - indent_width)
    | KNormal.FAdd (id1, id2) -> (
        print_endline "FAdd" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id1 ;
        padding !indent; print_endline id2 ;
        indent := !indent - indent_width)
    | KNormal.FSub (id1, id2) -> (
        print_endline "FSub" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id1 ;
        padding !indent; print_endline id2 ;
        indent := !indent - indent_width)
    | KNormal.FMul (id1, id2) -> (
        print_endline "FMul" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id1 ;
        padding !indent; print_endline id2 ;
        indent := !indent - indent_width)
    | KNormal.FDiv (id1, id2) -> (
        print_endline "FDiv" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id1 ;
        padding !indent; print_endline id2 ;
        indent := !indent - indent_width)
    | KNormal.IfEq (id1, id2, kn1, kn2) -> (
        print_endline "IfEq" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id1 ;
        padding !indent; print_endline id2 ;
        print_knormal_impl kn1 ;
        print_knormal_impl kn2 ;
        indent := !indent - indent_width)
    | KNormal.IfLE (id1, id2, kn1, kn2) -> (
        print_endline "IfLE" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id1 ;
        padding !indent; print_endline id2 ;
        print_knormal_impl kn1 ;
        print_knormal_impl kn2 ;
        indent := !indent - indent_width)
    | KNormal.Let ((id, t), kn1, kn2) -> (
        print_endline "Let" ;
        indent := !indent + indent_width ;
        padding !indent ; print_string (id ^ ": ") ;
        print_type t ; print_newline () ;
        print_knormal_impl kn1 ;
        print_knormal_impl kn2 ;
        indent := !indent - indent_width)
    | KNormal.Var id -> (
        print_endline id)
    | KNormal.LetRec (fd, kn) ->
        let (n, t) = fd.KNormal.name in (
        print_endline "LetRec" ;
        indent := !indent + indent_width ;
        padding !indent ; print_endline n ;
        List.iter (fun (n, t) -> (
            padding !indent ;
            print_string (n ^ ": ") ;
            print_type t ; print_newline ())) fd.KNormal.args ;
        padding !indent ; print_type t ; print_newline () ;
        print_knormal_impl fd.KNormal.body ;
        print_knormal_impl kn ;
        indent := !indent - indent_width)
    | KNormal.App (id, args) -> (
        print_endline "App" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id ;
        List.iter (fun a -> padding !indent; print_endline a) args ;
        indent := !indent - indent_width)
    | KNormal.Tuple ids -> (
        print_endline "Tuple" ;
        indent := !indent + indent_width ;
        List.iter (fun id -> padding !indent; print_endline id) ids ;
        indent := !indent - indent_width)
    | KNormal.LetTuple (pats, body, id) -> (
        print_endline "LetTuple" ;
        indent := !indent + indent_width ;
        List.iter (fun (n, t) -> (
            padding !indent ;
            print_string (n ^ ": ") ;
            print_type t ; print_newline ())) pats ;
        padding !indent; print_endline body ;
        print_knormal_impl id ;
        indent := !indent - indent_width)
    | KNormal.Get (id1, id2) -> (
        print_endline "Get" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id1 ;
        padding !indent; print_endline id2 ;
        indent := !indent - indent_width)
    | KNormal.Put (id1, id2, id3) -> (
        print_endline "Put" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id1 ;
        padding !indent; print_endline id2 ;
        padding !indent; print_endline id3 ;
        indent := !indent - indent_width)
    | KNormal.ExtArray id -> (
        print_endline "ExtArray" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id ;
        indent := !indent - indent_width)
    | KNormal.ExtFunApp (id, ids) -> (
        print_endline "ExtFunApp" ;
        indent := !indent + indent_width ;
        padding !indent; print_endline id ;
        List.iter (fun id -> (
            padding !indent ; print_endline id)) ids ;
        indent := !indent - indent_width))

let rec print_knormal (kn: KNormal.t) = (
    indent := 0 ;
    print_endline "[ KNormal.t ] =============================" ;
    print_knormal_impl kn ;
    print_newline ())

