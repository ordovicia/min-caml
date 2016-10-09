let indent = ref 0
let rec padding = function
    | 0 -> ()
    | n when (n mod 4) = 0 -> print_string "|" ; padding (n - 1)
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

let rec print_expr expr = (
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
        indent := !indent + 4 ;
        print_expr e ;
        indent := !indent - 4)
    | Syntax.Neg e -> (
        print_endline "Neg" ;
        indent := !indent + 4 ;
        print_expr e ;
        indent := !indent - 4)
    | Syntax.Add (e1, e2) -> (
        print_endline "Add" ;
        indent := !indent + 4 ;
        print_expr e1 ;
        print_expr e2 ;
        indent := !indent - 4)
    | Syntax.Sub (e1, e2) -> (
        print_endline "Sub" ;
        indent := !indent + 4 ;
        print_expr e1 ;
        print_expr e2 ;
        indent := !indent - 4)
    | Syntax.FNeg e -> (
        print_endline "FNeg" ;
        indent := !indent + 4 ;
        print_expr e ;
        indent := !indent - 4)
    | Syntax.FAdd (e1, e2) -> (
        print_endline "FAdd" ;
        indent := !indent + 4 ;
        print_expr e1 ;
        print_expr e2 ;
        indent := !indent - 4)
    | Syntax.FSub (e1, e2) -> (
        print_endline "FSub" ;
        indent := !indent + 4 ;
        print_expr e1 ;
        print_expr e2 ;
        indent := !indent - 4)
    | Syntax.FMul (e1, e2) -> (
        print_endline "FMul" ;
        indent := !indent + 4 ;
        print_expr e1 ;
        print_expr e2 ;
        indent := !indent - 4)
    | Syntax.FDiv (e1, e2) -> (
        print_endline "FDiv" ;
        indent := !indent + 4 ;
        print_expr e1 ;
        print_expr e2 ;
        indent := !indent - 4)
    | Syntax.Eq (e1, e2) -> (
        print_endline "Eq" ;
        indent := !indent + 4 ;
        print_expr e1 ;
        print_expr e2 ;
        indent := !indent - 4)
    | Syntax.LE (e1, e2) -> (
        print_endline "LE" ;
        indent := !indent + 4 ;
        print_expr e1 ;
        print_expr e2 ;
        indent := !indent - 4)
    | Syntax.If (cond, then_, else_) -> (
        print_endline "If" ;
        indent := !indent + 4 ;
        print_expr cond ;
        print_expr then_ ;
        print_expr else_ ;
        indent := !indent - 4)
    | Syntax.Let ((id, t), body, e) -> (
        print_endline "Let" ;
        indent := !indent + 4 ;
        padding !indent ; print_string (id ^ ": ") ;
        print_type t ; print_newline () ;
        print_expr body ;
        print_expr e ;
        indent := !indent - 4)
    | Syntax.Var t -> (
        print_string t ; print_endline ": Var")
    | Syntax.LetRec (fd, e) ->
        let (n, t) = fd.Syntax.name in (
        print_endline "LetRec" ;
        indent := !indent + 4 ;
        padding !indent ; print_endline n ;
        List.iter (fun (n, t) -> (
            padding !indent ;
            print_string (n ^ ": ") ;
            print_type t ; print_newline ())) fd.Syntax.args ;
        padding !indent ; print_type t ; print_newline () ;
        print_expr fd.Syntax.body ;
        print_expr e ;
        indent := !indent - 4)
    | Syntax.App (f, args) -> (
        print_endline "App" ;
        indent := !indent + 4 ;
        print_expr f ;
        List.iter (fun a -> print_expr a) args ;
        indent := !indent - 4)
    | Syntax.Tuple ts -> (
        print_endline "Tuple" ;
        indent := !indent + 4 ;
        List.iter (fun t -> print_expr t) ts ;
        indent := !indent - 4)
    | Syntax.LetTuple (pats, body, e) -> (
        print_endline "LetTuple" ;
        indent := !indent + 4 ;
        List.iter (fun (n, t) -> (
            padding !indent ;
            print_string (n ^ ": ") ;
            print_type t ; print_newline ())) pats ;
        print_expr body ;
        print_expr e ;
        indent := !indent - 4)
    | Syntax.Array (n, t) -> (
        print_endline "Array" ;
        indent := !indent + 4 ;
        print_expr n ;
        print_expr t ;
        indent := !indent - 4)
    | Syntax.Get (e1, e2) -> (
        print_endline "Get" ;
        indent := !indent + 4 ;
        print_expr e1 ;
        print_expr e2 ;
        indent := !indent - 4)
    | Syntax.Put (e1, e2, e3) -> (
        print_endline "Put" ;
        indent := !indent + 4 ;
        print_expr e1 ;
        print_expr e2 ;
        print_expr e3 ;
        indent := !indent - 4))
