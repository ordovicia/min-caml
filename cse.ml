open KNormal

let kn_list: ((t * Id.t) list ref) = ref []

let rec cse_impl kn = (
    match kn with
    | IfEq (id1, id2, kn1, kn2) -> (
        try
            let b1 = List.assoc kn1 !kn_list in (
            try
                IfEq (id1, id2, Var b1, Var (List.assoc kn1 !kn_list))
            with Not_found ->
                IfEq (id1, id2, Var b1, kn2))
        with Not_found -> kn)
    | IfLE (id1, id2, kn1, kn2) -> (
        try
            let b1 = List.assoc kn1 !kn_list in (
            try
                IfLE (id1, id2, Var b1, Var (List.assoc kn1 !kn_list))
            with Not_found ->
                IfLE (id1, id2, Var b1, kn2))
        with Not_found -> kn)
    | Let ((id, ty), kn1, kn2) -> (
        try
            let b1 = List.assoc kn1 !kn_list in
            Let ((id, ty), Var b1, cse_impl kn2)
        with Not_found -> (
            kn_list := (kn1, id) :: !kn_list ;
            Let ((id, ty), kn1, cse_impl kn2)))
    | LetRec (fd, kn1) -> (
        let (n, t) = fd.name in
        let a = fd.args in
        let b = fd.body in
        try
            let b1 = List.assoc b !kn_list in
            LetRec ({ name = (n, t); args = a; body = Var b1}, cse_impl kn1)
        with Not_found -> (
            kn_list := (kn1, n) :: !kn_list ;
            LetRec (fd, cse_impl kn1)))
    | _ -> kn)

let cse kn = (
    kn_list := [] ;
    cse_impl kn)
