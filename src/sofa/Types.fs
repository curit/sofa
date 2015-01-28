namespace sofa

type IdAndRev = {
    _id: string
    _rev: string
}

type Id = { 
    _id: string 
}

type Rev = { 
    _rev: string 
}

type Response = {
    id: string
    rev: string
    ok: bool
}

type Database = 
    {
        Id: string
        Url: string
    }
    member x.NormalizeUrl () =
        match x.Url.[x.Url.Length - 1] with 
        | '/' -> x.Url
        | _ -> x.Url + "/"

type SeatedSofa<'obj> = 
    {
        get: (string -> (string * string * 'obj) option Async)
        head: (string -> (string * Map<string, string list>) option Async)
        put: (string * string option -> 'obj -> (string * string) option Async)
        delete: (string * string -> (string * string) option Async)
        post: ('obj -> (string * string) option Async)
    } 