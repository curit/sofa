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

type View = {
    map: string
}

type DesignDoc = {
    views: Map<string, View>
}

type QueryResponse<'a> = {
    offset: int
    rows: 'a seq
    total_rows: int
}

type IncludedDoc<'a> = {
    doc: 'a
}

type QueryResponseIncludedDocs<'a> = {
    rows: IncludedDoc<'a> seq
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

type SofaView<'a, 'doc, 'key> = {
    all: (int * int) option -> (int * int * 'a seq) option Async
    allIncludeDocs: (int * int) option -> (int * int * ('a * 'doc) seq) option Async  
    keys: 'key list -> (int * int) option -> (int * int * 'a seq) option Async
    keysIncludeDocs: 'key list -> (int * int) option -> (int * int * ('a * 'doc) seq) option Async
}

type C683 = 
    {
        get: (string -> (string * string * DesignDoc) option Async)
        head: (string -> (string * Map<string, string list>) option Async)
        put: (string * string option -> DesignDoc -> (string * string) option Async)
        delete: (string * string -> (string * string) option Async)
    } 

type SeatedSofa<'obj> = 
    {
        get: (string -> (string * string * 'obj) option Async)
        head: (string -> (string * Map<string, string list>) option Async)
        put: (string * string option -> 'obj -> (string * string) option Async)
        delete: (string * string -> (string * string) option Async)
        post: ('obj -> (string * string) option Async)
        _design: C683
    } 

