namespace sofa

open System

open FSharp.Data
open Newtonsoft.Json

type IdAndRev = {
    _id: string
    _rev: string
}

type PutResponse<'id> = {
    id: 'id
    rev: string
    ok: bool
}

[<AutoOpen>]
module Convenience = 
    let mapHeaders headers = 
        headers |> Map.map (fun k (v:string) -> 
                        v.Split([|","|], StringSplitOptions.RemoveEmptyEntries) 
                        |> Seq.map (fun s -> s.Trim ()) 
                        |> Seq.filter (not << String.IsNullOrWhiteSpace)
                        |> Seq.toList
                    )

    let defaultDeserialzer str: (string * string * 'a) =
        let model = JsonConvert.DeserializeObject<'a>(str)
        let idandrev = JsonConvert.DeserializeObject<IdAndRev>(str)
        (idandrev._id, idandrev._rev, model)

    let putResultDeserializer str = 
        JsonConvert.DeserializeObject<PutResponse<'id>>(str)

    let defaultSerializer obj = 
        JsonConvert.SerializeObject obj

type Database = 
    {
        Url: string
    }
    member x.NormalizeUrl () =
        match x.Url.[x.Url.Length - 1] with 
        | '/' -> x.Url
        | _ -> x.Url + "/"

type SeatedSofa<'id, 'obj> = 
    {
        get: ('id -> 'obj Async)
        head: ('id -> Map<string, string list> Async)
        put: ('id * string option -> 'obj -> ('id * string) option Async)
    } 

module Sofa =
    /// get a resource from the db
    let get<'a> (db:Database) (ser: string -> string * string * 'a)  http id : Async<(string * string * 'a) option> = 
        async {
            let! res = http (sprintf "%s%O" (db.NormalizeUrl ()) id) 
            match res with 
            | Some x -> 
                let body, headers = x
                return Some (body |> ser)
            | None -> return None
        }
    
    let head (db:Database) http id = 
        async {
            return! http (sprintf "%s%O" (db.NormalizeUrl ()) id)
        }

    let put<'a> (db:Database) (ser: 'a -> string) http (id, rev:string option) (model:'a) =
        async {
            let! resp = http (sprintf "%s%O" (db.NormalizeUrl()) id) (ser model)
            return 
                match resp with 
                | Some x -> 
                    let putResult, headers = x 
                    let putResult = putResult |> putResultDeserializer

                    Some (putResult.id, putResult.rev)
                | None -> None
        }
                 
    let build<'a> db = 
        let headReq url =
            async { 
                let! res = Http.AsyncRequest (url, httpMethod = "HEAD")
                return res.Headers |> mapHeaders
            }

        let getReq url = 
            async {
                let! res = Http.AsyncRequest url

                return 
                    match res.StatusCode with 
                    | 200 | 304 -> 
                        let body = 
                            match res.Body with
                            | HttpResponseBody.Text x -> x
                            | _ -> failwith "expecting a textbased response"

                        Some (body, res.Headers |> mapHeaders)
                    | 404 -> None
                    | 401 -> failwith "Read privilege required"
                    | 400 -> failwith "Bad request"
                    | _ -> failwith "Something else happend that shouldn't have"
            }

        let putReq url model = 
            async {
                let! res = Http.AsyncRequest (url, headers = ["Content-Type", "application/json"], body = TextRequest(model))
                return 
                    match res.StatusCode with
                    | 201 | 202 -> 
                        let body = 
                            match res.Body with 
                            | HttpResponseBody.Text x -> x
                            | _ -> failwith "expecting a textbased response"
                        Some (body, res.Headers |> mapHeaders)
                    | 404 -> failwith "Doc doesn't exist"
                    | 401 -> failwith "Read privilege required"
                    | 400 -> failwith "Bad request"
                    | 409 -> failwith "Document with the specified ID already exists or specified revision is not latest for target document"
                    | _ -> failwith "Something else happend that shouldn't have"
            }
        
        {
            get = get db defaultDeserialzer getReq
            head = head db headReq
            put = put db defaultSerializer putReq 
        }

    