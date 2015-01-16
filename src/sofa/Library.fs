namespace sofa

open System

open FSharp.Data
open Newtonsoft.Json

type IdAndRev = {
    _id: string
    _rev: string
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

    let defaultSerialzer str: (string * string * 'a) =
        let model = JsonConvert.DeserializeObject<'a>(str)
        let idandrev = JsonConvert.DeserializeObject<IdAndRev>(str)
        (idandrev._id, idandrev._rev, model)

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
        
        {
            get = get db defaultSerialzer getReq
            head = head db headReq
        }

    