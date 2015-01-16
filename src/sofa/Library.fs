namespace sofa

open System

open FSharp.Data

[<AutoOpen>]
module Convenience = 
    let mapHeaders headers = 
        headers |> Map.map (fun k (v:string) -> 
                        v.Split([|","|], StringSplitOptions.RemoveEmptyEntries) 
                        |> Seq.map (fun s -> s.Trim ()) 
                        |> Seq.filter (not << String.IsNullOrWhiteSpace)
                        |> Seq.toList
                    )

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
    let get (db:Database) ser http id = 
        async {
            let! res = http (sprintf "%s%O" (db.NormalizeUrl ()) id) 
            return res |> ser
        }
    
    let head (db:Database) http id = 
        async {
            return! http (sprintf "%s%O" (db.NormalizeUrl ()) id)
        }
                 
    let build db ser = 
        let headReq url =
            async { 
                let! res = Http.AsyncRequest (url, httpMethod = "HEAD")
                return res.Headers |> mapHeaders
            }

        {
            get = get db ser Http.AsyncRequestString
            head = head db headReq
        }

    