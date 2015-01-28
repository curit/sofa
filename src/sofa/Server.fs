namespace sofa

open FSharp.Data
open Newtonsoft.Json

type Server = 
    {
        all: unit -> Database seq Async
        get: string -> Database option Async
        head: string -> bool Async
        put: string -> Database option Async
        delete: string -> bool Async
    }
    static member build (url:string) =
        let url = 
            match url.[url.Length - 1] with 
            | '/' -> url
            | _ -> url + "/"
        
        {
            all = fun () -> 
                async { 
                    let! res = Http.AsyncRequest(url + "_all_dbs", silentHttpErrors = true)
                    return 
                        match res.Body with
                        | HttpResponseBody.Text x -> 
                            JsonConvert.DeserializeObject<seq<string>>(x) 
                            |> Seq.map (fun s -> { Id = s; Url = url + s + "/" })
                        | _ -> Seq.empty
                }
            get = fun s -> 
                async {
                    let! res = Http.AsyncRequest(url + s, silentHttpErrors = true)
                    return
                        match res.StatusCode with
                        | 200 -> Some { Id = s; Url = url + s + "/" }
                        | _ -> None
                }
            head = fun s -> 
               async {
                    let! res = Http.AsyncRequest(url + s, httpMethod = "HEAD", silentHttpErrors = true)
                    return
                        match res.StatusCode with
                        | 200 -> true
                        | _ -> false
                }
            put = fun s -> 
                async {
                    let! res = Http.AsyncRequest(url + s, httpMethod = "PUT", silentHttpErrors = true)
                    return 
                        match res.StatusCode with 
                        | 201 | 412 -> Some { Id = s; Url = url + s + "/" }
                        | 401 -> failwith "CouchDB Server Administrator privileges required"
                        | 400 -> failwith "Invalid database name"
                        | _ -> failwith "Something else happend that shouldn't have"
                }
            delete = fun s -> 
                async {
                    let! res = Http.AsyncRequest(url + s, httpMethod = "DELETE", silentHttpErrors = true)
                    return 
                        match res.StatusCode with 
                        | 200 | 404 -> true
                        | 401 -> failwith "CouchDB Server Administrator privileges required"
                        | 400 -> failwith "Invalid database name"
                        | _ -> failwith "Something else happend that shouldn't have"
                }
        }