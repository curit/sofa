namespace sofa

[<AutoOpen>]
module Convenience = 
    open System
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq

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

    let resultDeserializer str = 
        JsonConvert.DeserializeObject<Response>(str)

    let defaultSerializer (id, rev) obj = 
        let model = JObject.Parse (JsonConvert.SerializeObject obj)
        match rev with 
        | Some x -> 
            let rev = JObject.Parse (JsonConvert.SerializeObject { _rev = x})
            rev.Merge(model)
            rev.ToString(Formatting.None)
        | None -> 
            model.ToString(Formatting.None)