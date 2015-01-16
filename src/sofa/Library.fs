namespace sofa

type Database = 
    {
        Url: string
    }
    member x.NormalizeUrl () =
        { x with 
            Url = 
                match x.Url.[x.Url.Length - 1] with 
                | '/' -> x.Url
                | _ -> x.Url + "/"
        }

module Sofa =

    /// Returns 42
    ///
    /// ## Parameters
    ///  - `num` - whatever
    let get id db ser http =
        http (sprintf "%s%O" db.Url id) |> ser
             
    