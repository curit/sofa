namespace sofa.Tests
open sofa
open Xunit
open FsUnit.Xunit
open Newtonsoft.Json

type Integration () =

    do 
        let server = Server.build "http://localhost:5984/"
        server.all ()
            |> Async.RunSynchronously 
            |> Seq.filter (fun s -> not (s.Id.StartsWith("_")))
            |> Seq.iter (fun s -> server.delete s.Id |> Async.RunSynchronously |> ignore)

    [<Fact>]
    let ``should be able to create a database`` () =
        async {
            // Given
            let server = Server.build "http://localhost:5984/"
    
            // When 
            let! db = server.put "test-1"
            let! yes = server.head "test-1"
        
            // Then
            yes |> should be True
            db |> should equal (Some { Id = "test-1"; Url = "http://localhost:5984/test-1/" })
        } |> Async.RunSynchronously


    [<Fact>]
    let ``should be able to delete a database`` () =
        async {
            // Given
            let server = Server.build "http://localhost:5984/"
    
            // When 
            let! db = server.put "test-1"
            let! yes = server.head "test-1"
            let! deleted = server.delete "test-1"
            let! result = server.head "test-1"

            // Then
            yes |> should be True
            db |> should equal (Some { Id = "test-1"; Url = "http://localhost:5984/test-1/" })
            deleted |> should be True
            result |> should be False
        } |> Async.RunSynchronously

    [<Fact>]
    let ``should be able to get all databases`` () = 
        async {
            // Given
            let server = Server.build "http://localhost:5984"

            // When 
            let! dbs = [ "one"; "two"; "three" ] |> Seq.map server.put |> Async.Parallel
            let! all = server.all () 
            let all = 
                all 
                |> Seq.filter (fun s -> s.Id.StartsWith("_") |> not)
                |> Seq.map (fun s -> s.Id)
            
            // Then
            dbs |> Array.length |> should equal 3
            all |> should contain "one"
            all |> should contain "two"
            all |> should contain "three"
        } |> Async.RunSynchronously