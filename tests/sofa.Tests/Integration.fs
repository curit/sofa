namespace sofa.Tests
open sofa
open Xunit
open FsUnit.Xunit

type Test = 
    {
        value: string
    }

type AnotherTest = 
    {
        value: int
    }

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
    let ``should be able to put and get a doc to a database`` () =
        async {
            // Given
            let server = Server.build "http://localhost:5984"
            let! db = server.put "test-2"
            let seatedsofa = Sofa.build<Test> db.Value

            // When
            let! result = seatedsofa.put ("SpagettiWithMeatballs", None) { value = "test" }
            let id, rev = result.Value

            let! head = seatedsofa.head "SpagettiWithMeatballs"
            let headId, headers = head.Value

            // Then 
            id |> should equal "SpagettiWithMeatballs"
            rev |> should not' (be NullOrEmptyString)
            headId |> should equal rev
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

    [<Fact>]
    let ``should return none for a non existant document`` ()=
        async {
            // Given
            let server = Server.build "http://localhost:5984"
            let! db = server.put "test-3"
            let seatedsofa = Sofa.build db.Value
            
            // When
            let! head = seatedsofa.head "DoesntExists"
            
            // Then
            head |> should equal None
        } |> Async.RunSynchronously

    [<Fact>]
    let ``should be able to put a new design-doc with a view`` () = 
        async {
            // Given
            let server = Server.build "http://localhost:5984"
            let! db = server.put "design-doc-test"
            let seatedsofa = Sofa.build db.Value
 
            // When           
            let! result = seatedsofa._design.put ("_design/test", None) { views = [("test-view", { map = "function (doc) { if (doc.value % 2 === 0) { emit(doc.value, 1); } }" } )] |> Map.ofList }
            let id, rev = result.Value

            // Then
            id |> should equal "_design/test"
            rev |> should not' (be NullOrEmptyString)
        } |> Async.RunSynchronously