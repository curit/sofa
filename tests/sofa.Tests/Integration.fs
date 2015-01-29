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
        intvalue: int
    }

type QueryResult = 
    {
        key: int
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

    
    [<Fact>]
    let ``should be able get the result of a view`` () = 
        async {
            // Given
            let server = Server.build "http://localhost:5984"
            let! db = server.put "design-doc-test-2"
            let seatedsofa = Sofa.build<AnotherTest> db.Value
            do! seatedsofa._design.put ("_design/test", None) { views = [("test-view", { map = "function (doc) { if (doc.intvalue % 2 === 0) { emit(doc.intvalue, 1); } }" } )] |> Map.ofList } |> Async.Ignore
            
            do! [ { intvalue = 1 }; { intvalue = 2 }; { intvalue = 3 }; { intvalue = 4 }; { intvalue = 5 }; { intvalue = 6 } ] 
                |> Seq.map (fun v -> seatedsofa.post v)
                |> Async.Parallel
                |> Async.Ignore
            
            let query = Sofa.buildQuery<QueryResult, AnotherTest, int> db.Value "_design/test" "test-view"
            
            // When
            let! result = query.all None
            let offset, total_rows, res = result.Value

            // Then
            total_rows |> should equal 3
            offset |> should equal 0
            res |> Seq.iter (fun t -> t.key % 2 |> should equal 0)

        } |> Async.RunSynchronously

    [<Fact>]
    let ``should be able get the result of a view including the docs`` () = 
        async {
            // Given
            let server = Server.build "http://localhost:5984"
            let! db = server.put "design-doc-test-2"
            let seatedsofa = Sofa.build<AnotherTest> db.Value
            do! seatedsofa._design.put ("_design/test", None) { views = [("test-view", { map = "function (doc) { if (doc.intvalue % 2 === 0) { emit(doc.intvalue, 1); } }" } )] |> Map.ofList } |> Async.Ignore
            
            do! [ { intvalue = 1 }; { intvalue = 2 }; { intvalue = 3 }; { intvalue = 4 }; { intvalue = 5 }; { intvalue = 6 } ] 
                |> Seq.map (fun v -> seatedsofa.post v)
                |> Async.Parallel
                |> Async.Ignore
            
            let query = Sofa.buildQuery<QueryResult, AnotherTest, int> db.Value "_design/test" "test-view"
            
            // When
            let! result = query.allIncludeDocs None
            let offset, total_rows, res = result.Value

            // Then
            total_rows |> should equal 3
            offset |> should equal 0
            res |> Seq.iter (
                fun (kv, doc) -> 
                    kv.key % 2 |> should equal 0
                    kv.key |> should equal doc.intvalue
            )

        } |> Async.RunSynchronously

    [<Fact>]
    let ``should be able get the result of a view including the docs skip one limit two`` () = 
        async {
            // Given
            let server = Server.build "http://localhost:5984"
            let! db = server.put "design-doc-test-2"
            let seatedsofa = Sofa.build<AnotherTest> db.Value
            do! seatedsofa._design.put ("_design/test", None) { views = [("test-view", { map = "function (doc) { if (doc.intvalue % 2 === 0) { emit(doc.intvalue, 1); } }" } )] |> Map.ofList } |> Async.Ignore
            
            do! [ { intvalue = 1 }; { intvalue = 2 }; { intvalue = 3 }; { intvalue = 4 }; { intvalue = 5 }; { intvalue = 6 } ] 
                |> Seq.map (fun v -> seatedsofa.post v)
                |> Async.Parallel
                |> Async.Ignore
            
            let query = Sofa.buildQuery<QueryResult, AnotherTest, int> db.Value "_design/test" "test-view"
            
            // When
            let! result = query.allIncludeDocs (Some (1, 2))
            let offset, total_rows, res = result.Value

            // Then
            total_rows |> should equal 3
            offset |> should equal 1
            res |> Seq.length |> should equal 2
            res |> Seq.iter (
                fun (kv, doc) -> 
                    kv.key |> should not' (equal 2)
                    kv.key % 2 |> should equal 0
                    kv.key |> should equal doc.intvalue
            )

        } |> Async.RunSynchronously

    [<Fact>]
    let ``should be able get the result of a view by key`` () = 
        async {
            // Given
            let server = Server.build "http://localhost:5984"
            let! db = server.put "design-doc-test-2"
            let seatedsofa = Sofa.build<AnotherTest> db.Value
            do! seatedsofa._design.put ("_design/test", None) { views = [("test-view", { map = "function (doc) { if (doc.intvalue % 2 === 0) { emit(doc.intvalue, 1); } }" } )] |> Map.ofList } |> Async.Ignore
            
            do! [ { intvalue = 1 }; { intvalue = 2 }; { intvalue = 3 }; { intvalue = 4 }; { intvalue = 5 }; { intvalue = 6 } ] 
                |> Seq.map (fun v -> seatedsofa.post v)
                |> Async.Parallel
                |> Async.Ignore
            
            let query = Sofa.buildQuery<QueryResult, AnotherTest, int> db.Value "_design/test" "test-view"
            
            // When
            let! result = query.keys [4] None
            let offset, total_rows, res = result.Value

            // Then
            total_rows |> should equal 3
            offset |> should equal 1
            res |> Seq.length |> should equal 1
            (res |> Seq.head).key |> should equal 4

        } |> Async.RunSynchronously

    [<Fact>]
    let ``should be able get the result of a view by mutliple keys`` () = 
        async {
            // Given
            let server = Server.build "http://localhost:5984"
            let! db = server.put "design-doc-test-2"
            let seatedsofa = Sofa.build<AnotherTest> db.Value
            do! seatedsofa._design.put ("_design/test", None) { views = [("test-view", { map = "function (doc) { if (doc.intvalue % 2 === 0) { emit(doc.intvalue, 1); } }" } )] |> Map.ofList } |> Async.Ignore
            
            do! [ { intvalue = 1 }; { intvalue = 2 }; { intvalue = 3 }; { intvalue = 4 }; { intvalue = 5 }; { intvalue = 6 } ] 
                |> Seq.map (fun v -> seatedsofa.post v)
                |> Async.Parallel
                |> Async.Ignore
            
            let query = Sofa.buildQuery<QueryResult, AnotherTest, int> db.Value "_design/test" "test-view"
            
            // When
            let! result = query.keys [4;6] None
            let offset, total_rows, res = result.Value

            // Then
            total_rows |> should equal 3
            offset |> should equal 1
            res |> Seq.length |> should equal 2
            (res |> Seq.head).key |> should equal 4
            (res |> Seq.last).key |> should equal 6

        } |> Async.RunSynchronously