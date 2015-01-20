module Tests

open sofa
open Xunit
open FsUnit.Xunit
open Newtonsoft.Json

type Future<'a> () =
    let mutable _value: 'a option = None

    member x.Resolve value =
        match _value with
        | Some _ -> false
        | None -> 
            _value <- Some value
            true
            
    member x.Value with get () = _value

let testHttpGet ret = 
    let future = Future ()

    let returnResult str = 
        async {
            future.Resolve str |> ignore
            return Some (ret, Map.empty) 
        }

    (returnResult, future)

let testHttpHead ret = 
    let returnResult str = 
        async {
            return
                [
                    ("E-Tag", ["1-" + ret])
                ] |> Map.ofList |> Some
        }

    returnResult

let testHttpDelete ret =
    let urlFuture = Future ()

    let returnResult url rev =
        async {
            urlFuture.Resolve (url + "?rev=" + rev) |> ignore
            return Some (ret, Map.empty)
        }

    (returnResult, urlFuture)

let testHttpPut<'a> ret =
    let urlFuture = Future ()
    let modelFuture = Future<'a> ()

    let returnResult url model =
        async {
            urlFuture.Resolve url |> ignore
            modelFuture.Resolve model |> ignore
            return Some (ret, Map.empty) 
        }

    (returnResult, urlFuture, modelFuture)

let testHttpPost ret =
    let urlFuture = Future ()
    let modelFuture = Future<'a> ()

    let returnResult url model = 
        async {
            modelFuture.Resolve model |> ignore
            urlFuture.Resolve url |> ignore
            return Some(ret, Map.empty)
        }

    (returnResult, urlFuture, modelFuture)

type TestData = 
    {
        value: string
    }

[<Fact>]
let ``basic head test`` () =
    async {
        // Given
        let db:Database = { Id = "bla"; Url = "test://bla" }
        let http = testHttpHead "5"

        // When
        let! res = Sofa.head db http 5 
        let rev, headers = res.Value

        // Then
        rev |> should equal "1-5"
        (headers |> Map.toList).[0] |> should equal ("E-Tag", ["1-5"])
    } |> Async.RunSynchronously

[<Fact>]
let ``basic get test`` () =
    async {
        // Given
        let db:Database = { Id = "bla"; Url = "test://bla" }
        let http, urlFuture = testHttpGet "{ \"value\": \"blaat\", \"_id\": 5, \"_rev\": \"5-1\" }" 

        // When
        let! res = Sofa.get<TestData> db defaultDeserialzer http "5"
        let id, rev, body = res.Value

        // Then
        id |> should equal "5"
        rev |> should equal"5-1"
        body.value |> should equal "blaat"
        urlFuture.Value |> should equal (Some "test://bla/5")
    } |> Async.RunSynchronously

[<Fact>]
let ``basic put test`` () =
    async {
        // Given
        let db = { Id = "peer"; Url= "test://peer" }
        let http, urlFuture, modelFuture = testHttpPut "{ \"id\": \"5\", \"rev\": \"1-5\", \"ok\": true }"

        // When
        let! res = Sofa.put db defaultSerializer http ("5", None) { value = "blaat"}
        let id, rev = res.Value
        
        // Then
        id |> should equal "5"
        rev |> should equal "1-5"
        urlFuture.Value |> should equal (Some "test://peer/5")
        modelFuture.Value |> should equal (Some "{\"value\":\"blaat\"}")
    } |> Async.RunSynchronously

[<Fact>]
let ``basic post test`` () =
    async {
        // Given
        let db = { Id = "peer"; Url= "test://peer" }
        let http, urlFuture, modelFuture = testHttpPost "{ \"id\": \"5\", \"rev\": \"1-5\", \"ok\": true }"

        // When
        let! res = Sofa.post db JsonConvert.SerializeObject http { value = "blaat"}
        let id, rev = res.Value
        
        // Then
        id |> should equal "5"
        rev |> should equal "1-5"
        urlFuture.Value |> should equal (Some "test://peer/")
        modelFuture.Value |> should equal (Some "{\"value\":\"blaat\"}")
    } |> Async.RunSynchronously
    
[<Fact>]
let ``basic put test with id and rev`` () =
    async {
        // Given
        let db = { Id = "peer"; Url= "test://peer" }
        let http, urlFuture, modelFuture = testHttpPut "{ \"id\": \"5\", \"rev\": \"1-5\", \"ok\": true }"

        // When
        let! res = Sofa.put db defaultSerializer http ("5", Some "2-5") { value = "blaat"}
        let id, rev = res.Value
        
        // Then
        id |> should equal "5"
        rev |> should equal "1-5"
        urlFuture.Value |> should equal (Some "test://peer/5")
        modelFuture.Value |> should equal (Some "{\"_rev\":\"2-5\",\"value\":\"blaat\"}")
    } |> Async.RunSynchronously


[<Fact>]
let ``basic delete test`` () =
    async {
        // Given
        let db = { Id = "peer"; Url= "test://peer" }
        let http, urlFuture = testHttpDelete "{ \"id\": \"5\", \"rev\": \"1-5\", \"ok\": true }"

        // When
        let! res = Sofa.delete db http ("5", "1-5")
        let id, rev = res.Value
        
        // Then
        id |> should equal "5"
        rev |> should equal "1-5"
        urlFuture.Value |> should equal (Some "test://peer/5?rev=1-5")
    } |> Async.RunSynchronously

[<Fact>]
let ``map headers`` () =
    // Given
    let headers = 
        [
            ("test1", "test")
            ("test2", "test,test")
            ("test3", "test, test")
            ("test4", "test,")
            ("test5", "test,,test")
            ("test6", "test, test, test, ")
            ("test7", "test, , , ")
        ] |> Map.ofList

    // When
    let mappedHeaders = headers |> mapHeaders |> Map.toList

    // Then
    mappedHeaders.[0] |> should equal ("test1", ["test"])
    mappedHeaders.[1] |> should equal ("test2", ["test"; "test"])
    mappedHeaders.[2] |> should equal ("test3", ["test"; "test"])
    mappedHeaders.[3] |> should equal ("test4", ["test"])
    mappedHeaders.[4] |> should equal ("test5", ["test"; "test"])
    mappedHeaders.[5] |> should equal ("test6", ["test"; "test"; "test"])
    mappedHeaders.[6] |> should equal ("test7", ["test"])
   