module sofa.Tests

open sofa
open Xunit
open FsUnit.Xunit

type Future<'a> () =
    let mutable _value: 'a option = None

    member x.Resolve value =
        if _value.IsSome then failwith "can only resolve once"
        _value <- Some value

    member x.IsResolved with get () = _value.IsSome

    member x.Value 
        with get () = 
            match _value with 
            | Some v -> v
            | None -> failwith "hasn't resolved yet"

let testHttpGet ret = 
    let future = Future<string>()

    let returnResult str = 
        async {
            future.Resolve str
            return Some (ret, [ ("X-Request-Url", [str]) ] |> Map.ofList) 
        }

    (returnResult, future)

let testHttpHead ret = 
    let returnResult str = 
        async {
            return 
                [
                    ("_rev", ["1-" + ret])
                ] |> Map.ofList
        }

    returnResult

type TestData = 
    {
        value: string
    }

[<Fact>]
let ``basic head test`` () =
    async {
        // Given
        let db:Database = { Url = "test://bla" }
        let http = testHttpHead "blaat"

        // When
        let! result = Sofa.head db http 5 

        // Then
        (result |> Map.toList).[0] |> should equal ("_rev", ["1-blaat"])
    } |> Async.RunSynchronously

[<Fact>]
let ``basic get test`` () =
    async {
        // Given
        let db:Database = { Url = "test://bla" }
        let http, urlFuture = testHttpGet "{ \"value\": \"blaat\", \"_id\": 5, \"_rev\": \"5-1\" }" 

        // When
        let! res = Sofa.get<TestData> db defaultSerialzer http 5
        let id, rev, body = res.Value

        // Then
        id |> should equal "5"
        rev |> should equal"5-1"
        body.value |> should equal "blaat"
        urlFuture.IsResolved |> should be True
        urlFuture.Value |> should equal "test://bla/5"
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
   