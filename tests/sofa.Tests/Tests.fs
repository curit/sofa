module sofa.Tests

open sofa
open Xunit
open FsUnit.Xunit

type HttpResult = {
    Url: string
    Result: string
}

let testHttpGet ret = 
    let returnResult str = 
        async {
            return { 
                Url = str
                Result = ret
            }
        }

    returnResult

let testHttpHead ret = 
    let returnResult str = 
        async {
            return 
                [
                    ("_rev", ["1-" + ret])
                ] |> Map.ofList
        }

    returnResult

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
        let http = testHttpGet "blaat"

        // When
        let! result = Sofa.get db (fun a -> a) http 5

        // Then
        result.Result |> should equal "blaat"
        result.Url |> should equal "test://bla/5"
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
   