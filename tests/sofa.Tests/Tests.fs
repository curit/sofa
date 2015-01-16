module sofa.Tests

open sofa
open xunit

[<Fact>]
let ``hello returns 42`` () =
  let result = Library.hello 42
  printfn "%i" result
  Assert.AreEqual(42,result)
