namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("sofa")>]
[<assembly: AssemblyProductAttribute("sofa")>]
[<assembly: AssemblyDescriptionAttribute("CouchDB client in f#")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
