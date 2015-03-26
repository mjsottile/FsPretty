namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsPretty")>]
[<assembly: AssemblyProductAttribute("FsPretty")>]
[<assembly: AssemblyDescriptionAttribute("FSharp implementation of the Wadler prettier printer library.")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
