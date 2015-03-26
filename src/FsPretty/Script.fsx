#load "Library.fs"
open FsPretty.PrettyPrint

let x = semiBraces [text "foo" <**> text "bar"]
printfn "%A" (displayString x)