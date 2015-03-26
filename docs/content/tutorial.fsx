(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Using FsPretty
==============

The goal of a pretty printer combinator library is to make it easy to
render structured documents.  The combinators provide ways of combining
elements of a document together to achieve a layout that is desired without
having to manually deal with things like indentation, nesting, alignment, and
so on.

*)
#r "FsPretty.dll"
open FsPretty.PrettyPrint

let x = semiBraces [text "foo" <**> text "bar"]
printfn "%A" (displayString x)

(**

This tutorial is a placeholder.  More details will be posted soon as the
tutorial is developed.

*)
