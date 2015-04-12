(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FsPretty"

(**
Using FsPretty
==============

The goal of a pretty printer combinator library is to make it easy to
render structured documents.  The combinators provide ways of combining
elements of a document together to achieve a layout that is desired without
having to manually deal with things like indentation, nesting, alignment, and
so on.

For example, say you want to format a list of integers as a tuple:

*)
#r "FsPretty.dll"
open FsPretty.PrettyPrint
open FsPretty.Rendering

let mylist = [1..6]
let tuple = List.map mkint mylist |> encloseSep lparen rparen comma
printfn "%s" (displayString tuple)

(**

In this case, the list of six integers is first converted to a list of
Doc objects via the mkint function.  This is handed to the encloseSep
function that produces the tuple:

    (1,2,3,4,5,6)

The combinator library also provides functions that help with layout
such as aligning text on multiple lines to a column.  For example, consider
this code that prints one string next to two strings that are stacked
above each other with <*>.   The align function ensures that the two stacked strings are
aligned with respect to the column of their left-most character.

*)

let s = text "foo" <+> align ((text "string1") <*> (text "string2"))
printfn "%s" (displayString s)

(**

This prints the string:

    foo string1
        string2

Pretty printers are often useful when generating code where one wants to
avoid messy sprintf() statements or other manual formatting statements.
The combinators make it easy to build up layout logic that matches the way
you want code laid out.  For example, say we want to generate an F# function
with types explicitly enumerated in the function definition.  We can start off
by making some functions to assemble things like function types, list types,
function arguments with types specified, and the function declaration itself.

*)

let function_type ts = List.map text ts
                       |> punctuate (text "->")
                       |> hcat |> parens
let list_type t = text t <+> text "list"
let argument name ty = parens (text name <<>> colon <<>> ty)
let signature fname args = hsep [(text "let");(text fname)]::args::[equals]

(**

Once we have this, we can put together a simple function.

*)

let fname = "mapper"
let arg1 = argument "f" (function_type ["int";"float"])
let arg2 = argument "l" (list_type "int")
let decl = signature fname [arg1; arg2]
let body = indent 4 ((text "let x = List.map f l") <*> (text "x"))

printfn "%s" (displayString (signature <*> body))

(**

The result is:

    let mapper (f:(int->float)) (l:int list) =
        let x = List.map f l
        x

Using the combinators that take into account alignment, wrapping, and so on,
one can build pretty printers that obey expected rules of layout that are
challenging to handle manually using print statements and simple string
formatting functions.

*)