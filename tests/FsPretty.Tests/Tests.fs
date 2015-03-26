module FsPretty.Tests

open FsPretty.PrettyPrint
open NUnit.Framework
open FsUnit

// =======================================================================
//     Test Code
//
// Most of these are intended to verify that the string
// rendered for the Doc matches the Haskell implementation exactly.
//
// The file haskell/test.hs has the test cases that were used to generate
// these tests.
// =======================================================================

let words (s:string) : string list =
  s.Split([|' '|])
  |> List.ofArray
  |> List.filter (fun (s:string) -> s.Length > 0)

[<TestFixture>]
type TestClass() =

  [<Test>]
  member x.``text``() =
    let t = function
    | Text(4,"test") -> true
    | _ -> false

    text "test"
    |> t
    |> should equal true

  [<Test>]
  member x.``spaces``() =
    spaces 4 |> should equal "    "

  [<Test>]
  member x.``no spaces``() =
    spaces 0 |> should equal ""

  [<Test>]
  member x.``fill1``() =
    fill 6 (text "hi")
    |> displayString
    |> should equal "hi    "

  [<Test>]
  member x.``fill2``() =
    fill 6 (text "hello1")
    |> displayString
    |> should equal "hello1"

  [<Test>]
  member x.``tuple``() =
    tupled (List.map mkint [10;200;3000])
    |> displayString
    |> should equal "(10,200,3000)"

  [<Test>]
  member x.``hello world``() =
    text "hello" <<>> space <<>> text "world"
    |> displayString
    |> should equal "hello world"

  [<Test>]
  member x.``hello world2``() =
    text "hello" <*> text "world"
    |> displayString
    |> should equal "hello\nworld"

  [<Test>]
  member x.``hello world nest``() =
    nest 2 (text "hello" <*> text "world") <*> text "!"
    |> displayString
    |> should equal "hello\n  world\n!"

  [<Test>]
  member x.``hang width 40``() =
    hang 4 (fillSep (List.map text (words "the hang combinator indents these words !")))
    |> displayStringW 40
    |> should equal "the hang\n    combinator\n    indents these\n    words !"

  [<Test>]
  member x.``hang width 20``() =
    hang 4 (fillSep (List.map text (words "the hang combinator indents these words !")))
    |> displayStringW 20
    |> should equal "the hang\n    combinator\n    indents\n    these\n    words !"

  [<Test>]
  member x.``punctuate width 15``() =
    let someText = List.map text ["words";"in";"a";"tuple"]
    parens (align (cat (punctuate comma someText)))
    |> displayStringW 15
    |> should equal "(words,\n in,\n a,\n tuple)"

  [<Test>]
  member x.``punctuate``() =
    let someText = List.map text ["words";"in";"a";"tuple"]
    parens (align (cat (punctuate comma someText)))
    |> displayString
    |> should equal "(words,in,a,tuple)"

  [<Test>]
  member x.``list``() =
    let mylist = encloseSep lbracket rbracket comma
    text "list" <+> (mylist (List.map mkint [10;200;3000]))
    |> displayString
    |> should equal "list [10,200,3000]"

  [<Test>]
  member x.``list width 20``() =
    let mylist = encloseSep lbracket rbracket comma
    text "list" <+> (mylist (List.map mkint [10;200;3000]))
    |> displayStringW 20
    |> should equal "list [10\n     ,200\n     ,3000]"

  [<Test>]
  member x.``indent width 20``() =
    indent 4 (fillSep (List.map text (words "the indent combinator indents these words !")))
    |> displayStringW 20
    |> should equal "    the\n    indent\n    combinator\n    indents\n    these\n    words !"

  [<Test>]
  member x.``indent width 40``() =
    indent 4 (fillSep (List.map text (words "the indent combinator indents these words !")))
    |> displayStringW 40
    |> should equal "    the indent\n    combinator\n    indents these\n    words !"

  [<Test>]
  member x.``vsep``() =
    let someText = List.map text (words "text to lay out")
    text "some" <+> vsep someText
    |> displayString
    |> should equal "some text\nto\nlay\nout"

  [<Test>]
  member x.``align``() =
    let someText = List.map text (words "text to lay out")
    text "some" <+> align (vsep someText)
    |> displayString
    |> should equal "some text\n     to\n     lay\n     out"

  [<Test>]
  member x.``complex``() =
    let types = [("empty","Doc"); ("nest","Int -> Doc -> Doc"); ("linebreak","Doc")]
    let ptype = function
      | (name,tp) -> (fill 6 (text name)) <+> text "::" <+> text tp
    text "let" <+> align (vcat (List.map ptype types))
    |> displayString
    |> should equal "let empty  :: Doc\n    nest   :: Int -> Doc -> Doc\n    linebreak :: Doc"
