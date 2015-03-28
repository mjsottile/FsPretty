#load "Library.fs"
open FsPretty.PrettyPrint

let showit d = d |> displayString |> printfn "%s\n--------"

semiBraces [text "foo" <**> text "bar"] |> showit

(text "Hello") <<>> (mkint 42) |> showit

mkstring "abc\ndef\nghi" |> showit

(text "Hello") <+> (text "world") <+> (mkint 42) |> showit

(text "Hello") <*> (text "world") <*> (mkint 42) |> showit

(text "Hello") <**> (text "world") <**> (mkint 42) |> showit

["this";"is";"a";"test"] |> List.map text |> vcat |> showit

["this";"is";"a";"test"] |> List.map text |> vsep |> showit

["this";"is";"a";"test"] |> List.map text |> hsep |> showit

["this";"is";"a";"test"] |> List.map text |> hcat |> showit

["this";"is";"a";"test"] |> List.map text |> fillSep |> showit

["this";"is";"a";"test"] |> List.map text |> fillCat |> showit

(text "another") </> (text "test") |> showit

(text "another") <//> (text "test") |> showit

(text "string1") <*> nest 1 (text "string2") |> showit

[1..5] |> List.map mkint |> punctuate semi |> hcat |> showit

(text "string1") <*> indent 3 (text "string2") |> showit
