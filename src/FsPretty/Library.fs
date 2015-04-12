namespace FsPretty

(*

Port of wl-pprint pretty printing library from Haskell to FSharp.

The code is a very direct translation with minimal changes other than
some renaming due to collisions with existing F# functions and operators,
as well as disallowed characters in infix operator names.  The code is
rearranged from the order in the Haskell such that definitions occur before
their use.

Author: Matthew Sottile <mjsottile@me.com>

March 2015

*)

/// Library of pretty printer primitives for constructing Doc document objects.
module PrettyPrint =

    /// A Doc represents a Document that has been constructed with the set of
    /// provided pretty printer combinators, and then rendered with one of the
    /// provided renderers. 
    type Doc =
        | Empty
        | Char of char
        | Text of int * string
        | Line of bool
        | Cat of Doc * Doc
        | Nest of int * Doc
        | Union of Doc * Doc
        | Column of (int -> Doc)
        | Nesting of (int -> Doc)

    /// The empty document.
    let empty = Empty

    let line = Line false

    let linebreak = Line true

    /// Lift a string into the Doc type.
    let text (s:string) =
        match s.Length with
        | 0 -> Empty
        | l -> Text (l, s)

    /// Place two documents side-by-side
    let beside x y = Cat (x,y)

    /// Infix notation for beside.  In haskell this is <>, but this collides 
    /// poorly with <> in F# so it was renamed.
    let inline (<<>>) (x:Doc) (y:Doc) = beside x y

    // to avoid issues with existing functions like int and float
    // those are renamed to mkint and mkfloat.  then, to keep these
    // functions consistent, the others are also prefaced with "mk"

    /// Given a string with one or more lines of text separated by newline
    /// characters, create a sequence of Doc objects for each line and then
    /// fold the resulting Doc objects together with the <<>> operator
    /// with a line between each.
    let rec mkstring (s:string) =
        let parts = s.Split([|'\n'|])
                    |> Array.map text
        match parts.Length with
        | 0 -> Empty
        | 1 -> parts.[0]
        | _ -> Array.fold (fun (x:Doc) (y:Doc) -> x <<>> line <<>> y)
                          parts.[0] parts.[1..]

    /// Lift a char into the Doc type.
    let mkchar c =
        match c with
        | '\n' -> line
        | c -> Char c

    /// Lift a bool into the Doc type.
    let mkbool = function
        | true -> text "True"
        | false -> text "False"

    /// Lift an integer into the Doc type.
    let mkint (i:int) = string i |> text

    /// Lift a float into the Doc type.
    let mkfloat (f:float) = string f |> text

    /// Lift a double into the Doc type.
    let mkdouble (d:double) = string d |> text

    /// '.' character.
    let dot       = mkchar '.'

    /// '\' character.
    let backslash = mkchar '\\'

    /// '=' character.
    let equals    = mkchar '='

    /// ' ' character.
    let space     = mkchar ' '

    /// ',' character.
    let comma     = mkchar ','

    /// ':' character.
    let colon     = mkchar ':'

    /// ';' character.
    let semi      = mkchar ';'

    /// '"' character.
    let dquote    = mkchar '"' // hacky fix for syntax highligher stupidity -> "

    /// ''' character.
    let squote    = mkchar '\''

    /// ']' character.
    let rbracket  = mkchar ']'

    /// '[' character.
    let lbracket  = mkchar '['

    /// '}' character.
    let rbrace    = mkchar '}'

    /// '{' character.
    let lbrace    = mkchar '{'

    /// ')' character.
    let rparen    = mkchar ')'

    /// '(' character.
    let lparen    = mkchar '('

    /// '>' character.
    let rangle    = mkchar '>'

    /// '<' character.
    let langle    = mkchar '<'

    /// Produce a string with n spaces.
    let spaces = function
        | n when n <= 0 -> ""
        | n             -> String.replicate n " "

    /// Indentation is an alias for spaces.
    let indentation = spaces

    /// Concatenate two Doc objects, separated by a single space.
    let inline (<+>) (x:Doc) (y:Doc) = x <<>> space <<>> y

    /// Fold specialized to Doc types.  Accepts only functions of type Doc -> Doc -> Doc,
    /// and returns the empty Doc when an empty list is provided.
    let fold f = function
        | []    -> empty
        | x::xs -> List.fold f x xs

    /// Concatenate two Doc objects, separating them with a line.  This function was <$>
    /// in Haskell.
    let inline (<*>) x y = x <<>> line <<>> y

    /// Concatenate two Doc objects, separating them with a line break.  This function was <$$>
    /// in Haskell.
    let inline (<**>) x y = x <<>> linebreak <<>> y

    /// Vertical concatenation of a list of Docs.
    let vcat = fold (<**>)

    // break in Haskell is isbreak in F# due to reserved word collision
    let rec flatten = function
        | Cat (x,y)    -> Cat ((flatten x), (flatten y))
        | Nest (i,x)   -> Nest (i, flatten x)
        | Line isbreak -> if isbreak then Empty else Text (1, " ")
        | Union (x,y)  -> flatten x
        | Column f     -> Column (flatten << f)
        | Nesting f    -> Nesting (flatten << f)
        | other        -> other

    let group x = Union (flatten x, x)

    /// A softline behaves line empty if the output fits on one line, otherwise it behaves
    /// like a lineline
    let softline = group line

    /// A softbreak behaves line empty if the output fits on one line, otherwise it behaves
    /// like a linebreak
    let softbreak = group linebreak

    /// Concatenate two documents with a softline in between them.
    let inline (</>) x y = x <<>> softline <<>> y

    /// Concatenate two documents with a softbreak in between them.
    let inline (<//>) x y = x <<>> softbreak <<>> y

    let vsep = fold (<*>)

    let fillSep = fold (</>)

    /// Horizontally concatenate a list of Doc objects separated by spaces.
    let hsep = fold (<+>)

    /// Horizontally concatenate a list of Doc objects.
    let hcat = fold (<<>>)

    /// Concatenate all of the Docs horizontally as long as it fits the page and then
    /// insert a linebreak and continue doing so for the remaining Docs.
    let fillCat = fold (<//>)

    /// Surround a Doc by a left and a right Doc.
    let enclose l r x = l <<>> x <<>> r

    /// Enclose a Doc in brackets.
    let brackets = enclose lbracket rbracket

    /// Enclose a Doc in angle brackets.
    let angles = enclose langle rangle

    /// Enclose a Doc in parenthesis.
    let parens = enclose lparen rparen

    /// Enclose a Doc in braces. 
    let braces = enclose lbrace rbrace

    /// Enclose a Doc in double quotes.
    let dquotes = enclose dquote dquote

    /// Enclose a Doc in single quotes.
    let squotes = enclose squote squote

    let nest i x = Nest (i,x)

    let column f = Column f
    let nesting f = Nesting f

    let sep = group << vsep

    /// Concatenate all of the Documents horizontally if it fits a page or vertically if not.
    let cat = group << vcat
    
    /// Given a list of Doc objects and a punctuation Doc, return the list
    /// with each Doc except the last followed by the punctuation.
    let rec punctuate p = function
        | []    -> []
        | d::[] -> [d]
        | d::ds -> (d <<>> p) :: punctuate p ds

    let width d f = column (fun k1 -> d <<>> column (fun k2 -> f (k2-k1)))

    /// Render the given document and append spaces until the width is f.  If the width is
    /// greather than f without added spaces then nothing is added.
    let fill f d = width d (fun w -> if w >= f then empty else text (spaces (f-w)))

    let align d = column (fun k -> nesting (fun i -> nest (k-i) d))

    /// Enclose a list of Doc objects between a left and right Doc, and
    /// separate the list elements with the given separator Doc.
    let encloseSep left right sep ds =
        match ds with
        | []    -> left <<>> right
        | x::[] -> left <<>> x <<>> right
        | x::xs -> let n = List.length ds
                   let ls = left :: (List.replicate (n-1) sep)
                   align (cat (List.map2 (<<>>) ls ds) <<>> right)

    /// Treat a list of Docs as a tuple (d1,d2,...,dN)
    let tupled = encloseSep lparen rparen comma

    /// Treat a list of Docs as a list [d1,d2,...,dN]
    let mklist = encloseSep lbracket rbracket comma

    /// Treat a list of Docs as a semi-colon separated sequence between braces,
    /// such as {d1;d2;...;dN}
    let semiBraces = encloseSep lbrace rbrace semi

    let hang i d = align (nest i d)

    /// Indent a Doc by the given number of spaces.
    let indent i d = hang i (text (spaces i) <<>> d)

    let fillBreak f x = width x (fun w -> if w > f then nest f linebreak
                                                   else text (spaces (f-w)))
