namespace FsPretty

(*

Port of wl-pprint pretty printing library from Haskell to FSharp.
The code is a very direct translation with minimal changes other than
some renaming due to collisions with existing F# functions and operators,
as well as disallowed characters in infix operator names.  The code is
rearranged from the order in the Haskell such that definitions occur before
their use.

Author: Matthew Sottile <matt@sailfan.co>

March 2015

*)

module PrettyPrint =

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

    type SimpleDoc =
        | SEmpty
        | SChar of char * SimpleDoc
        | SText of int * string * SimpleDoc
        | SLine of int * SimpleDoc

    let empty = Empty

    let line = Line false

    let linebreak = Line true

    let text (s:string) =
        match s.Length with
        | 0 -> Empty
        | l -> Text (l, s)

    let beside x y = Cat (x,y)

    // in haskell this is <>, but this collides poorly with <> in F#
    let inline (<<>>) (x:Doc) (y:Doc) = beside x y

    // to avoid issues with existing functions like int and float
    // those are renamed to mkint and mkfloat.  then, to keep these
    // functions consistent, the others are also prefaced with "mk"

    let rec mkstring (s:string) =
        let parts = s.Split([|'\n'|])
                    |> Array.map text
        match parts.Length with
        | 0 -> Empty
        | 1 -> parts.[0]
        | _ -> Array.fold (fun (x:Doc) (y:Doc) -> x <<>> line <<>> y)
                          parts.[0] parts.[1..]

    let mkchar c =
        match c with
        | '\n' -> line
        | c -> Char c

    let mkbool = function
        | true -> text "True"
        | false -> text "False"

    let mkint (i:int) = string i |> text

    let mkfloat (f:float) = string f |> text

    let mkdouble (d:double) = string d |> text

    let dot       = mkchar '.'
    let backslash = mkchar '\\'
    let equals    = mkchar '='
    let space     = mkchar ' '
    let comma     = mkchar ','
    let colon     = mkchar ':'
    let semi      = mkchar ';'
    let dquote    = mkchar '"' // hacky fix for syntax highligher stupidity -> "
    let squote    = mkchar '\''
    let rbracket  = mkchar ']'
    let lbracket  = mkchar '['
    let rbrace    = mkchar '}'
    let lbrace    = mkchar '{'
    let rparen    = mkchar ')'
    let lparen    = mkchar '('
    let rangle    = mkchar '>'
    let langle    = mkchar '<'

    let spaces = function
        | n when n <= 0 -> ""
        | n             -> String.replicate n " "

    let indentation = spaces

    let inline (<+>) (x:Doc) (y:Doc) = x <<>> space <<>> y

    let fold f = function
        | []    -> empty
        | x::xs -> List.fold f x xs

    // <$> and <$$> in haskell, <*> and <**> in F#
    let inline (<*>) x y = x <<>> line <<>> y
    let inline (<**>) x y = x <<>> linebreak <<>> y

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

    let softline = group line

    let softbreak = group linebreak

    let inline (</>) x y = x <<>> softline <<>> y

    let inline (<//>) x y = x <<>> softbreak <<>> y

    let vsep = fold (<*>)

    let fillSep = fold (</>)

    let hsep = fold (<+>)

    let hcat = fold (<<>>)

    let fillCat = fold (<//>)

    let enclose l r x = l <<>> x <<>> r

    let brackets = enclose lbracket rbracket

    let angles = enclose langle rangle

    let parens = enclose lparen rparen

    let braces = enclose lbrace rbrace

    let dquotes = enclose dquote dquote

    let squotes = enclose squote squote

    let nest i x = Nest (i,x)

    let column f = Column f
    let nesting f = Nesting f

    let sep = group << vsep
    let cat = group << vcat

    let rec punctuate p = function
        | []    -> []
        | d::[] -> [d]
        | d::ds -> (d <<>> p) :: punctuate p ds

    let width d f = column (fun k1 -> d <<>> column (fun k2 -> f (k2-k1)))

    let fill f d = width d (fun w -> if w >= f then empty else text (spaces (f-w)))

    let align d = column (fun k -> nesting (fun i -> nest (k-i) d))

    let encloseSep left right sep ds =
        match ds with
        | []    -> left <<>> right
        | x::[] -> left <<>> x <<>> right
        | x::xs -> let n = List.length ds
                   let ls = left :: (List.replicate (n-1) sep)
                   align (cat (List.map2 (<<>>) ls ds) <<>> right)

    let tupled = encloseSep lparen rparen comma

    let mklist = encloseSep lbracket rbracket comma

    let semiBraces = encloseSep lbrace rbrace semi

    let hang i d = align (nest i d)

    let indent i d = hang i (text (spaces i) <<>> d)

    let fillBreak f x = width x (fun w -> if w > f then nest f linebreak
                                                   else text (spaces (f-w)))

    // =======================================================================
    //     Rendering code
    // =======================================================================

    type Docs =
        | Nil
        | Cons of int * Doc * Docs

    let rec fits w = function
        | _ when w < 0  -> false
        | SEmpty        -> true
        | SChar (c,x)   -> fits (w-1) x
        | SText (l,s,x) -> fits (w-l) x
        | SLine (i,x)   -> true

    let renderPretty (rfrac:float) w x =
        let r = max 0 (min w ((float w) * rfrac |> round |> int))
        let nicest n k x y =
            let width = min (w-k) (r-k+n)
            if (fits width x) then x else y
        let rec best n k = function
            | Nil -> SEmpty
            | Cons (i,d,ds) -> match d with
                               | Empty       -> best n k ds
                               | Char c      -> SChar (c, best n (k+1) ds)
                               | Text (l,s)  -> SText (l, s, best n (k+l) ds)
                               | Line _      -> SLine (i, best i i ds)
                               | Cat (x,y)   -> best n k (Cons (i, x, Cons(i, y, ds)))
                               | Nest (j,x)  -> best n k (Cons (i+j, x, ds))
                               | Union (x,y) -> nicest n k (best n k (Cons (i, x, ds)))
                                                           (best n k (Cons (i, y, ds)))
                               | Column f    -> best n k (Cons (i, (f k), ds))
                               | Nesting f   -> best n k (Cons (i, (f i), ds))
        best 0 0 (Cons (0,x,Nil))

    let renderCompact x =
        let rec scan k = function
        | [] -> SEmpty
        | d::ds -> match d with
                   | Empty       -> scan k ds
                   | Char c      -> SChar (c, scan (k+1) ds)
                   | Text (l,s)  -> SText (l, s, scan (k+1) ds)
                   | Line _      -> SLine (0, scan 0 ds)
                   | Cat (x,y)   -> scan k (x::y::ds)
                   | Nest (j,x)  -> scan k (x::ds)
                   | Union (x,y) -> scan k (y::ds)
                   | Column f    -> scan k (f k::ds)
                   | Nesting f   -> scan k (f 0::ds)
        scan 0 [x]

    let rec displayS = function
        | SEmpty        -> ""
        | SChar (c,x)   -> string(c) + (displayS x)
        | SText (l,s,x) -> s + (displayS x)
        | SLine (i,x)   -> "\n"+(indentation i)+(displayS x)

    let displayString s = renderPretty 0.4 80 s |> displayS

    let displayStringW w s = renderPretty 0.4 w s |> displayS

