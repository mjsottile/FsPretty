namespace FsPretty

open PrettyPrint

/// Code for rendering Doc objects.
module Rendering =
    // =======================================================================
    //     Rendering code
    // =======================================================================

    type SimpleDoc =
        | SEmpty
        | SChar of char * SimpleDoc
        | SText of int * string * SimpleDoc
        | SLine of int * SimpleDoc

    type private Docs =
        | Nil
        | Cons of int * Doc * Docs

    let rec private fits w = function
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

    /// Render a SimpleDoc as a string.
    let rec displayS = function
        | SEmpty        -> ""
        | SChar (c,x)   -> string(c) + (displayS x)
        | SText (l,s,x) -> s + (displayS x)
        | SLine (i,x)   -> "\n"+(indentation i)+(displayS x)

    /// Render a Doc as a string assuming an 80 column width.
    let displayString s = renderPretty 0.4 80 s |> displayS

    /// Render a Doc as a string with a given width w.
    let displayStringW w s = renderPretty 0.4 w s |> displayS


