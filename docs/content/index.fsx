(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
FsPretty
======================

This library is an implementation of the pretty printer combinator library described by Philip Wadler in
"A Prettier Printer", (available at <A HREF="http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.19.635">CiteSeer</A>).
It is based on the Haskell wl-pprint implementation.

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The FsPretty library can be <a href="https://nuget.org/packages/FsPretty">installed from NuGet</a>:
      <pre>PM> Install-Package FsPretty</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

The goal of the library is to allow documents to be assembled easily via a set of functional combinators.
For example, a list of integers can be turned into a tuple via:

*)
#r "FsPretty.dll"
open FsPretty.PrettyPrint

let mylist = List.map mkint [1;2;3]
             |> encloseSep lparen rparen comma

printfn "mylist = %s" <| displayString mylist

(**

Samples & documentation
-----------------------

The following are available as documentation.  Please note that the documentation is currently under
active development.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

For license information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/mjsottile/FsPretty/tree/master/docs/content
  [gh]: https://github.com/mjsottile/FsPretty
  [issues]: https://github.com/mjsottile/FsPretty/issues
  [readme]: https://github.com/mjsottile/FsPretty/blob/master/README.md
  [license]: https://github.com/mjsottile/FsPretty/blob/master/LICENSE.txt
*)
