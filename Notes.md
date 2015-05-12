Possible demonstrations:

  * Annotations on arguments
      - Name, `static`, etc.
  * Sized arguments
  * Result by ref/val
  * Conversion (`struct array *` <-> `(arr[ ], length)`)

Alternative: annotations as identify functions in the AST; use patch combinators to add annotations.

  * Annotations on function arguments (e.g. `icompile (f . newLen 10)`) are "to late". We need to annotate the whole lambda.
  * Annotations are often specific to a particular back end. We don't want them to leak through the implementation.
  * Annotations are tricky to propagate through the compilation chain. We don't want them to get in the way of optimizations.

Our approach:

  * Additional signature language
  * Only small changes to code generator needed

Size annotations can be done directly on the AST, e.g. using patch combinators.

The method is general:

  * Any expression language that implements `CompExp`

