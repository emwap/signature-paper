# TODO

- [ ] Describe the problem better
- [x] Indent code listings
- [x] Specialize implementation for Feldspar
- [x] Move Feldspar.System.Signature to Feldspar.Compiler.Signature
- [x] Break out signature-paper to it's own repository
- [x] Make a figure of the language code listing



# Reviews

It would have been nice to see a bit more evaluation or case study in
terms of looking at a typical Feldspar application and discussing
kinds of signature "deviations" from the standard calling convention
that are desirable there, and the possibility of expressing them with
the proposed Signature EDSL.

- [x] Explain that Feldspar functions are inlined by default.

- [x] On page 4, it would be useful to explicitly give the (Haskell) type of ex1 (which is the same as for ex2 and ex3, of course).

- [x] On the middle of page 5, I don't understand what is meant by the 2. item.
- [ ] What extra dereferencing in line 9 are meant? And in what sense are they "extra"?
      Even when looking at the generated code at the middle of page 6, which supposedly is meant to improve the 1. and 2. aspects, I don't see somehow fewer dereferencing going on.

  - The C compiler will in many cases optimize the access.

- [ ] At the bottom of page 6, implying that only the shallow embedding is the programmer interface is a bit unclear.
      After all, the example at the bottom of page 5 already used the constructors from the deep embedding (and it was explicitly said there that the code for native and exposeLength comes from the end user).

- [x] Page 10: "which it means it is" -> "which means it is"

Abstract
- [x] lead to ... performance (?) Do you mean performance shortcomings, or performance issues?
- [x] I'd also make the abstract a single paragraph

1.

- [x] <space>,<space>

2.

- [x] "later in the paper" => "in section ABC".

- [x] (Emil?) In the definition native, the code uses the deep embedding, Lam and Native, without introduction.
      Further, what is F.<...> ? I can guess, but this should be explained.

3

- [ ] (Emil?) Be specific about how you are combining deep and shallow.
      This looking like just a deep embedding to me (or the shallow is really a simple wrapper to deep.
      Are you talking about the use of HOAS?


"For example, consider the following two lines"

- [x] Indent them, because the code you are quoting has the indented.

 4.

- [ ] The related work is too short. This paper is about generating wrappers.
      You could mention Haskell FFI, and Java's JNI, for example.

Other:

- [ ] The passing of the length, via DSL, is close to dependent typing.
      You might want to mention the relationship here.

      * Two-level language gives some of the same benefits as dependent types.

This is an interesting idea, but the paper feels as if it needs a bit more context
before it is included in the final proceedings. In particular:

- [ ] the problem is described in terms of Feldspar, and I'm not sure how general the solution is.
      Are there likely other contexts where this approach is necessary, or is it specific to Feldspar?
      As decribed, it feels mostly like an implementation detail, even if it is an interesting one.

      * [x] Create a generalized implementation
      * [ ] Release library and point to its generality

- [x] are there other ways this problem could be addressed?
      For example, could there be a typeclass for 'Data' with different instances for describing the signatures?

      * Yes...

- [x] a brief introduction to Feldspar would also be useful.

- [ ] I think I would find it more convincing if the programmable signatures idea was described more generally,
      without using features specific to Feldspar, ideally with some examples in other domains (even if they are just toy examples).
      It's great to see an idea applied to a DSL which is used in practice, but additional (toy) examples would help to show more general applicability.
