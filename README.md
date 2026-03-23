## define-language v1

This DSL was created with the intention of reducing the friction in encoding BNF grammar as Typed Racket structs. This process involved introducing the types, parsers to convert from s-expressions to these structs, and serializers to go in the other direction. The `define-language` DSL aims to ease this process by deriving the parsers, serializers, and type definitions from a single source of truth. The surface syntax and semantics for this DSL are heavily inspired by Nanopass's `define-language` DSL (our version is a lot less general and tailored towards the way we did things manually).

The structure of our DSL can be described as follows:

```Racket
(define-language <lang-name>
    (terminals 
        [<terminal-name> : <tr-type> <pred>] ...)

    (unions
        [<union-name> (member1 member2 ...)] ...) ; unions can only contain terminals, structs, or other unions 

    (pairs 
        [<pair-name> ([<member1-name> : <tr-type>] [<member2-name> : <tr-type>]) <serialization-pattern>] ...)

    (structs 
        [<struct-name> ([<field-name> : <tr-type>] ...)] <serialization-pattern>)

    (<non-terminal-name> 
        [<variant-name> ([<field-name> : <tr-type>] ...) <serialization-pattern> #:when <expr>] ...
        [#:ref <type-name>])) ; only one of these clauses is allowed, TODO: extend to more refs
```


