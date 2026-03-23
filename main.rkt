#lang typed/racket

(require (for-syntax racket/base
                     syntax/parse
                     racket/list
                     racket/syntax
                     racket/match
                     racket/pretty))

(require
  "generate-tr-defs.rkt"
  "generate-parsers.rkt"
  "generate-serializers.rkt"
  "parser.rkt"
  "common.rkt")

(require cpsc411/compiler-lib)

(provide define-language)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-language DSL v1
;;
;; Heavily inspired by nanopass's define-language DSL, the idea here
;; is to allow generation of parsers, serializers, and struct definitions
;; needed to typecheck our passes from a single source of truth. This
;; reduces the friction required to add new types, especially since
;; we have so many intermediate languages, and so many versions of each.
;;
;; In an ideal world, we would simply need to paste the BNF grammar; the
;; current version is _close enough_.
;;
;; Usage:
;;
;; We define the grammar in "sections". Each section provides the types for a
;; particular catgory of forms in our grammar (such as terminals, non-terminals,
;; supplementary types [for eg. the Loc type], etc).
;;
;; (define-language lang-name
;;
;;   ; Terminal types: maps a type alias to a Typed Racket type.
;;   (terminals
;;     [TypeAlias : RacketType pred-expr] ...)
;;
;;   ; Union types: (U Member ...) aliases.
;;   ; IMPORTANT: members can only be terminals, standalone structs, or other unions
;;   (unions
;;     [TypeAlias (Member ...)] ...)
;;
;;   ; Pair types: (Pairof T1 T2) aliases.
;;   (pairs
;;     [TypeAlias ([field-name : FieldType] ...) pattern] ...)
;;
;;   ; Standalone structs: structs not belonging to any nonterminal.
;;   ; Useful for top-level forms like Program
;;   (structs
;;     [StructName ([field-name : FieldType] ...) pattern] ...)
;;
;;   ; Nonterminals: each generates a (define-type NT (U Variant ...))
;;   ; followed by one typed struct per variant.
;;   (NonterminalName #:alias (Alias ...)
;;     ; Struct variant:
;;     [VariantName ([field-name : FieldType] ...) parse-pattern]
;;     ...
;;     ; Reference existing types into the union (no new struct generated):
;;     ; only one such clause is supported for now (this may need to be extended)
;;     [#:ref Type ...])
;;   ...)
;;
;;   NOTE: IT IS CRUCIAL YOU MAKE SURE MORE GENERAL PATTERNS APPEAR TOWARDS THE BOTTOM!
;;   (or you use strong enough #:when guards)
;;   For instance, if you have a non-terminal where one variant is serialized as
;;   `(let ,x ,y) and you have another variant that follows the pattern `(,op ,l ,r),
;;   if the second variant appears before the first variant, IT WILL CATCH!
;;   The DSL doesn't support #:when cases for patterns yet, but even with #:when,
;;   it is important you make sure patterns are specific and guarded when there
;;   is an overlap.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-language stx)
  (syntax-parse stx
    [(_ lang-name:id form ...)
     ; Forms (see common.rkt)
     (define forms (parse-forms (syntax->list #'(form ...))))

     ; all of these are (Listof stx)
     (define tr-defs (generate-tr-definitions forms))
     (define parsers (generate-parsers forms))
     (define serializers (generate-serializers forms))

     ; for debugging
     ; TODO: comment out in prod
     #| (pretty-print (syntax->datum #`(begin |#
     #|                                  #,@tr-defs |#
     #|                                  #,@serializers |#
     #|                                  #,@parsers))) |#

     #`(begin
         #,@tr-defs
         #,@serializers
         #,@parsers
         (provide (all-defined-out)))])) ; TODO: why doesn't this work? need to add provide in the file this dsl is used
; ^ william said this should work, therer is probably a bug somehwere here that consumes the final provide statement!

