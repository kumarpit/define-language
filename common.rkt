#lang racket


(require (for-syntax racket/base
                     syntax/parse
                     racket/list
                     racket/syntax
                     racket/match
                     racket/pretty))

(provide (for-syntax
          ; TODO: why doesn't struct-out work at phase 1?
          Forms Forms?
          Forms-terminals
          Forms-nonterminals
          Forms-unions
          Forms-pairs
          Forms-structs

          try-unwrap-listof

          terminal-name
          terminal-type
          terminal-pred

          struct-name
          struct-fields
          struct-field-types
          struct-pat
          struct-pat-when-cond

          union-name
          union-members

          pair-name
          pair-fields
          pair-field-types
          pair-pat

          is-unquote-elem?
          is-splice-elem?
          unwrap-unquote
          unwrap-splice))

(begin-for-syntax
  (struct Forms (terminals nonterminals unions pairs structs) #:transparent)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; accessors
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Terminal is 3-tuple of the following form:
  ; (Listof [name : stx] [type : stx] [pred : stx])

  (define (terminal-name terminal) (first terminal))
  (define (terminal-type terminal) (second terminal))
  (define (terminal-pred terminal) (third terminal))

  ; Struct is a 5-tuple of the following form:
  ; (Listof [struct-name : stx]
  ;               (Listof [field-name : stx])
  ;               (Listof [field-type : stx])
  ;               [pat : stx]
  ;               [when-cond? : (U stx #f)]))

  (define (struct-name struct) (first struct))
  (define (struct-fields struct) (second struct ))
  (define (struct-field-types struct) (third struct))
  (define (struct-pat struct) (fourth struct))
  (define (struct-pat-when-cond struct) (fifth struct))

  ; Unions is 2-tuple of the following form:
  ; (Listof [union-name : stx] (Listof [member : stx]))

  (define (union-name union) (first union))
  (define (union-members union) (second union))


  ; Pair is a 4-tuple of
  ; ((Listof [pair-name : stx]
  ;     (Listof [field-name : stx])
  ;     (Listof [field-type : stx])
  ;     [pat : stx])))

  (define (pair-name pair) (first pair))
  (define (pair-fields pair) (second pair))
  (define (pair-field-types pair) (third pair))
  (define (pair-pat pair) (fourth pair))

  ;;;;;; end accessors

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; pattern helpers
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; (Listof stx) -> Boolean
  ; true if the pattern elem list starts with an unquote: ,x
  (define (is-unquote-elem? e)
    (and (pair? e)
         (eq? (syntax-e (car e)) 'unquote)))

  ; (Listof stx) -> Boolean
  ; true if the pattern elem list is a splice: ,x ...
  (define (is-splice-elem? e)
    (and (pair? (cdr e))
         (let ([e^ (syntax-e (car e))])
           (and (pair? e^) (eq? (syntax-e (car e^)) 'unquote)))
         (eq? (syntax-e (cadr e)) '...)))

  ; (Listof stx) -> Symbol
  ; extracts the field name symbol from a splice elem: ,x ... -> x
  (define (unwrap-splice e) (syntax-e (cadr (syntax-e (car e)))))

  ; (Listof stx) -> Symbol
  ; extracts the field name symbol from an unquote elem: ,x -> x
  (define (unwrap-unquote e) (syntax-e (cadr e)))

  ;;;;;; end pattern helpers

  ; stx -> (U stx False)
  ; extract X from (Listof X) syntax, or return #f
  (define (try-unwrap-listof ft)
    (match (syntax->list ft)
      [(list (? identifier? kw) ty) #:when (eq? (syntax-e kw) 'Listof)
                                    ty]
      [_ #f]))

  ; end begin-for-syntax
  )
