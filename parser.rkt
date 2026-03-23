#lang racket

;; This file defines the parser for the define-language DSL

(require (for-syntax racket/base
                     racket/list
                     syntax/parse)
         "common.rkt")

(provide (for-syntax parse-forms))

(begin-for-syntax
  (define SECTION-IDENTIFIERS '(terminals unions pairs structs))

  ; stx -> Symbol
  ; returns the leading symbol of each section, meant to identify the section
  ; for eg: (extract-tag #'(terminals ...)) => 'terminals
  (define (extract-tag stx)
    (let ([e (syntax-e stx)])
      (and (pair? e)
           (identifier? (car e))
           (syntax-e (car e)))))


  ; (Listof stx) Symbol -> stx
  ; find the section whose leading keyword matches the given tag
  (define (find-section forms tag)
    (findf (λ (f) (eq? (extract-tag f) tag)) forms))

  ; (Listof stx) -> (Listof stx)
  ; NOTE: all sections with tags that are not section identifiers in the
  ; define-language DSL are non-terminals
  (define (extract-nonterminal-forms forms)
    (filter (λ (f)
              (not (member (extract-tag f) SECTION-IDENTIFIERS)))
            forms))

  ; (Listof stx) -> Forms
  ; (entrypoint) parses the define-language program
  (define (parse-forms forms)
    (define terminals-stx     (find-section forms 'terminals))
    (define unions-stx        (find-section forms 'unions))
    (define pairs-stx         (find-section forms 'pairs))
    (define structs-stx       (find-section forms 'structs))
    (define nonterminals-stx  (extract-nonterminal-forms forms))

    (Forms (parse/terminals terminals-stx)
           (parse/nonterminals nonterminals-stx)
           (parse/unions unions-stx)
           (parse/pairs pairs-stx)
           (parse/structs structs-stx)))

  ; stx -> (Listof (Listof [name : stx] [type : stx] [pred : stx]))
  (define (parse/terminals terminals-stx)
    (if terminals-stx
        (syntax-parse terminals-stx
          #:datum-literals (:)
          [(_ [t-name:id : t-type t-pred:expr] ...)
           (map list
                (syntax->list #'(t-name ...))
                (syntax->list #'(t-type ...))
                (syntax->list #'(t-pred ...)))])
        '()))

  ; stx -> 
  ; (Listof (list [nt-name : stx]
  ;               -- struct variants
  ;               (Listof (list [variant-name : stx]
  ;                             (Listof [field-name : stx])
  ;                             (Listof [field-type : stx])
  ;                             [pat : stx]
  ;                             [when-cond? : (U stx #f)]))
  ;               -- non-struct variants (via #:ref) [only 1 such variant supported for now]
  ;               (Listof [type-name : stx])
  ;               (U #f [nt-name : stx])) ; if alias, store the orignal's name here
  (define (parse/nonterminals nt-stxs)
    (define (parse-prods/nonterminal prods)
      (for/fold ([struct-variants '()]
                 [non-struct-variants '()]
                 #:result (values (reverse struct-variants)
                                  (reverse non-struct-variants)))
                ([p prods])
        (let ([elems (syntax->list p)])
          (if (and (not (null? elems))
                   (keyword? (syntax-e (car elems)))) ; [#:ref Type ...], reference existing types
              ; into the union
              (values struct-variants
                      (append (cdr elems) non-struct-variants))
              ; else [StructName ([field : type] ...) pattern ...]
              (syntax-parse p
                #:datum-literals (:)
                [(sname:id ([fname:id : ftype] ...) pat (~optional (~seq #:when when-cond:expr)))
                 (values (cons (list #'sname
                                     (syntax->list #'(fname ...))
                                     (syntax->list #'(ftype ...))
                                     #'pat
                                     (attribute when-cond)) ; use attribute here because
                               ; when-cond is not always bound!
                               struct-variants)
                         non-struct-variants)])))))

    (append-map (λ (nt-stx)
                  (syntax-parse nt-stx
                    [(nt-name:id (~optional (~seq #:alias (alias:id ...))) prod ...)
                     (define-values (struct-variants non-struct-variants)
                       (parse-prods/nonterminal (syntax->list #'(prod ...))))
                     (cons (list #'nt-name struct-variants non-struct-variants #f)
                           (map (λ (a) (list a struct-variants non-struct-variants (attribute nt-name)))
                                (or (attribute alias) '())))]))
                nt-stxs))

  ; stx -> (Listof (list [union-name : stx] (Listof [type-name : stx])))
  (define (parse/unions unions-stx)
    (if unions-stx
        (syntax-parse unions-stx
          [(_ [u-name:id (u-member:id ...)] ...)
           (map list
                (syntax->list #'(u-name ...))
                (map syntax->list
                     (syntax->list #'((u-member ...) ...))))])
        '()))

  ; stx -> 
  ; (Listof (list [pair-name : stx]
  ;               (Listof [field-name : stx])
  ;               (Listof [field-type : stx])
  ;               [pat : stx]))
  ; TODO: can possibly get rid of the ... because we know there are only two names, types, etc
  (define (parse/pairs pairs-stx)
    (if pairs-stx
        (syntax-parse pairs-stx
          #:datum-literals (:)
          [(_ [p-name:id ([pf-name:id : pf-type] ...) p-pat] ...)
           (map list
                (syntax->list #'(p-name ...))
                (map syntax->list (syntax->list #'((pf-name ...) ...)))
                (map syntax->list (syntax->list #'((pf-type ...) ...)))
                (syntax->list #'(p-pat ...)))])
        '()))

  ; stx ->  
  ; (Listof (list [struct-name : stx]
  ;               (Listof [field-name : stx])
  ;               (Listof [field-type : stx])
  ;               [pat : stx]
  ;               [when-cond? : (U stx #f)]))
  (define (parse/structs structs-stx)
    (if structs-stx
        (syntax-parse structs-stx
          #:datum-literals (:)
          [(_ [st-name:id ([sf-name:id : sf-type] ...) st-pat
                          (~optional (~seq #:when st-when:expr))] ...)
           (map list
                (syntax->list #'(st-name ...))
                (map syntax->list (syntax->list #'((sf-name ...) ...)))
                (map syntax->list (syntax->list #'((sf-type ...) ...)))
                (syntax->list #'(st-pat ...))
                (attribute st-when))])
        '()))

  ; end begin-for-syntax
  )

