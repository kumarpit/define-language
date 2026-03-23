#lang typed/racket

(require (for-syntax racket/base
                     racket/list
                     racket/match))

(require "common.rkt")

;; generates the typed-racket definitions for the language defined with define-language 

(provide (for-syntax generate-tr-definitions))

(begin-for-syntax

  ; Forms -> (Listof stx)
  ; entrypoint
  (define (generate-tr-definitions forms)
    (append (tr-defs/terminals forms)
            (tr-defs/nonterminals forms)
            (tr-defs/unions forms)
            (tr-defs/pairs forms)
            (tr-defs/structs forms)))

  ; Forms -> (Listof stx)
  ; eg:
  ; (terminals [Int64 : Integer int64?])
  ; generates:
  #|
   (define-type Int64 Integer)
  |#
  (define (tr-defs/terminals forms)
    (map (match-lambda
           [(list name type _pred)
            #`(define-type #,name #,type)])
         (Forms-terminals forms)))

  ; Forms -> (Listof stx)
  ; eg:
  ; (unions [Triv (Int64 Name)])
  ; generates:
  #|
   (define-type Triv (U Int64 Name))
  |#
  (define (tr-defs/unions forms)
    (map (match-lambda
           [(list name members)
            #`(define-type #,name (U #,@members))])
         (Forms-unions forms)))

  ; Forms -> (Listof stx)
  ; eg:
  ; (pairs [Binding ([name : Name] [value : Value]) [,name ,value]])
  ; generates:
  #|
   (define-type Binding (Pairof Name Value))
  |#
  (define (tr-defs/pairs forms)
    (map (match-lambda
           [(list name _fnames field-types _pat)
            #`(define-type #,name (Pairof #,@field-types))])
         (Forms-pairs forms)))

  ; Forms -> (Listof stx)
  ; eg:
  ; (Pred
  ;   [TruePred  () (true)]
  ;   [RelOpPred ([op : RelOp] [left : Triv] [right : Triv]) (,op ,left ,right) #:when (relop? op)]
  ;   [#:ref Triv])
  ;
  ; generates:
  #|
   (define-type Pred (U TruePred RelOpPred Triv))
   (struct TruePred  ()                                          #:transparent)
   (struct RelOpPred ([op : RelOp] [left : Triv] [right : Triv]) #:transparent)
  |#
  (define (tr-defs/nonterminals forms)
    (append-map
     (match-lambda
       [(list nt-name struct-variants non-struct-variants alias)
        (let* ([variant-names (map first struct-variants)]
               [type-def #`(define-type #,nt-name (U #,@variant-names #,@non-struct-variants))]
               [struct-defs
                (if alias
                    '() ; don't duplicate structs for aliases
                    (map (match-lambda
                           [(list sname fnames ftypes _pat _when-cond)
                            (define typed-fields
                              (map (λ (fn ft) #`[#,fn : #,ft]) fnames ftypes))
                            #`(struct #,sname (#,@typed-fields) #:transparent)])
                         struct-variants))])
          (cons type-def struct-defs))])
     (Forms-nonterminals forms)))

  ; Forms -> (Listof stx)
  ; eg:
  ; (structs [Def ([name : Name] [params : (Listof Name)] [body : Tail]) (define ,name (lambda ,params ,body))])
  ; generates:
  #|
   (struct Def ([name : Name] [params : (Listof Name)] [body : Tail]) #:transparent)
  |#
  (define (tr-defs/structs forms)
    (map (match-lambda
           [(list name fnames ftypes _pat _when-cond)
            (define typed-fields
              (map (λ (fn ft) #`[#,fn : #,ft]) fnames ftypes))
            #`(struct #,name (#,@typed-fields) #:transparent)])
         (Forms-structs forms)))

  ; end begin-for-syntax
  )
