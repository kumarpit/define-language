#lang typed/racket

(require (for-syntax racket/base
                     syntax/parse
                     racket/list
                     racket/syntax
                     racket/match
                     racket/pretty)
         "common.rkt")

;; Generates the parsers (in typed racket) for the language defined using the define-language DSL

(provide (for-syntax generate-parsers))

(begin-for-syntax
  ; Forms -> (Listof stx)
  ; Generates the parsers for each of the language forms (in typed/racket)
  (define (generate-parsers forms)
    (append (parsers/terminals forms)
            (parsers/nonterminals forms)
            (parsers/unions forms)
            (parsers/pairs forms)
            (parsers/structs forms)))

  ; Forms -> (Listof stx)
  ; eg:
  ; (terminals [Int64 : Integer int64?])
  ;
  ; generates the following parser:
  ;
  #|
   (: parse-Int64 (Any -> Integer))
   (define (parse-Int64 (x : Any))
     (cond
      ((int64? x) (cast x Integer))
      (else (error 'parse-Int64 (format "type mismatch ~a" x)))))
   |#
  (define (parsers/terminals forms)
    (append-map (match-lambda
                  [(list name type pred)
                   (let ([fun-name #`#,(format-id name "parse-~a" name)])
                     (list
                      #`(: #,fun-name (Any -> #,type))
                      #`(define (#,fun-name [x : Any])
                          (cond
                            [(#,pred x) (cast x #,type)]
                            [else (error '#,fun-name (format "Type mismatch: Expected ~a but got ~a" '#,name x))]))))])
                (Forms-terminals forms)))

  ; Forms -> (Listof stx)
  ; eg:
  #|
  (Tail
    [LetTail   ([bindings : (Listof Binding)] [body : Tail])
               (let (,bindings ...) ,body)]
    [IfTail    ([cond : Pred] [then : Tail] [else : Tail])
               (if ,cond ,then ,else)]
    [CallTail  ([proc : Name] [args : (Listof Triv)])
               (call ,proc ,args ...)]
    [#:ref Value])
  |#
  ; generates the following parser:
  ;
  #|
   (: parse-Tail (Any -> Tail))
   (define (parse-Tail (x : Any))
     (match
      x
      (`(let (,bindings ...) ,body)
       (LetTail
        (map parse-Binding (cast bindings (Listof Any)))
        (parse-Tail body)))
      (`(if ,cond ,then ,else)
       (IfTail (parse-Pred cond) (parse-Tail then) (parse-Tail else)))
      (`(call ,proc ,args ...)
       (CallTail (parse-Name proc) (map parse-Triv (cast args (Listof Any)))))
      (v (parse-Value v))
      (_ (error 'parse-Tail "not a valid Tail"))))
  |#
  (define (parsers/nonterminals forms)
    (append-map
     (match-lambda
       [(list nt-name struct-variants non-struct-variants alias)
        (if alias
          ; just alias the function
            (list #`(define #,(format-id nt-name "parse-~a" nt-name) #,(format-id alias "parse-~a" alias)))
            (let ([struct-clauses    (map struct/make-match-arm struct-variants)]
                  ; TODO: support multiple ref clauses
                  [catch-all-clauses (map (λ (nsv)
                                            #`[v (#,(format-id nsv "parse-~a" nsv) v)])
                                          non-struct-variants)]
                  [fun-name (format-id nt-name "parse-~a" nt-name)])
              (list
               #`(: #,fun-name (Any -> #,nt-name))
               #`(define (#,fun-name [x : Any])
                   (match x
                     #,@struct-clauses
                     #,@catch-all-clauses
                     [_ (error '#,fun-name #,(format "not a valid ~a" (syntax-e nt-name)))])))))])
     (Forms-nonterminals forms)))

  ; Struct -> stx
  ; generates the match clause for a struct
  (define (struct/make-match-arm variant)
    (match variant
      [(list sname fnames ftypes pat when-cond)
       (let ([; maps fields of the struct to their parse calls
              ; for list fields: (map parse-X fname)
              ; for scalar fields: (parse-T fname)
              fnames->parse-calls
              (map (λ (fname ft)
                     (let ([list-elem (try-unwrap-listof ft)])
                       (cons (syntax-e fname)
                             (if list-elem ; NOTE: this means we only support support structs with scalar
                                 ; or 1-level deep list types in fields
                                 ; TODO: can i make n-level deep lists work?
                                 #`(map
                                    #,(format-id list-elem "parse-~a" list-elem)
                                    (cast #,fname (Listof Any)))
                                 #`(#,(format-id ft "parse-~a" ft) #,fname)))))
                   fnames
                   ftypes)])
         (if when-cond
             #`[`#,pat #:when #,when-cond (#,sname #,@(map cdr fnames->parse-calls))]
             #`[`#,pat (#,sname #,@(map cdr fnames->parse-calls))]))]))

  ; stx -> (Listof stx)
  ; eg.
  ; (unions [Triv (Int64 Name)])
  ; generates the parser
  #|
   (: parse-Triv (Any -> Triv))
   (define (parse-Triv (x : Any))
     (cond
      ((int64? x) (parse-Int64 x))
      ((name? x) (parse-Name x))
      (else (error 'parse-Triv (format "not a valid ~a: ~a" 'Triv x)))))
  |#
  (define (parsers/unions forms)
    (append-map (match-lambda
                  [(list name members)
                   (let ([clauses (reverse
                                   (for/fold ([clauses (list)])
                                             ([member (in-list members)])
                                     (let ([pairs (union/resolve-member member forms)])
                                       (append (map (λ (p) #`[(#,(car p) x) #,(cdr p)]) pairs)
                                               clauses))))]
                         [fun-name (format-id name "parse-~a" name)])
                     (list
                      #`(: #,fun-name (Any -> #,name))
                      #`(define (#,fun-name [x : Any])
                          (cond
                            #,@clauses
                            [else (error '#,fun-name
                                         (format "not a valid ~a: ~a" '#,(syntax-e name) x))]))))])
                (Forms-unions forms)))

  ; stx Forms -> (Listof (cons stx stx))
  ; returns list of pairs where the first element in the pair is the predicate to be used for the match clause,
  ; and the second element is the dispatch to the appropriate parser for the member
  (define (union/resolve-member member forms)
    (let ([member-sym (syntax-e member)]
          [parse-call #`(#,(format-id member "parse-~a" member) x)])
      (let* ([is-this-member? (λ (x) (eq? (syntax-e (car x)) member-sym))]
             [terminal-ref (findf is-this-member? (Forms-terminals forms))]
             [union-ref    (findf is-this-member? (Forms-unions forms))]
             [struct-ref   (findf is-this-member? (Forms-structs forms))])
        (cond
          [terminal-ref
           (list (cons (terminal-pred terminal-ref) parse-call))]

          ; TODO: add struct when-cond pat here
          ; ^ though tbh i forgot why i added when-cond for structs?
          [struct-ref (list (cons #`(λ ([x : Any]) : Boolean (match x [`#,(struct-pat struct-ref) #t] [_ #f]))
                                  #`(#,(format-id (struct-name struct-ref) "parse-~a" (struct-name struct-ref)) x)))]

          [union-ref
           ; flatten sub-members to get predicates, but use THIS union's parser
           (map (λ (pair) (cons (car pair) parse-call))
                (append-map (λ (x) (union/resolve-member x forms)) (union-members union-ref)))]
          [else (error "unknown union member ~a" member-sym)]))))

  ; Forms -> (Listof stx)
  ; eg.
  ; (pairs [Binding ([name : Name] [value : Value]) [,name ,value]])
  ; generates the following parser:
  #|
   (: parse-Binding (Any -> Binding))
   (define (parse-Binding (x : Any))
     (match
      x
      (`(,name ,value) (cons (parse-Name name) (parse-Value value)))
      (_ (error 'parse-Binding "invalid Binding"))))
  |#
  (define (parsers/pairs forms)
    (append-map
     (match-lambda
       [(list name fnames ftypes pat)
        ; the parse calls for the two fields, in order
        (let ([fields-parse-call-stx
               (map (λ (fname ft)
                      #`(#,(format-id ft "parse-~a" ft) #,fname))
                    fnames
                    ftypes)]
              [fun-name (format-id name "parse-~a" name)])
          (list
           #`(: #,fun-name (Any -> #,name))
           #`(define (#,fun-name [x : Any])
               (match x
                 [`#,pat (cons #,@fields-parse-call-stx)]
                 [_ (error '#,fun-name #,(format "invalid ~a" (syntax-e name)))]))))])
     (Forms-pairs forms)))

  ; Forms -> (Listof stx)
  ; eg:
  ; (structs [Program ([defs : (Listof Def)] [body : Tail]) (module ,defs ... ,body)])
  ; generates the following parser:
  #|
   (: parse-Program (Any -> Program))
   (define (parse-Program (x : Any))
     (match
      x
      (`(module ,defs ... ,body)
       (Program (map parse-Def (cast defs (Listof Any))) (parse-Tail body)))
      (_ (error 'parse-Program (format "not a valid ~a: ~a" 'Program x))))))
  |#
  (define (parsers/structs forms)
    (append-map (λ (struct)
                  (let ([match-arm (struct/make-match-arm struct)]
                        [fun-name (format-id (struct-name struct) "parse-~a" (struct-name struct))])
                    (list
                     #`(: #,fun-name (Any -> #,(struct-name struct)))
                     #`(define (#,fun-name [x : Any])
                         (match x
                           #,match-arm
                           [_ (error '#,fun-name (format "not a valid ~a: ~a" '#,(syntax-e (struct-name struct)) x))])))))
                (Forms-structs forms)))

  ; end begin-for-syntax
  )
