#lang typed/racket

(require (for-syntax racket/base
                     syntax/parse
                     racket/list
                     racket/syntax
                     racket/match
                     racket/pretty)
         "common.rkt")

;; Generates the serializers (in typed racket) for the language defined using the define-language DSL

(provide (for-syntax generate-serializers))

(begin-for-syntax

  ; Forms -> (Listof stx)
  ; Generates the serializers for each of the language forms (in typed/racket)
  (define (generate-serializers forms)
    (append (serializers/terminals forms)
            (serializers/nonterminals forms)
            (serializers/unions forms)
            (serializers/pairs forms)
            (serializers/structs forms)))

  ; Forms -> (Listof stx)
  ; eg:
  ; (terminals [Int64 : Integer int64?])
  ;
  ; generates:
  #|
   (: serialize-Int64 (Integer -> Any))
   (define (serialize-Int64 [x : Integer]) x)
  |#
  (define (serializers/terminals forms)
    (append-map (match-lambda
                  [(list name type _pred)
                   (list
                    #`(: #,(format-id name "serialize-~a" name) (#,type -> Any))
                    #`(define (#,(format-id name "serialize-~a" name) [x : #,type]) x))])
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
  ; generates:
  #|
   (: serialize-Tail (Tail -> Any))
   (define (serialize-Tail (x : Tail))
     (match
      x
      ((LetTail bindings body)
       (append
        (list 'let)
        (list (append (map serialize-Binding bindings)))
        (list (serialize-Tail body))))
      ((IfTail cond then else)
       (append
        (list 'if)
        (list (serialize-Pred cond))
        (list (serialize-Tail then))
        (list (serialize-Tail else))))
      ((CallTail proc args)
       (append
        (list 'call)
        (list (serialize-Name proc))
        (map serialize-Triv args)))
      (v (serialize-Value (cast v Value)))))
  |#
  ; each variant's pattern is walked, replacing (unquote fname) with
  ; (serialize-FieldType fname), and (unquote fname) ... with
  ; (map serialize-ElemType fname) for list fields.
  (define (serializers/nonterminals forms)
    (append-map
     (match-lambda
       [(list nt-name struct-variants non-struct-variants alias)
        (if alias
            (list #`(define #,(format-id nt-name "serialize-~a" nt-name) #,(format-id alias "serialize-~a" alias)))
            ; catch-all clauses for non-struct variants (e.g. Triv in Value)
            ; cast is required: TR doesn't narrow the type in catch-all match patterns
            (let ([struct-clauses    (map struct/make-match-arm struct-variants)]
                  ; BUG: Assumes just one ref per non-terminal!!!
                  [catch-all-clauses (map (λ (nsv)
                                            #`[v (#,(format-id nsv "serialize-~a" nsv) (cast v #,nsv))])
                                          non-struct-variants)])
              (list
               #`(: #,(format-id nt-name "serialize-~a" nt-name) (#,nt-name -> Any))
               #`(define (#,(format-id nt-name "serialize-~a" nt-name) [x : #,nt-name])
                   (match x
                     #,@struct-clauses
                     #,@catch-all-clauses)))))])
     (Forms-nonterminals forms)))

  ; Struct -> stx
  ; build one match arm for a struct variant with the corresponding serialize calls for each of its
  ; fields
  (define (struct/make-match-arm variant)
    (match variant
      [(list sname fnames ftypes pat _when-cond)
       (let* (; maps fname-sym -> its type
              [ftype-alist (map (λ (fname ft) (cons (syntax-e fname) ft)) fnames ftypes)]

              [; field-serializer-alist: fname-sym -> serializer-call-stx
               ; for list fields: (map serialize-X fname)
               ; for scalar fields: (serialize-T fname)
               field-serializer-alist
               (map (λ (fname ft)
                      (let ([list-elem (try-unwrap-listof ft)])
                        (cons (syntax-e fname)
                              (if list-elem
                                  #`(map #,(format-id list-elem "serialize-~a" list-elem) #,fname)
                                  #`(#,(format-id ft "serialize-~a" ft) #,fname)))))
                    fnames
                    ftypes)])

         ; build the match arm body; match pattern: (SName fn1 fn2 ...)
         (let ([body (let ([chunks (pat/walk-elems (syntax->list pat) ftype-alist field-serializer-alist)])
                       (if (null? chunks) #`'() #`(append #,@chunks)))])
           #`[(#,sname #,@fnames) #,body]))]))

  ; walk a list of pattern elements with ... lookahead
  ; returns a list of syntax chunks, each of which evaluates to a list
  (define (pat/walk-elems elems ftype-alist field-serializer-alist)
    (cond
      [(null? elems) '()]
      ; ,fname ..., splice: (map serialize-ElemType fname)
      [(is-splice-elem? elems)
       (let* ([fname-sym (unwrap-splice elems)]
              [ft        (cdr (assq fname-sym ftype-alist))]
              [elem-type (try-unwrap-listof ft)])
         (unless elem-type
           (error "splice of non-list field ~a" fname-sym))
         (cons (cdr (assq fname-sym field-serializer-alist))
               (pat/walk-elems (cddr elems) ftype-alist field-serializer-alist)))] ; IMPORTANT: skip the ..., TODO: this is really finnicky
      ; normal element, wrap singleton in list
      [else
       (cons #`(list #,(pat/walk-elem (car elems) ftype-alist field-serializer-alist))
             (pat/walk-elems (cdr elems) ftype-alist field-serializer-alist))]))

  ; walk a single pattern element
  (define (pat/walk-elem p ftype-alist field-serializer-alist)
    (let ([e (syntax-e p)])
      (cond
        ; ,fname, substitute with serializer call
        [(is-unquote-elem? e) (cdr (assq (unwrap-unquote e) field-serializer-alist))]
        ; nested list, recurse, combining chunks with append
        [(pair? e)
         (let ([chunks (pat/walk-elems (syntax->list p) ftype-alist field-serializer-alist)])
           (if (null? chunks)
               #`'()
               #`(append #,@chunks)))]
        ; literal atom, quote it
        [else #`'#,p])))

  ; Forms -> (Listof stx)
  ; eg:
  ; (unions [Triv (Int64 Name)])
  ; generates:
  #|
   (: serialize-Triv (Triv -> Any))
   (define (serialize-Triv [x : Triv])
     (cond
      ((int64? x) (serialize-Int64 (cast x Integer)))
      ((name? x)  (serialize-Name  (cast x Symbol)))
      (else (error 'serialize-Triv (format "not a valid ~a: ~a" 'Triv x)))))
  |#
  (define (serializers/unions forms)
    (append-map (match-lambda
                  [(list name members)
                   (let ([clauses
                          (append-map (λ (member)
                                        (map (λ (p) #`[(#,(car p) x) #,(cdr p)])
                                             (union/resolve-member member forms)))
                                      members)]
                         [fun-name (format-id name "serialize-~a" name)])
                     (list
                      #`(: #,fun-name (#,name -> Any))
                      #`(define (#,fun-name [x : #,name])
                          (cond
                            #,@clauses
                            [else (error '#,fun-name
                                         (format "not a valid ~a: ~a" '#,(syntax-e name) x))]))))])
                (Forms-unions forms)))

  ; stx Forms -> (Listof stx)
  ; returns list of pairs where the first element in the pair is the predicate to be used for the cond clause,
  ; and the second element is the dispatch to the appropriate serializer for the member
  (define (union/resolve-member member forms)
    (let ([member-sym    (syntax-e member)]
          [serialize-call #`(#,(format-id member "serialize-~a" member) (cast x #,member))])
      (let* ([is-this-member? (λ (x) (eq? (syntax-e (car x)) member-sym))]
             [terminal-ref (findf is-this-member? (Forms-terminals forms))]
             [union-ref    (findf is-this-member? (Forms-unions forms))]
             [struct-ref   (findf is-this-member? (Forms-structs forms))])
        (cond
          [terminal-ref
           (list (cons (third terminal-ref)
                       #`(#,(format-id member "serialize-~a" member)
                          (cast x #,(second terminal-ref)))))]
          [struct-ref
           (list (cons (format-id (first struct-ref) "~a?" (first struct-ref))
                       serialize-call))]
          [union-ref
           ; flatten sub-members' preds, but dispatch through THIS union's serializer
           (map (λ (pair) (cons (car pair) serialize-call))
                (append-map (λ (x) (union/resolve-member x forms)) (second union-ref)))]
          [else (error "unknown union member ~a" member-sym)]))))

  ; Forms -> (Listof stx)
  ; eg:
  ; (pairs [Binding ([name : Name] [value : Value]) [,name ,value]])
  ; generates:
  #|
   (: serialize-Binding (Binding -> Any))
   (define (serialize-Binding (x : Binding))
     (list (serialize-Name (car x)) (serialize-Value (cdr x))))
  |#
  ; walks the dsl pattern, replacing (unquote fname) positions with the
  ; appropriate serialize-<FieldType> call on car/cdr.
  (define (serializers/pairs forms)
    (append-map (match-lambda
                  [(list name fnames field-types pat)
                   ; maps field-name -> serializer-call syntax
                   (define field-alist
                     (map (λ (fname ft accessor)
                            (cons (syntax-e fname)
                                  #`(#,(format-id ft "serialize-~a" ft) (#,accessor x))))
                          fnames
                          field-types
                          (list #'car #'cdr)))

                   ;walk the pattern, substituting unquote positions
                   (define (walk p)
                     (let ([e (syntax-e p)])
                       (cond
                         ; ,<name>
                         [(and (pair? e) (eq? (syntax-e (car e)) 'unquote))
                          (cdr (assq (syntax-e (cadr e)) field-alist))]
                         [(pair? e)
                          #`(list #,@(map walk (syntax->list p)))]
                         ; quote literal atoms
                         [else #`'#,p])))

                   (list
                    #`(: #,(format-id name "serialize-~a" name) (#,name -> Any))
                    #`(define (#,(format-id name "serialize-~a" name) [x : #,name])
                        #,(walk pat)))])
                (Forms-pairs forms)))

  ; Forms -> (Listof stx)
  ; eg:
  ; (structs [Program ([defs : (Listof Def)] [body : Tail]) (module ,defs ... ,body)])
  ;
  ; generates:
  #|
   (: serialize-Program (Program -> Any))
   (define (serialize-Program (x : Program))
     (match
      x
      ((Program defs body)
       (append
        (list 'module)
        (map serialize-Def defs)
        (list (serialize-Tail body))))))
  |#
  ; TODO: this is very similar to the non-terminal case, should reduce code duplication
  (define (serializers/structs forms)
    (append-map (λ (struct)
                  (let ([clause (struct/make-match-arm struct)]
                        [fun-name (format-id (struct-name struct) "serialize-~a" (struct-name struct))])
                    (list
                     #`(: #,fun-name (#,(struct-name struct) -> Any))
                     #`(define (#,fun-name [x : #,(struct-name struct)])
                         (match x
                           #,clause)))))
                (Forms-structs forms)))

  ; end begin-for-syntax
  )
