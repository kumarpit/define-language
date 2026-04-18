#lang typed/racket

(require "../define-language/main.rkt")

(require/typed
  cpsc411/compiler-lib
  [aloc?  (-> Any Boolean)]
  [label? (-> Any Boolean)])

(provide (all-defined-out))

(: int61? (Any -> Boolean))
(define (int61? x)
  (and (exact-integer? x)
       (<= (- (expt 2 60)) x (- (expt 2 60) 1))))

(: uint8? (Any -> Boolean))
(define (uint8? x)
  (and (exact-integer? x)
       (<= 0 x 255)))

(: ascii-char-literal? (Any -> Boolean))
(define (ascii-char-literal? x)
  (and (char? x)
       (< (char->integer x) 128)))

(: empty-literal? (Any -> Boolean))
(define (empty-literal? x)
  (eq? x 'empty))

(: prim-f? (Any -> Boolean))
(define PRIMOPS : (Listof Symbol) ; no longer distinguishing between binops and unops
  '(* + - < <= > >= eq?
    fixnum? boolean? empty? void? ascii-char? error? not
    pair? vector? cons car cdr
    make-vector vector-length vector-set! vector-ref))
(define (prim-f? x)
  (and (symbol? x)
       (if (member x PRIMOPS) #t #f)))

(define-language exprs-unique-lang-v8

  (terminals
    [Aloc             : Symbol  aloc?]
    [Label            : Symbol  label?]
    [Fixnum           : Integer int61?]
    [Bool             : Boolean boolean?]
    [Empty            : Symbol  empty-literal?]
    [UInt8            : Integer uint8?]
    [AsciiCharLiteral : Char    ascii-char-literal?]
    [PrimF            : Symbol  prim-f?])

  (unions
    [AtomTriv (Label Aloc PrimF Fixnum Bool Empty AsciiCharLiteral)])

  (pairs
    [Binding ([name : Aloc] [value : Value]) [,name ,value]])

  (structs
    [Def     ([name : Label] [params : (Listof Aloc)] [body : Value])
             (define ,name (lambda ,params ,body))]
    [Program ([defs : (Listof Def)] [body : Value])
             (module ,defs ... ,body)])

  (Triv
    [VoidTriv  ()
               (void)]
    [ErrorTriv ([code : UInt8])
               (error ,code)]
    [#:ref AtomTriv])

  (Value
    [CallValue ([proc : Value] [args : (Listof Value)])
               (call ,proc ,args ...)]
    [LetValue  ([bindings : (Listof Binding)] [body : Value])
               (let (,bindings ...) ,body)]
    [IfValue   ([cond : Value] [then : Value] [else : Value])
               (if ,cond ,then ,else)]
    [#:ref Triv]))
