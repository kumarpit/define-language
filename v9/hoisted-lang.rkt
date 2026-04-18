#lang typed/racket

(require/typed
 cpsc411/compiler-lib
 [aloc?               (Any -> Boolean)]
 [label?              (Any -> Boolean)]
 [int61?              (Any -> Boolean)]
 [uint8?              (Any -> Boolean)]
 [ascii-char-literal? (Any -> Boolean)])

(require "../define-language/main.rkt")

(provide (all-defined-out))

(: empty-literal? (Any -> Boolean))
(define (empty-literal? x)
  (eq? x 'empty))

(: primop? (Any -> Boolean))
(define PRIMOPS
  '(unsafe-fx* unsafe-fx+ unsafe-fx- eq?
               unsafe-fx< unsafe-fx<= unsafe-fx> unsafe-fx>=

               fixnum? boolean? empty? void? ascii-char? error? not

               pair? vector? procedure? cons unsafe-car unsafe-cdr

               unsafe-make-vector unsafe-vector-length
               unsafe-vector-set! unsafe-vector-ref
               unsafe-procedure-arity))
(define (primop? x)
  (if (memq x PRIMOPS) #t #f))

(define-language hoisted-lang-v9

  (terminals
   [Aloc             : Symbol  aloc?]
   [Label            : Symbol  label?]
   [Fixnum           : Integer int61?]
   [Uint8            : Integer uint8?]
   [Bool             : Boolean boolean?]
   [Empty            : Symbol  empty-literal?]
   [AsciiCharLiteral : Char    ascii-char-literal?]
   [PrimOp           : Symbol  primop?])

  (unions
   [AtomTriv (Label Aloc Fixnum Bool Empty AsciiCharLiteral)])

  (pairs
   [Binding       ([name : Aloc] [value : Value]) [,name ,value]]
   [ClosureBinding ([name : Aloc] [value : MakeClosureValue]) [,name ,value]])

  (structs
   [Def              ([name : Label] [params : (Listof Aloc)] [body : Value])
                     (define ,name (lambda ,params ,body))]
   [Program          ([defs : (Listof Def)] [body : Value])
                     (module ,defs ... ,body)]
   [MakeClosureValue ([label : Label] [values : (Listof Value)])
                     (make-closure ,label ,values ...)])

  (Triv
   [VoidTriv  ()
              (void)]
   [ErrorTriv ([code : Uint8])
              (error ,code)]
   [#:ref AtomTriv])

  (Effect
   [PrimOpEffect ([op : PrimOp] [args : (Listof Value)])
                 (,op ,args ...) #:when (primop? op)]
   [BeginEffect  ([effects : (Listof Effect)] [effect : Effect])
                 (begin ,effects ... ,effect)])

  (Value
   [PrimOpValue      ([op : PrimOp] [args : (Listof Value)])
                     (,op ,args ...) #:when (primop? op)]
   [ClosureRefValue  ([closure : Value] [index : Value])
                     (closure-ref ,closure ,index)]
   [ClosureCallValue ([proc : Value] [args : (Listof Value)])
                     (closure-call ,proc ,args ...)]
   [CallValue        ([proc : Value] [args : (Listof Value)])
                     (call ,proc ,args ...)]
   [CletrecValue     ([bindings : (Listof ClosureBinding)] [body : Value])
                     (cletrec (,bindings ...) ,body)]
   [LetValue         ([bindings : (Listof Binding)] [body : Value])
                     (let (,bindings ...) ,body)]
   [IfValue          ([cond : Value] [then : Value] [else : Value])
                     (if ,cond ,then ,else)]
   [BeginValue       ([effects : (Listof Effect)] [value : Value])
                     (begin ,effects ... ,value)]
   [#:ref Triv]))