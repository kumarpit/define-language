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
(define (empty-literal? x) (eq? x 'empty))

(: primop? (Any -> Boolean))
(define PRIMOPS '(unsafe-fx* unsafe-fx+ unsafe-fx- eq?
                             unsafe-fx< unsafe-fx<= unsafe-fx> unsafe-fx>=
                             fixnum? boolean? empty? void? ascii-char? error? not
                             pair? vector? cons unsafe-car unsafe-cdr
                             unsafe-make-vector unsafe-vector-length
                             unsafe-vector-set! unsafe-vector-ref
                             procedure? unsafe-procedure-arity))
(define (primop? x)
  (and (memq x PRIMOPS) #t))

(define-language just-exprs-lang-v9

  (terminals
   [Aloc             : Symbol  aloc?]
   [Fixnum           : Integer int61?]
   [Uint8            : Integer uint8?]
   [Bool             : Boolean boolean?]
   [Empty            : Symbol  empty-literal?]
   [AsciiCharLiteral : Char    ascii-char-literal?]
   [PrimOp           : Symbol  primop?])

  (unions
   [AtomTriv (Aloc Fixnum Bool Empty AsciiCharLiteral)])

  (pairs
   [Binding    ([name : Aloc] [value : Value])   [,name ,value]]
   [LetrecBind ([name : Aloc] [lam  : Lambda])   [,name ,lam]])

  (structs
   [Program ([body : Value])
            (module ,body)]
   [Lambda  ([params : (Listof Aloc)] [body : Value])
            (lambda ,params ,body)])

  (Triv
   [VoidTriv  ()
              (void)]
   [ErrorTriv ([code : Uint8])
              (error ,code)]
   [LambdaTriv ([params : (Listof Aloc)] [body : Value])
               (lambda ,params ,body)]
   [#:ref AtomTriv])

  (Effect
   [PrimOpEffect ([op : PrimOp] [args : (Listof Value)])
                 (,op ,args ...) #:when (primop? op)]
   [BeginEffect  ([effects : (Listof Effect)] [effect : Effect])
                 (begin ,effects ... ,effect)])

  (Value
   [PrimOpValue ([op : PrimOp] [args : (Listof Value)])
                (,op ,args ...) #:when (primop? op)]
   [UnsafeCallValue ([proc : Value] [args : (Listof Value)])
                (unsafe-procedure-call ,proc ,args ...)]
   [LetrecValue ([bindings : (Listof LetrecBind)] [body : Value])
                (letrec (,bindings ...) ,body)]
   [LetValue    ([bindings : (Listof Binding)] [body : Value])
                (let (,bindings ...) ,body)]
   [IfValue     ([cond : Value] [then : Value] [else : Value])
                (if ,cond ,then ,else)]
   [BeginValue  ([effects : (Listof Effect)] [value : Value])
                (begin ,effects ... ,value)]
   [#:ref Triv]))
