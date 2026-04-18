#lang typed/racket

(require cpsc411/compiler-lib)
(require/typed
 cpsc411/compiler-lib
 [int64? (Any -> Boolean)]
 [aloc?  (Any -> Boolean)]
 [label? (Any -> Boolean)])

(require "../define-language/main.rkt")

(provide (all-defined-out))

(define binop? (lambda (x) (memq x '(+ * - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right))))
(define relop? (lambda (x) (memq x '(< <= = != >= >))))

(define-language imp-mf-lang-v8

  (terminals
    [Int64 : Integer  int64?]
    [Aloc  : Symbol   aloc?]
    [Label : Symbol   label?]
    [BinOp : Symbol   binop?]
    [RelOp : Symbol   relop?])

  (unions
    [Opand (Int64 Aloc)]
    [Triv  (Opand Label)])

  (structs
    [Def     ([name : Label] [params : (Listof Aloc)] [body : Tail])
             (define ,name (lambda ,params ,body))]
    [Program ([defs : (Listof Def)] [body : Tail])
             (module ,defs ... ,body)])

  (Pred
    [RelOpPred ([op : RelOp] [left : Opand] [right : Opand])
               (,op ,left ,right) #:when (relop? op)]
    [TruePred  ()
               (true)]
    [FalsePred ()
               (false)]
    [NotPred   ([pred : Pred])
               (not ,pred)]
    [BeginPred ([effects : (Listof Effect)] [pred : Pred])
               (begin ,effects ... ,pred)]
    [IfPred    ([cond : Pred] [then : Pred] [else : Pred])
               (if ,cond ,then ,else)])

  (Tail
    [CallTail  ([proc : Triv] [args : (Listof Opand)])
               (call ,proc ,args ...)]
    [BeginTail ([effects : (Listof Effect)] [next : Tail])
               (begin ,effects ... ,next)]
    [IfTail    ([pred : Pred] [then : Tail] [else : Tail])
               (if ,pred ,then ,else)]
    [#:ref Value])

  (Value
    [BinOpTriv  ([op : BinOp] [left : Opand] [right : Opand])
                (,op ,left ,right) #:when (binop? op)]
    [MrefValue  ([base : Aloc] [index : Opand])
                (mref ,base ,index)]
    [AllocValue ([size : Opand])
                (alloc ,size)]
    [IfValue    ([pred : Pred] [then : Value] [else : Value])
                (if ,pred ,then ,else)]
    [BeginValue ([effects : (Listof Effect)] [next : Value])
                (begin ,effects ... ,next)]
    [CallValue  ([proc : Triv] [args : (Listof Opand)])
                (call ,proc ,args ...)]
    [#:ref Triv])

  (Effect
    [SetEffect   ([target : Aloc] [value : Value])
                 (set! ,target ,value)]
    [MSetEffect  ([base : Aloc] [index : Opand] [value : Value])
                 (mset! ,base ,index ,value)]
    [BeginEffect ([effects : (Listof Effect)] [effect : Effect])
                 (begin ,effects ... ,effect)]
    [IfEffect    ([pred : Pred] [then : Effect] [else : Effect])
                 (if ,pred ,then ,else)]))
