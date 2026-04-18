#lang typed/racket

(require cpsc411/compiler-lib)
(require/typed
 cpsc411/compiler-lib
 [int64? (Any -> Boolean)]
 [aloc?  (Any -> Boolean)]
 [label? (Any -> Boolean)])

(require "../define-language/main.rkt")

(provide (all-defined-out))

(define binop? (λ (x) (memq x '(+ * - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right))))
(define relop? (λ (x) (memq x '(< <= = != >= >))))

(define-language exprs-bits-lang-v8

  (terminals
    [Int64 : Integer  int64?]
    [Aloc  : Symbol   aloc?]
    [Label : Symbol   label?]
    [BinOp : Symbol   binop?]
    [RelOp : Symbol   relop?])

  (unions
    [Triv (Int64 Aloc Label)])

  (pairs
    [Binding ([name : Aloc] [value : Value]) [,name ,value]])

  (structs
    [Def     ([name : Label] [params : (Listof Aloc)] [body : Value])
             (define ,name (lambda ,params ,body))]
    [Program ([defs : (Listof Def)] [body : Value])
             (module ,defs ... ,body)])

  (Pred
    [TruePred  ()
               (true)]
    [FalsePred ()
               (false)]
    [NotPred   ([pred : Pred])
               (not ,pred)]
    [LetPred   ([bindings : (Listof Binding)] [body : Pred])
               (let (,bindings ...) ,body)]
    [IfPred    ([cond : Pred] [then : Pred] [else : Pred])
               (if ,cond ,then ,else)]
    [BeginPred ([effects : (Listof Effect)] [pred : Pred])
               (begin ,effects ... ,pred)]
    [RelOpPred ([op : RelOp] [left : Value] [right : Value])
               (,op ,left ,right) #:when (relop? op)])

  (Effect
    [MsetEffect  ([base : Value] [index : Value] [val : Value])
                 (mset! ,base ,index ,val)]
    [BeginEffect ([effects : (Listof Effect)])
                 (begin ,effects ...)])

  (Value
    [BinOpValue  ([op : BinOp] [left : Value] [right : Value])
                 (,op ,left ,right) #:when (binop? op)]
    [MrefValue   ([base : Value] [index : Value])
                 (mref ,base ,index)]
    [AllocValue  ([size : Value])
                 (alloc ,size)]
    [CallValue   ([proc : Value] [args : (Listof Value)])
                 (call ,proc ,args ...)]
    [LetValue    ([bindings : (Listof Binding)] [body : Value])
                 (let (,bindings ...) ,body)]
    [IfValue     ([cond : Pred] [then : Value] [else : Value])
                 (if ,cond ,then ,else)]
    [BeginValue  ([effects : (Listof Effect)] [next : Value])
                 (begin ,effects ... ,next)]
    [#:ref Triv]))
