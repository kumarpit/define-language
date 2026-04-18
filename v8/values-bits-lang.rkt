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

(define-language values-bits-lang-v8

  (terminals
    [Int64 : Integer  int64?]
    [Aloc  : Symbol   aloc?]
    [Label : Symbol   label?]
    [BinOp : Symbol   binop?]
    [RelOp : Symbol   relop?])

  (unions
    [Opand (Int64 Aloc)]
    [Triv  (Opand Label)])

  (pairs
    [Binding ([name : Aloc] [value : Value]) [,name ,value]])

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
    [LetPred   ([bindings : (Listof Binding)] [body : Pred])
               (let (,bindings ...) ,body)]
    [IfPred    ([cond : Pred] [then : Pred] [else : Pred])
               (if ,cond ,then ,else)]
    [BeginPred ([effects : (Listof Effect)] [pred : Pred])
               (begin ,effects ... ,pred)])

  (Effect
    [MsetEffect  ([base : Aloc] [index : Opand] [val : Value])
                 (mset! ,base ,index ,val)]
    [LetEffect   ([bindings : (Listof Binding)] [body : Effect])
                 (let (,bindings ...) ,body)]
    [BeginEffect ([effects : (Listof Effect)])
                 (begin ,effects ...)])

  (Value
    [BinOpValue  ([op : BinOp] [left : Opand] [right : Opand])
                 (,op ,left ,right) #:when (binop? op)]
    [MrefValue   ([base : Aloc] [index : Opand])
                 (mref ,base ,index)]
    [AllocValue  ([size : Opand])
                 (alloc ,size)]
    [LetValue    ([bindings : (Listof Binding)] [body : Value])
                 (let (,bindings ...) ,body)]
    [IfValue     ([cond : Pred] [then : Value] [else : Value])
                 (if ,cond ,then ,else)]
    [CallValue   ([proc : Triv] [args : (Listof Opand)])
                 (call ,proc ,args ...)]
    [BeginValue  ([effects : (Listof Effect)] [next : Value])
                 (begin ,effects ... ,next)]
    [#:ref Triv])

  (Tail
    [LetTail   ([bindings : (Listof Binding)] [body : Tail])
               (let (,bindings ...) ,body)]
    [IfTail    ([cond : Pred] [then : Tail] [else : Tail])
               (if ,cond ,then ,else)]
    [CallTail  ([proc : Triv] [args : (Listof Opand)])
               (call ,proc ,args ...)]
    [BeginTail ([effects : (Listof Effect)] [next : Tail])
               (begin ,effects ... ,next)]
    [#:ref Value]))
