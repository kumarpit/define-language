#lang typed/racket

(require/typed
  cpsc411/compiler-lib
  [int64?    (Any -> Boolean)]
  [aloc?     (Any -> Boolean)]
  [fvar?     (Any -> Boolean)]
  [register? (Any -> Boolean)]
  [label?    (Any -> Boolean)])

(require "../define-language/main.rkt")

(provide (all-defined-out))

(define binop? (lambda (x) (memq x '(+ * - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right))))
(define relop? (lambda (x) (memq x '(< <= = != >= >))))
(define info? (lambda (x) #t))
(define loaloc? (lambda (x) (and (list? x)
                                 ((inst andmap Any Boolean Any) aloc? x))))

(define-language imp-cmf-lang-v8

  (terminals
    [Int64 : Integer int64?]
    [Aloc  : Symbol  aloc?]
    [Fvar  : Symbol  fvar?]
    [Reg   : Symbol  register?]
    [Label : Symbol  label?]
    [BinOp : Symbol  binop?]
    [RelOp : Symbol  relop?]
    [Frame : (Listof Aloc) loaloc?]
    [Info  : Any     info?])

  (structs
    [Define  ([label : Label] [info : Info] [tail : Tail])
             (define ,label ,info ,tail)]
    [Program ([info : Info] [defines : (Listof Define)] [tail : Tail])
             (module ,info ,defines ... ,tail)])

  (unions
    [Rloc  (Reg Fvar)]
    [Loc   (Rloc Aloc)]
    [Opand (Int64 Loc)]
    [Triv  (Opand Label)]
    [Trg   (Label Loc)])

  (Pred
    [RelOpPred ([relop : RelOp] [left : Opand] [right : Opand])
               (,relop ,left ,right) #:when (relop? relop)]
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

  (Effect
    [SetEffect         ([target : Loc] [value : Value])
                       (set! ,target ,value)]
    [MSetEffect        ([base : Loc] [index : Opand] [value : Triv])
                       (mset! ,base ,index ,value)]
    [BeginEffect       ([effects : (Listof Effect)] [effect : Effect])
                       (begin ,effects ... ,effect)]
    [IfEffect          ([cond : Pred] [then : Effect] [else : Effect])
                       (if ,cond ,then ,else)]
    [ReturnPointEffect ([label : Label] [tail : Tail])
                       (return-point ,label ,tail)])

  (Value
    [BinOpTriv  ([op : BinOp] [left : Opand] [right : Opand])
                (,op ,left ,right) #:when (binop? op)]
    [MrefValue  ([base : Loc] [index : Opand])
                (mref ,base ,index)]
    [AllocValue ([size : Opand])
                (alloc ,size)]
    [#:ref Triv])

  (Tail
    [JumpTail  ([trg : Trg] [locs : (Listof Loc)])
               (jump ,trg ,locs ...)]
    [BeginTail ([effects : (Listof Effect)] [next : Tail])
               (begin ,effects ... ,next)]
    [IfTail    ([cond : Pred] [then : Tail] [else : Tail])
               (if ,cond ,then ,else)]))
