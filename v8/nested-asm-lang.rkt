#lang typed/racket

(require/typed
  cpsc411/compiler-lib
  [int64?      (Any -> Boolean)]
  [dispoffset? (Any -> Boolean)]
  [register?   (Any -> Boolean)]
  [label?      (Any -> Boolean)])

(require "../define-language/main.rkt")

(provide (all-defined-out))

(define binop? (λ (x) (memq x '(+ * - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right))))
(define relop? (λ (x) (memq x '(< <= = != >= >))))

(define-language nested-asm-lang-v8

  (terminals
    [Int64      : Integer int64?]
    [Dispoffset : Integer dispoffset?]
    [Reg        : Symbol  register?]
    [Label      : Symbol  label?]
    [BinOp      : Symbol  binop?]
    [RelOp      : Symbol  relop?])

  (structs
    [Addr    ([base : Reg] [offset : Dispoffset])
             (,base - ,offset)]
    [Def     ([label : Label] [tail : Tail])
             (define ,label ,tail)]
    [Program ([defs : (Listof Def)] [tail : Tail])
             (module ,defs ... ,tail)])

  (unions
    [Loc   (Reg Addr)]
    [Opand (Int64 Loc)]
    [Triv  (Opand Label)]
    [Trg   (Label Loc)]
    [Index (Int64 Loc)])

  (Pred
    [RelOpPred ([relop : RelOp] [loc : Loc] [opand : Opand])
               (,relop ,loc ,opand) #:when (relop? relop)]
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
    [SetBinOpEffect    ([loc : Loc] [binop : BinOp] [left : Loc] [opand : Opand])
                       (set! ,loc (,binop ,left ,opand)) #:when (and (binop? binop)
                                                                     (equal? loc left))]
    [SetMRefEffect     ([loc1 : Loc] [loc2 : Loc] [index : Index])
                       (set! ,loc1 (mref ,loc2 ,index))]
    [MSetEffect        ([loc : Loc] [index : Index] [triv : Triv])
                       (mset! ,loc ,index ,triv)]
    [SetEffect         ([loc : Loc] [triv : Triv])
                       (set! ,loc ,triv)]
    [BeginEffect       ([effects : (Listof Effect)] [effect : Effect])
                       (begin ,effects ... ,effect)]
    [IfEffect          ([cond : Pred] [then : Effect] [else : Effect])
                       (if ,cond ,then ,else)]
    [ReturnPointEffect ([label : Label] [tail : Tail])
                       (return-point ,label ,tail)])

  (Tail
    [JumpTail  ([trg : Trg])
               (jump ,trg)]
    [BeginTail ([effects : (Listof Effect)] [tail : Tail])
               (begin ,effects ... ,tail)]
    [IfTail    ([cond : Pred] [then : Tail] [else : Tail])
               (if ,cond ,then ,else)]))
