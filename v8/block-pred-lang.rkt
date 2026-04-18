#lang typed/racket

(require/typed
  cpsc411/compiler-lib
  [int64?      (Any -> Boolean)]
  [dispoffset? (Any -> Boolean)]
  [register?   (Any -> Boolean)]
  [frame-base-pointer-register? (Any -> Boolean)]
  [label?      (Any -> Boolean)])

(require "../define-language/main.rkt")

(provide (all-defined-out))

(define binop? (λ (x) (memq x '(+ * - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right))))
(define relop? (λ (x) (memq x '(< <= = != >= >))))

(define-language block-pred-lang-v7

  (terminals
    [Int64      : Integer int64?]
    [Dispoffset : Integer dispoffset?]
    [Reg        : Symbol  register?]
    [Fbp        : Symbol  frame-base-pointer-register?]
    [Label      : Symbol  label?]
    [BinOp      : Symbol  binop?]
    [RelOp      : Symbol  relop?])

  (structs
    [Addr    ([base : Fbp] [offset : Dispoffset])
             (,base - ,offset)]
    [Block   ([label : Label] [tail : Tail])
             (define ,label ,tail)]
    [Program ([blocks : (Listof Block)])
             (module ,blocks ...)])

  (unions
    [Loc   (Reg Addr)]
    [Opand (Int64 Loc)]
    [Trg   (Label Loc)]
    [Triv  (Opand Label)]
    [Index (Int64 Loc)])

  (Pred
    [RelOpPred ([relop : RelOp] [loc : Loc] [opand : Opand])
               (,relop ,loc ,opand) #:when (relop? relop)]
    [TruePred  ()
               (true)]
    [FalsePred ()
               (false)]
    [NotPred   ([pred : Pred])
               (not ,pred)])

  (Effect
    [SetMRefEffect   ([target : Loc] [base : Loc] [index : Index])
                     (set! ,target (mref ,base ,index))]
    [MSetEffect      ([base : Loc] [index : Index] [value : Triv])
                     (mset! ,base ,index ,value)]
    [SetBinOpEffect ([loc : Loc] [binop : BinOp] [left : Loc] [opand : Opand])
                    (set! ,loc (,binop ,left ,opand))]
    [SetEffect      ([loc : Loc] [triv : Triv])
                    (set! ,loc ,triv)])

  (Tail
    [JumpTail  ([trg : Trg])
               (jump ,trg)]
    [BeginTail ([effects : (Listof Effect)] [tail : Tail])
               (begin ,effects ... ,tail)]
    [IfTail    ([pred : Pred] [trg1 : Trg] [trg2 : Trg])
               (if ,pred (jump ,trg1) (jump ,trg2))]))
