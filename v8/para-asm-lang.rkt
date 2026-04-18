#lang typed/racket

(require/typed
 cpsc411/compiler-lib
 [int64?      (Any -> Boolean)]
 [dispoffset? (Any -> Boolean)]
 [register?   (Any -> Boolean)]
 [label?      (Any -> Boolean)]
 [frame-base-pointer-register? (Any -> Boolean)])

(require "../define-language/main.rkt")

(provide (all-defined-out))

(define binop? (λ (x) (memq x '(+ * - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right))))
(define relop? (λ (x) (memq x '(< <= = != >= >))))

(define-language para-asm-lang-v8

  (terminals
   [Int64      : Integer int64?]
   [DispOffset : Integer dispoffset?]
   [Reg        : Symbol  register?]
   [Label      : Symbol  label?]
   [Fbp        : Symbol  frame-base-pointer-register?]
   [BinOp      : Symbol  binop?]
   [RelOp      : Symbol  relop?])

  (structs
   [Addr    ([fbp : Fbp] [offset : DispOffset])
            (,fbp - ,offset)]
   [Program ([stmts : (Listof Stmt)])
            (begin ,stmts ...)])

  (unions
   [Loc   (Reg Addr)]
   [Opand (Int64 Loc)]
   [Trg   (Label Loc)]
   [Triv  (Opand Label)]
   [Index (Int64 Loc)])

  (Stmt
   [SetBinOpEffect  ([target : Loc] [op : BinOp] [left : Loc] [right : Opand])
                    (set! ,target (,op ,left ,right)) #:when (and (binop? op)
                                                                  (equal? target left))]
   [SetMrefEffect   ([target : Loc] [base : Loc] [index : Index])
                    (set! ,target (mref ,base ,index))]
   [SetEffect       ([target : Loc] [value : Triv])
                    (set! ,target ,value)]
   [MsetEffect      ([base : Loc] [index : Index] [value : Triv])
                    (mset! ,base ,index ,value)]
   [WithLabel       ([label : Label] [stmt : Stmt])
                    (with-label ,label ,stmt)]
   [Jump            ([trg : Trg])
                    (jump ,trg)]
   [Compare         ([loc : Loc] [opand : Opand])
                    (compare ,loc ,opand)]
   [JumpIf          ([relop : RelOp] [trg : Trg])
                    (jump-if ,relop ,trg)]))
