#lang typed/racket

(require/typed
 cpsc411/compiler-lib
 [int64?                       (Any -> Boolean)]
 [int32?                       (Any -> Boolean)]
 [dispoffset?                  (Any -> Boolean)]
 [register?                    (Any -> Boolean)]
 [label?                       (Any -> Boolean)]
 [frame-base-pointer-register? (Any -> Boolean)])

(require "../define-language/main.rkt")

(provide (all-defined-out))

(define binop? (λ (x) (memq x '(+ * - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right))))
(define relop? (λ (x) (memq x '(< <= = != >= >))))
(define addr? (λ (x) (match x
                       [`(,fbp - ,offset) #:when (and (register? fbp)
                                                      (dispoffset? offset)) #t]
                       [_ #f])))

(define-language paren-x64-mops-v8

  (terminals
   [Int64      : Integer int64?]
   [Int32      : Integer int32?]
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
   [Opand (Int64 Reg)]
   [Trg   (Reg Label)]
   [Triv  (Trg Int64)]
   [Index (Int32 Reg)])

  (Stmt
   [SetAddrInt32     ([addr : Addr] [int32 : Int32])
                     (set! ,addr ,int32) #:when (and (addr? addr) (int32? int32))]

   [SetAddrTrg       ([addr : Addr] [trg : Trg])
                     (set! ,addr ,trg) #:when (addr? addr)]

   [SetRegTriv       ([reg : Reg] [triv : Triv])
                     (set! ,reg ,triv) #:when (and (register? reg)
                                                   (or (register? triv)
                                                       (label? triv)
                                                       (int64? triv)))]

   [SetRegBinOpInt32 ([reg : Reg] [op : BinOp] [left : Reg] [int32 : Int32])
                     (set! ,reg (,op ,left ,int32)) #:when (and (register? reg)
                                                                (equal? reg left)
                                                                (binop? op)
                                                                (int32? int32))]

   [SetRegBinOpLoc   ([reg : Reg] [op : BinOp] [left : Reg] [loc : Loc])
                     (set! ,reg (,op ,left ,loc)) #:when (and (register? reg)
                                                              (binop? op)
                                                              (equal? reg left))]
   
   [SetRegMref       ([reg : Reg] [base : Reg] [index : Index])
                     (set! ,reg (mref ,base ,index))]

   [SetRegLoc        ([reg : Reg] [loc : Loc])
                     (set! ,reg ,loc)]

   [MsetInt32        ([base : Reg] [index : Index] [int32 : Int32])
                     (mset! ,base ,index ,int32) #:when (int32? int32)]

   [MsetTrg          ([base : Reg] [index : Index] [trg : Trg])
                     (mset! ,base ,index ,trg)]

   [WithLabel        ([label : Label] [stmt : Stmt])
                     (with-label ,label ,stmt)]

   [Jump             ([trg : Trg])
                     (jump ,trg)]

   [Compare          ([reg : Reg] [opand : Opand])
                     (compare ,reg ,opand)]

   [JumpIf           ([relop : RelOp] [label : Label])
                     (jump-if ,relop ,label)]))
