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

;; heap-index? accepts both int32 and register (the two valid index types)
(define heap-index? (λ (x) (or (int32? x) (register? x))))

(define addr? (λ (x) (match x
                       [`(,fbp - ,offset) #:when (and (register? fbp)
                                                      (dispoffset? offset)) #t]
                       [`(,base + ,index) #:when (and (register? base)
                                                       (heap-index? index)) #t]
                       [_ #f])))

(define-language paren-x64-v8

  (terminals
   [Int64      : Integer int64?]
   [Int32      : Integer int32?]
   [DispOffset : Integer dispoffset?]
   [Reg        : Symbol  register?]
   [Label      : Symbol  label?]
   [Fbp        : Symbol  frame-base-pointer-register?]
   [BinOp      : Symbol  binop?]
   [RelOp      : Symbol  relop?]
   ; TODO: could this be typed as (U Reg Int32)
   [HeapIndex  : Any     heap-index?])

  (structs
   ;; Frame-pointer relative displacement: (rbp - offset)
   [FbpAddr  ([fbp : Fbp] [offset : DispOffset])
             (,fbp - ,offset)]
   ;; Heap address with int32 or register index: (reg + index)
   [HeapAddr ([base : Reg] [index : HeapIndex])
             (,base + ,index) #:when (and (register? base) (heap-index? index))]
   [Program  ([stmts : (Listof Stmt)])
             (begin ,stmts ...)])

  (unions
   [Addr  (FbpAddr HeapAddr)]
   [Loc   (Reg Addr)]
   [Opand (Int64 Reg)]
   [Trg   (Reg Label)]
   [Triv  (Trg Int64)])

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

   [SetRegLoc        ([reg : Reg] [loc : Loc])
                     (set! ,reg ,loc)]

   [WithLabel        ([label : Label] [stmt : Stmt])
                     (with-label ,label ,stmt)]

   [Jump             ([trg : Trg])
                     (jump ,trg)]

   [Compare          ([reg : Reg] [opand : Opand])
                     (compare ,reg ,opand)]

   [JumpIf           ([relop : RelOp] [label : Label])
                     (jump-if ,relop ,label)]))
