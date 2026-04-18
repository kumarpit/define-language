#lang typed/racket

(require/typed
 cpsc411/compiler-lib
 [int64?    (Any -> Boolean)]
 [aloc?     (Any -> Boolean)]
 [info-ref  (Any Symbol -> Any)]
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

(: loc? (Any -> Boolean))
(define (loc? x)
  (or (aloc? x) (register? x) (fvar? x)))

(: rloc? (Any -> Boolean))
(define (rloc? x)
  (or (register? x) (fvar? x)))

(define-language asm-pred-lang-v8

  (terminals
   [Int64 : Integer int64?]
   [Aloc  : Symbol  aloc?]
   [Fvar  : Symbol  fvar?]
   [Reg   : Symbol  register?]
   [Label : Symbol  label?]
   [BinOp : Symbol  binop?]
   [RelOp : Symbol  relop?]
   [Frame : (Listof Aloc) loaloc?]

   ; (Info
   ; /locals
   ; + 'new-frames : (Listof Frame)
   ; + 'locals     : (Listof Aloc))
   ;
   ; /undead
   ; + 'undead-out  : UndeadTree
   ; + 'call-undead : CallUndead
   ;
   ; /conflicts
   ; + 'conflicts : ConflictGraph
   ;
   ; /pre-framed
   ; + 'assignment : (Listof (Pairof Aloc Fvar))
   ;
   ; /framed
   ; - 'new-frames
   ; - 'call-undead)
   ;
   ; /spilled
   ; + 'assignment : (Listof Assignment)
   [Info  : Any     info?])

  (structs
   [Block   ([label : Label] [info : Info] [tail : Tail])
            (define ,label ,info ,tail)]
   [Program ([info : Info] [blocks : (Listof Block)] [tail : Tail])
            (module ,info ,blocks ... ,tail)])

  (unions
   [Rloc  (Reg Fvar)]
   [Loc   (Rloc Aloc)]
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
   [JumpTail  ([trg : Trg] [locs : (Listof Loc)])
              (jump ,trg ,locs ...)]
   [BeginTail ([effects : (Listof Effect)] [tail : Tail])
              (begin ,effects ... ,tail)]
   [IfTail    ([cond : Pred] [then : Tail] [else : Tail])
              (if ,cond ,then ,else)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undead sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type UndeadSet (Listof Loc))
(define-type UndeadIfTree (List UndeadTree UndeadTree UndeadTree))
(define-type UndeadTree (U UndeadSet UndeadIfTree (Listof UndeadTree)))

(: undead-add (UndeadSet Loc -> UndeadSet))
(define (undead-add us a)
  (if (member a us) us (cons a us)))

(: undead-remove (UndeadSet Loc -> UndeadSet))
(define (undead-remove us a)
  (filter (lambda ([x : Loc]) (not (eq? x a))) us))

(: undead-set-union (UndeadSet UndeadSet -> UndeadSet))
(define (undead-set-union a b)
  (foldl (lambda ([x : Loc] [acc : UndeadSet])
           (undead-add acc x))
         a
         b))

;; call-undead: set of alocs and fvars live across non-tail calls for the entire block
(define-type CallUndead (Listof (U Aloc Fvar)))

(: call-undead-add (CallUndead Loc -> CallUndead))
(define (call-undead-add cu loc)
  (if (or (member loc cu)
          (register? loc))
      cu
      (cons loc cu)))

(: call-undead-add-from-set (CallUndead UndeadSet -> CallUndead))
(define (call-undead-add-from-set cu us)
  (foldl (lambda ([l : Loc] [acc : CallUndead])
           (if (or (aloc? l) (fvar? l))
               (call-undead-add acc l)
               acc))
         cu
         us))

(: call-undead-union (CallUndead CallUndead -> CallUndead))
(define (call-undead-union a b)
  (foldl (lambda ([x : Loc] [acc : CallUndead])
           (call-undead-add acc x))
         a
         b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conflict graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type ConflictGraph (Listof (List Aloc (Listof Loc))))

(: conflict-graph/get-neighbours (ConflictGraph Aloc -> (Listof Loc)))
(define (conflict-graph/get-neighbours conflicts-list x)
  (let ([entry (assoc x conflicts-list)])
    (if entry (cast (cadr entry) (Listof Loc)) '())))

(: conflict-graph/remove-vertex (ConflictGraph Aloc -> ConflictGraph))
(define (conflict-graph/remove-vertex conflicts node)
  (for/list : ConflictGraph ([entry : (List Aloc (Listof Loc)) conflicts]
                             #:unless (eq? (first entry) node))
    (list (first entry)
          (for/list : (Listof Loc) ([loc (second entry)]
                                    #:unless (eq? loc node))
            loc))))

(: conflict-graph/node-degree (ConflictGraph Aloc -> Natural))
(define (conflict-graph/node-degree graph node)
  (length (conflict-graph/get-neighbours graph node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type PreFramedAssignment (List Aloc Fvar))
(define-type Assignment (List Aloc Rloc))

(: pre-framed-assignment/lookup ((Listof PreFramedAssignment) Aloc -> (Option Fvar)))
(define (pre-framed-assignment/lookup assignments a)
  (match (assoc a assignments)
    [#f #f]
    [(list _ loc) loc]))

(: assignment/lookup ((Listof Assignment) Aloc -> (Option (U Reg Fvar))))
(define (assignment/lookup assignments a)
  (match (assoc a assignments)
    [#f #f]
    [(list _ loc) loc]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: info/new-frames (-> Any (Listof Frame)))
(define (info/new-frames info) (cast (info-ref info 'new-frames) (Listof Frame)))

(: info/locals (-> Any (Listof Aloc)))
(define (info/locals info) (cast (info-ref info 'locals) (Listof Aloc)))

(: info/undead-out (-> Any UndeadTree))
(define (info/undead-out info) (cast (info-ref info 'undead-out) UndeadTree))

(: info/call-undead (-> Any CallUndead))
(define (info/call-undead info) (cast (info-ref info 'call-undead) CallUndead))

(: info/conflicts (-> Any ConflictGraph))
(define (info/conflicts info) (cast (info-ref info 'conflicts) ConflictGraph))

(: info/pre-framed-assignment (-> Any (Listof PreFramedAssignment)))
(define (info/pre-framed-assignment info)
  (cast (info-ref info 'assignment) (Listof PreFramedAssignment)))

(: info/assignment (-> Any (Listof Assignment)))
(define (info/assignment info) (cast (info-ref info 'assignment) (Listof Assignment)))
