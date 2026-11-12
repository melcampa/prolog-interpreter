#lang racket

;; ------------------------------------------------------------
;; PART 1: VARIABLES
;; ------------------------------------------------------------

;; Let's start by defining a logic variable! This can be filled
;; in later. Variables have:
;; `name`, which is just for printing, say ?x
;; `id`, which makes every variable *unique*; two different ?x blanks
;; (e.g. from two different uses of the same rule) never get
;; confused with each other.

(struct var (name id) #:transparent)

;; We need a counter to hand out unique ids...
(define counter 0)

;; This lets us create a brand new variable that is uniquely distinct
;; from all other variables, using what we know above.
(define (new-var name)
  (set! counter (add1 counter))
  (var name counter))

;; In Prolog, variables are written as symbols, such that they
;; start with a question mark, like ?x or ?who
(define (is-var-symbol? x)
  (and (symbol? x) ; x *must* be a symbol!
       (char=? (string-ref (symbol->string x) 0) #\?)))

;; We can now define variables! BUT...

;; What if we want to use a term multiple times within one rule?
;; For example, something like append calls itself, and we want the
;; inner call's ?x to be different from the outer ?x.

;; Let's make a way to make a fresh copy of a term for just ONE use
;; of a rule.
(define (rename-term t env)
  (cond [(is-var-symbol? t) (hash-ref! env t (λ () (new-var t)))]
        [(pair? t) (cons (rename-term (car t) env)
                         (rename-term (cdr t) env))]
        [else t]))

;; ------------------------------------------------------------
;; TESTS
;; Run with:  raco test vars.rkt
;; ------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; ---------- new-var ----------

  (let ([v (new-var '?x)])
    ;; It makes a var struct...
    (check-true (var? v))
    ;; ...and remembers the name we gave it.
    (check-equal? (var-name v) '?x))

  ;; Each call increases the counter by one, so ids are consecutive.
  (let* ([a (new-var '?x)]
         [b (new-var '?x)])
    (check-equal? (var-id b) (add1 (var-id a))))

  ;; Two vars with the SAME name are still different vars.
  (let ([a (new-var '?x)]
        [b (new-var '?x)])
    (check-not-equal? a b)
    (check-false (eq? a b)))

  ;; ---------- is-var-symbol? ----------

  ;; Symbols starting with ? are variables.
  (check-true (is-var-symbol? '?x))
  (check-true (is-var-symbol? '?who))
  (check-true (is-var-symbol? '?))

  ;; Other symbols are not.
  (check-false (is-var-symbol? 'x))
  (check-false (is-var-symbol? 'append))
  (check-false (is-var-symbol? 'mel?))  

  ;; Things that aren't symbols are not.
  (check-false (is-var-symbol? 5))
  (check-false (is-var-symbol? "?x"))
  (check-false (is-var-symbol? '(?x)))
  (check-false (is-var-symbol? '()))

  ;; ---------- rename-term ----------

  ;; Things that aren't variables come back unchanged.
  (check-equal? (rename-term 5 (make-hasheq)) 5)
  (check-equal? (rename-term 'melanie (make-hasheq)) 'tom)
  (check-equal? (rename-term '() (make-hasheq)) '())
  (check-equal? (rename-term '(f a 1) (make-hasheq)) '(f a 1))

  ;; A ?symbol becomes a real var that keeps its name.
  (let ([r (rename-term '?x (make-hasheq))])
    (check-true (var? r))
    (check-equal? (var-name r) '?x))

  ;; Inside a list, the plain parts stay and the ?parts become vars.
  (let ([r (rename-term '(f ?x a) (make-hasheq))])
    (check-equal? (first r) 'f)
    (check-true (var? (second r)))
    (check-equal? (third r) 'a))

  ;; Same name twice in one copy => the SAME var (eq? = same object).
  (let ([r (rename-term '(f ?x ?x) (make-hasheq))])
    (check-eq? (second r) (third r)))

  ;; Different names => different vars.
  (let ([r (rename-term '(f ?x ?y) (make-hasheq))])
    (check-not-eq? (second r) (third r)))

  ;; Dotted patterns work: the two ?h's in the append rule match up.
  (let* ([r (rename-term '(append (?h . ?t) ?y (?h . ?z)) (make-hasheq))]
         [first-list (second r)]     ; (?h . ?t)
         [last-list  (fourth r)])    ; (?h . ?z)
    (check-true (var? (car first-list)))
    (check-true (var? (cdr first-list)))
    (check-eq? (car first-list) (car last-list))       ; same ?h
    (check-not-eq? (cdr first-list) (cdr last-list)))  ; ?t and ?z differ

  ;; The table records each name once.
  (let ([env (make-hasheq)])
    (rename-term '(f ?x ?x ?y) env)
    (check-equal? (hash-count env) 2)
    (check-true (hash-has-key? env '?x))
    (check-true (hash-has-key? env '?y)))

  ;; A NEW table gives NEW vars (this is "each use of a rule is fresh").
  (let ([r1 (rename-term '?x (make-hasheq))]
        [r2 (rename-term '?x (make-hasheq))])
    (check-not-eq? r1 r2))

  ;; The SAME table reused gives the same var.
  (let* ([env (make-hasheq)]
         [r1 (rename-term '?x env)]
         [r2 (rename-term '?x env)])
    (check-eq? r1 r2)))

