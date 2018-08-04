(define variable? symbol?)
(define (same-variable? v1 v2) (eq? v1 v2))

(define (product? expr)
  (and (pair? expr) (eq? (car expr) '*)))

(define (add-lhs expr)
  (cadr expr))

(define (add-rhs expr)
  (caddr expr))

(define (make-sum lhs rhs)
  (cond
    ((eq? lhs 0) rhs)
    ((eq? rhs 0) lhs)
    ((and (variable? lhs) (variable? rhs) (same-variable? lhs rhs))
     (list '* lhs 2))
    (else (list '+ lhs rhs))))

(define (mul-lhs expr)
  (cadr expr))

(define (mul-rhs expr)
  (caddr expr))

(define (make-product lhs rhs)
  (cond
    ((or (eq? lhs 0) (eq? rhs 0)) 0)
    ((eq? lhs 1) rhs)
    ((eq? rhs 1) lhs)
    (else (list '* lhs rhs))))

(define (sum? expr)
  (and (pair? expr) (eq? (car expr) '+)))

(define (differentiate expr var)
  (cond
    ((number? expr)
     0)
    ((variable? expr)
     (if (same-variable? expr var) 1 0))
    ; sum rule: (f + g)' = f' + g'
    ((sum? expr)
     (make-sum (differentiate (add-lhs expr) var)
               (differentiate (add-rhs expr) var)))
    ; product rule: (f * g)' = f' * g + f * g'
    ((product? expr)
     (make-sum (make-product (differentiate (mul-lhs expr) var) (mul-rhs expr))
               (make-product (mul-lhs expr) (differentiate (mul-rhs expr) var))))))

; x^2 + 2x + 3
(define expr '(+ (* x x) (* 2 x) 3))
(display "derivative of ")
(display expr)
(display " is ")
(display (differentiate expr 'x))
(newline)
