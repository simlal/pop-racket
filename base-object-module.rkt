#lang racket
(provide make-object!)

; ***************** IFT359 / TP2 Groupe AH
; ***************** Lalonde, Simon 22 225 192

;;; object creation ;;;
; name as symbol
; props a flat list of pairs of pptyname-val combinations
(define (make-object! name . props)
  ; Build an association list (pairs list) with the props passed in
    (define (make-alist props)
      (if (null? props)
          `()
          (cons (cons (car props) (cadr props)) (make-alist (cddr props)))))
  ; Store the object's state (props) and init the methods alist
  (let ([state (make-alist props)]
        [methods `()])

    ; Return the name of the created object
    (define (get-name) name)
    (set! methods (cons (cons `get-name get-name) methods))

    ; Change the name of an already existing object
    (define (set-name! new-name)
      (set! name new-name))
    (set! methods (cons (cons `set-name! set-name!) methods))

    ; Get the value for a property (var) at the current state
    (define (get-state var)
      (cdr (assoc var state)))
    (set! methods (cons (cons `get-state get-state) methods))

    ; Dispatcher for method calls
    (define (dispatch method . args)
      (let ([method-pair (assoc method methods)])
        (if method-pair
            (apply (cdr method-pair) args)
            `doesNotUnderstand)))
    (display methods)
    (newline)
    (display state)
    (newline)
    dispatch))
