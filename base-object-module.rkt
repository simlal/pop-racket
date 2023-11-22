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

    ; Add a method to the methods alist of symbol-procedure
    (define (add-method! m-name proc)
      (if (assoc m-name methods)
          (display "method ~a already exists." m-name)
          (set! methods (cons (cons m-name proc) methods))))

    ; Return the name of the created object
    (define (get-name) name)
    (add-method! `get-name get-name)

    ; Change the name of an already existing object
    (define (set-name! new-name)
      (set! name new-name))
    (add-method! `set-name! set-name!)

    ; Get the value for a property (var) at the current state
    (define (get-state var)
      (cdr (assoc var state)))
    (add-method! `get-state get-state)

    ; Change the value for a property
;;     (define (set-state! var value)
;;       (set! (cdr (assoc var state)) value))
;;     (set! methods (cons (cons `set-state! set-state!) methods))

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
    ; Call method dispatcher
    dispatch))
