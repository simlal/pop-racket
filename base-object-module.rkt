#lang racket
(provide make-object!)

; ***************** IFT359 / TP2 Groupe AH
; ***************** Lalonde, Simon 22 225 192

;;; object creation ;;;
; name as symbol
; props a flat list of pairs of pptyname-val combinations
(define (make-object! name . props)
  ; Build an association list (alist, list of pairs) with the props passed in
  (define (make-alist props)
    (if (null? props)
        `()
        (cons (cons (car props) (cadr props)) (make-alist (cddr props)))))
  ; Helper to update the a value of a ppty on an alist
  (define (update-alist alist key value)
    (if (null? alist)
        `()
        (if (eq? (caar alist) key)
            (cons (cons key value) (cdr alist))
            (cons (car alist) (update-alist (cdr alist) key value)))))
  ; Helper to remove an entry from an alist
  (define (rm-ele-alist alist key)
        (if (null? alist)
            `()
            (if (eq? (caar alist) key)
                (rm-ele-alist (cdr alist) key)
                (cons (car alist) (rm-ele-alist (cdr alist) key)))))
  
  ; Store the object's state (props) and init the methods alist
  (let ([state (make-alist props)]
        [methods `()])

    ; Add/update a method to the methods alist of symbol-procedure
    (define (add-method! selector proc)
      (if (assoc selector methods)
          (set! methods (update-alist methods selector proc))
          (set! methods (cons (cons selector proc) methods)))
      selector)
    (add-method! `add-method! add-method!)

    ; Remove a method from the methods alist
    (define (delete-method! selector)
      (""))
      

    ; Return the name of the created object
    (define (get-name) name)
    (add-method! `get-name get-name)

    ; Change the name of an already existing object
    (define (set-name! new-name)
      (set! name new-name))
    (add-method! `set-name! set-name!)

    ; Get the value for a property (var) at the current state
    (define (get-state var)
      (if (assoc var state)
          (cdr (assoc var state))
          (error "Property name does not exist.")))
    (add-method! `get-state get-state)

    ; Change the value for a property
    (define (set-state! var value)
      (set! state (update-alist state var value)))
    (add-method! `set-state! set-state!)

    ; Delete a property and its value
    (define (delete-state! var)
      (set! state (rm-ele-alist state var)))
    (add-method! `delete-state! delete-state!)

    ; Method validation for dispatcher
    (define (understand? selector)
      (if (assoc selector methods)
          #t
          #f))
    (add-method! `understand? understand?)

    ; Adding method post-construction
    ;(define (add-method! selector method)
      
    
      ; Dispatcher for method calls
      (define (dispatch method . args)
        ;(let ([method-pair (assoc method methods)])
        ;(if method-pair
        (if (understand? method)
            (apply (cdr (assoc method methods)) args)
            `doesNotUnderstand))
       (display methods)
;;       (newline)
;;       (display state)
;;       (newline)
      ; Call method dispatcher
      dispatch))
  