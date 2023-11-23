#lang racket

(provide make-object-with-friends!)

; ***************** IFT359 / TP3 Groupe AH
; ***************** Lalonde, Simon 22 225 192

;;; *** object delegation module *** ;;;
; *** Constructor ***
; name as object name
; friends as list of objects
; props as an association list of ((key . value) ...) pairs
(define (make-object-with-friends! name friends . props)
  ; Build an association list (alist, list of pairs) with the props passed in
  (define (make-alist props)
    (if (null? props)
        `()
        (cons (cons (car props) (cadr props)) (make-alist (cddr props)))))
  ; *** Helpers ***
  ; Helper to update the a value of a ppty on an alist
  (define (update-alist alist key value)
    (cond
      [(null? alist) (list (cons key value))]    ; create alist when empty
      [(eq? (caar alist) key) (cons (cons key value) (cdr alist))]
      [else (cons (car alist) (update-alist (cdr alist) key value))]))

  ; Helper to remove an entry from an alist
  (define (rm-ele-alist alist key)
    (filter (lambda (prop) (not (eq? key (car prop)))) alist))          

  ;*** Init object state at construction ***
  ; Store the object's state (props) and init the methods alist
  (let ([state (make-alist props)]
        [methods `()])

    ;*** Method management ***
    ; Method validation for dispatcher
    (define (understand? selector)
      (pair? (assoc selector methods)))

    ; Add/update a method to the methods alist of symbol-procedure
    (define (add-method! selector proc)
      (if (understand? selector)
          (set! methods (update-alist methods selector proc))
          (set! methods (cons (cons selector proc) methods)))
      selector)
    (add-method! `understand? understand?)    ; placement here to deal with REPL order
    (add-method! `add-method! add-method!)

    ; Remove a method from the methods alist
    (define (delete-method! selector)
      (if (understand? selector)
          (set! methods (rm-ele-alist methods selector))
          `doesNotUnderstand)
      selector)
    (add-method! `delete-method! delete-method!)

    ;*** Object Name management ***
    ; Return the name of the created object
    (define (get-name) name)
    (add-method! `get-name get-name)

    ; Change the name of an already existing object
    (define (set-name! new-name)
      (set! name new-name)
      new-name)
    (add-method! `set-name! set-name!)

    ;*** Local-friends management ***
    ; Return the local-friends
    (define (get-friends) friends)
    (add-method! `get-friends get-friends)

    ; Predicate to check if an object is in local-friends list
    (define (is-my-friend? object)
      (member (object `get-name) friends))
    (add-method! `is-my-friend? is-my-friend?)

    ; Add another object to local-friend list
    (define (add-friend! object)
      (if (is-my-friend? object)
          #f    ; Avoid double entries
          (set! friends (append friends (list (object 'get-name))))))
    (add-method! `add-friend! add-friend!)

    ; Remove an object from the local-friends list
    (define (remove-friend! object)
      (if (is-my-friend? object)
          (set! friends (remove (object 'get-name) friends))
          #f))
    (add-method! `remove-friend! remove-friend!)

    ;TODO Recursive friends management
    ;TODO State management in a recursive way with delegation
    ;TODO Recursive method management with delagation

    ; *** Local properties management ***
    ; Helper to check if ppty is set on an object
    (define (own? var)
      (pair? (assoc var state)))
    (add-method! `own? own?)

    ; Get the value for a property (var) at the current state
    (define (get-state var)
      (if (own? var)
          (cdr (assoc var state))
          `undefined))
    (add-method! `get-state get-state)

    ; Change the value for a property
    (define (set-state! var value)
      (if (own? var)
          (begin (set! state (update-alist state var value))
                 #f)
          (begin (set! state (update-alist state var value))
                 var)))      
    (add-method! `set-state! set-state!)

    ; Delete a property and its value
    (define (delete-state! var)
      (set! state (rm-ele-alist state var))
      var)
    (add-method! `delete-state! delete-state!)

    ; Add a new property
    (define (add-state! var val)
      (if (own? var)
          #f
          (begin (set! state (cons (cons var val) state))
                var)))
    (add-method! `add-state! add-state!)

    ; *** Method parser/closure ***
    ; Dispatcher for method calls
    (define (dispatch method . args)
      (if (understand? method)
          (apply (cdr (assoc method methods)) args)
          `doesNotUnderstand))
    ; Call method dispatcher
    dispatch))
