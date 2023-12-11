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
  (let ([state (append (list (cons `name name) (cons `friends friends)) (make-alist props))]
        [methods `()]
        [local-calls `()]
        [deleg-calls `()]
        [learned-methods `()])
    ; (displayln state) ; display at construction for debugging
    
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

    ; Fetch all methods of an object
    (define (get-methods) methods)
    (add-method! `get-methods get-methods)

    ;*** Object Name management ***
    ; Return the name of the created object
    (define (get-name) name)
    (add-method! `get-name get-name)

    ; Change the name of an already existing object
    (define (set-name! new-name)
      (set! name new-name)
      new-name)
    (add-method! `set-name! set-name!)
    
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
      (set! state (update-alist state var value))
      var)
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

    ;*** Local-friends management ***
    ; Return the local-friends
    (define (friends) (get-state `friends))
    (add-method! `friends friends)

    ; Predicate to check if an object is in local-friends list
    (define (is-my-friend? object)
      (member object (friends) eq?))    ; Not only value but by reference
    (add-method! `is-my-friend? is-my-friend?)

    ; Add another object to local-friend list
    (define (add-friend! object)
      (if (is-my-friend? object)
          #f    ; Avoid double entries
          (begin (set-state! `friends (append (friends) (list object)))
                 object)))
    (add-method! `add-friend! add-friend!)

    ; Remove an object from the local-friends list
    (define (remove-friend! object)
      (if (not (is-my-friend? object))
          #f
          (begin (set-state! `friends (remove object (friends)))
                 object)))
    (add-method! `remove-friend! remove-friend!)

    ;*** Recursive friends management ***
    ; Get all friends recursively
    (define (friends*)
      ; Use depth-first-search with foldl to iterate and accumnulate
      (define (dfs friend all-friends)
        (if (member friend all-friends eq?)
            all-friends
            (foldl dfs (cons friend all-friends) (friend 'friends))))
      (foldl dfs '() (friends)))
    (add-method! `friends* friends*)

    (define (is-my-friend*? object)
      (member object (friends*) eq?))
    (add-method! `is-my-friend*? is-my-friend*?)
      
    
    ; *** State management in a recursive way with delegation ***
    ; List all the objects with the queried ppty
    (define (owners* var)
      ; Add self to list of owners if possess var
      (filter (lambda (friend) (friend `own? var)) (friends*)))
    (add-method! `owners* owners*)

    ; Predicate to check if any of all objects possess the queried ppty
    (define (own*? var)
        (if (null? (owners* var))
            #f
            #t))
    (add-method! `own*? own*?)

    ; Get state of first ele in self-and-friends*
    (define (get-state* var)
      (if (own*? var)
          ((car (owners* var)) `get-state var)
          `undefined))
    (add-method! `get-state* get-state*)

    ; Set a new value for a property among object and friends
    ; The ppty is created if is not found in any objects
    (define (set-state*! var val)
      (if (own*? var)
          (if (equal? (get-state* var) val)
              #f    ;    if new val is same as old-val
              ; Set the new value if different
              (begin ((car (owners* var)) `set-state! var val)
                     var))
          ; Set the value on the object if not found
          (begin (set-state! var val)
                 var)))
    (add-method! `set-state*! set-state*!)

    ; Remove the property among object and friends if exists
    (define (delete-state*! var)
      (if (own*? var)
          ((car (owners* var)) `delete-state! var)
          #f))
    (add-method! `delete-state*! delete-state*!)

    ; Add a property to an friend if it does not exists
    (define (add-state*! var val object)
      (cond
        [(eq? object dispatch) #f]
        [(object `own? var) #f]
        [(not (is-my-friend*? object)) #f]
        [else (begin (object `set-state! var val)
                     var)]))
    (add-method! `add-state*! add-state*!)
    

    ; *** Recursive method management with delagation ***
    ; Predicate to check if method is in one of self-and-friends
    (define (understand*? selector)
      ; list of objects that understand
      (let ([understand-objs (map (lambda (obj) (obj `understand? selector)) (friends*))])
        (not (null? (filter (lambda (bool-ele) (eq? bool-ele #t)) understand-objs)))))
    (add-method! `understand*? understand*?)

    (define (delegate* selector)
      (let ([understand-objs (filter (lambda (obj) (obj `understand? selector)) (friends*))])
        (if (null? understand-objs)
            (`noOneUnderstands)
            (car understand-objs))))
    (add-method! `delegate* delegate*)

    ; Add or replace a method to a friend's object
    (define (add-method*! selector method object)
      (cond
        [(eq? object dispatch) #f]    ; not self
        [(not (is-my-friend*? object)) #f]    ; obj must be friend
        [else (object `add-method! selector method)]))
    (add-method! `add-method*! add-method*!)

    ; Remove a method from a friend's object
    (define (delete-method*! selector object)
      (cond
        [(eq? object dispatch) #f]    ; not self
        [(not (object `understand? selector)) #f]    ; obj must have method
        [(not (is-my-friend*? object)) #f]    ; obj must be friend
        [else (object `delete-method! selector)]))
    (add-method! `delete-method*! delete-method*!)

    ; *** Delegation and object dynamism ***
    ; Display current state of call history and learning
    (define (display-history-learning)
      (printf "history for object: ~a\n" (get-name))
      (printf "local-calls: ~a\n" local-calls)
      (printf "delegated-calls: ~a\n" deleg-calls)
      (printf "learned-methods: ~a\n" learned-methods)
      (newline))
      
    ; Local calls history logging
    (define (track-and-learn-calls type selector)
      ; Helper to create/modify selected-call-logs with max len of 5
      (define (add-to-log calls-log)
        (if (>= (length calls-log) 5)
            (cons selector (take calls-log 4)) ; Push 6th call out
            (cons selector calls-log)))
      ; Helper to remove from the call-logs
      (define (rm-from-log calls-log selector)
        (filter (lambda(ele) (not (equal? selector ele))) calls-log))
      ; Helper to check if learned methods still in local-calls log
      (define (forget-method learned-log local-log)
        (define (iter-learned learned-log)
          (cond
            [(null? learned-log) `()]
            [(not (member (car learned-log) local-log)) (car learned-log)]
            [else (iter-learned (cdr learned-log))]))
        (iter-learned learned-log))
        
      ; Set the type of calls to modify history to
      (cond
        ; Track local history or forget when needed
        [(equal? type "local")
         (set! local-calls (add-to-log local-calls))
         (let ([method-to-delete (forget-method learned-methods local-calls)])
           (unless (null? method-to-delete)
             (delete-method! method-to-delete)
             (set! learned-methods (rm-from-log learned-methods method-to-delete))))]
        ; Track delegated calls and learn when needed
        [(equal? type "deleg")
         (set! deleg-calls (add-to-log deleg-calls))
         ; Adjust delegation history and learn locally when frequently delegated
         (when (>= (length (filter (lambda(ele) (eq? selector ele)) deleg-calls)) 3)
           (set! local-calls (add-to-log local-calls))
           (set! deleg-calls (rm-from-log deleg-calls selector))
           ; Learn the method locally
           (add-method! selector (cdr (assoc selector ((delegate* selector) `get-methods))))
           (set! learned-methods (add-to-log learned-methods)))]
        [else (error "type must be 'local' or 'deleg'")]))
    
    ; Dispatcher for method calls
    (define (dispatch method . args)
  (let ([dispatch-result (cond
                  [(understand? method)
                   (track-and-learn-calls "local" method)
                   (apply (cdr (assoc method methods)) args)]
                   ;(forget-a-method method)]
                  [(understand*? method)
                   (track-and-learn-calls "deleg" method)
                   (apply (delegate* method) method args)]
                  [else 'doesNotUnderstand])])
    ;(display-history-learning)
    dispatch-result))
    ; Call method dispatcher
    dispatch))
