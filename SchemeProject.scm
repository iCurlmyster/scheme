;Scheme project

; Function to make a list of all characters in file
(define (char-list x)
  (let ((p (read-char x)))
    (if (eof-object? p)
      `()
      (cons p (char-list x)))))

; function to open file and get list of chars in file
(define (get-contents filename)
  (char-list (open-input-file filename) ) )

; expressions variable to hold list of chars
(define expressions (get-contents "data.txt"))

; Function to determine if a character is an operation
(define (is-op? x)
  (if (char? x)
    (cond
      ((char=? #\+ x) #t)
      ((char=? #\* x) #t)
      ((char=? #\- x) #t)
      (else #f) )
    #f))

; get-word is suppose to have someone check that the first item in a list is already
; a character before calling this method. This method accepts alphanumeric characters
(define (get-word x)
  (let loop ((y x))
    (cond
      ((null? y) '())
      ((char-alphanumeric? (car y)) (cons (car y) (loop (cdr y))) )
      (else '() ) ) ))

; function to encapsulate numbers
(define (get-number x)
  (let loop ((y x))
    (cond
      ((null? y) '())
      ((char-numeric? (car y)) (cons (car y) (loop (cdr y)) ))
      (else '())  ) ))

; Function to adjust the character list buffer of the file after getting a word or function
(define (adjust-buffer x y)
  (cond
    ((null? x) y)
    (else (adjust-buffer (cdr x) (cdr y)) ) ))

; function to parse all of the objects and expressions into a large list
(define (parse-expr x)
  (let ((ord '()) (all-expr '()) (paran-count 0))
    (let checking ((check expressions))
    (cond ((null? check) '())
      ((char=? #\( (car check))
        (set! paran-count 1)
        (let loop ((y (cdr check)))
          (cond
            ((eq? paran-count 0)(set! all-expr (cons (reverse ord) all-expr)) (set! ord '()) (checking y))
            ((char=? #\( (car y)) (set! paran-count (+ paran-count 1)) (loop (cdr y)) )
            ((char=? #\) (car y)) (set! paran-count (- paran-count 1)) (loop (cdr y)) )
            ((is-op? (car y)) (set! ord (cons (car y) ord)) (loop (cdr y)))
            ((char-alphabetic? (car y)) (let ((word (get-word y))) (set! ord (cons word ord)) (loop  (adjust-buffer word y)))  )
            ((char-numeric? (car y)) (let ((num (get-number y))) (set! ord (cons num ord)) (loop (adjust-buffer num y)) ))
            (else (loop (cdr y)))  ) ) )
      (else (checking (cdr check)) ) ) ) all-expr) )


(define (print-list x)
  (cond
    ((null? x) (display (newline)) '())
    (else (display (newline)) (display (car x)) (print-list (cdr x)) ) ))

; stack-exp variable to hold the list of expressions
(define stack-exp (reverse (parse-expr expressions)))

(print-list stack-exp)

; function to check if an expression has variables or not
(define (has-vars? expr)
  (if (null? expr)
    #f
    (let ((elem (car expr)))
      (if (list? elem)
        (cond
          ((char-alphabetic? (car elem)) #t)
          (else (has-vars? (cdr expr))) )
        (has-vars? (cdr expr)) ) ) ) )

; function to check if the expression has all same operations
(define (has-same-ops? expr)
  (let ((first-sign (car expr)))
    (let check-signs ((si (cdr expr)))
      (cond
        ((null? si) #t)
        ((is-op? (car si)) (if (char=? first-sign (car si))
                              (check-signs (cdr si))
                              #f ) )
        (else (check-signs (cdr si))) ) ) ) )

; function to return the correct operation
(define (return-op x)
  (cond
    ((char=? #\+ x) +)
    ((char=? #\- x) -)
    ((char=? #\* x) *) ))

; function to get the constants out of the expression and return them as a list
(define (get-constants expr)
  (cond
    ((null? expr) '())
    ((list? (car expr)) (if (char-numeric? (car (car expr)))
                          (cons (string->number (list->string (car expr))) (get-constants (cdr expr)))
                          (get-constants (cdr expr)) ) )
    (else (get-constants (cdr expr))) ) )

; Function to rearrange the expression for different operation expressions for easier manipulation
(define (return-rearranged expr index move-count)
  (let ((first-expr (sublist expr 0 index))
        (sec-expr (sublist expr (+ 1 index) (- (length expr) move-count))) )
    (if (< 0 move-count)
      (append first-expr sec-expr (list (list-ref expr index)) (sublist expr (- (length expr) move-count) (length expr)))
      (append first-expr sec-expr (list (list-ref expr index)) ) ) ) )

;Function to rearrange the expression for same operation expressions for easier manipulation
(define (return-rearranged-same expr index)
  (let ((first-expr (sublist expr 0 index))
        (sec-expr (sublist expr (+ 1 index) (+ 2 index)))
        (third-expr (sublist expr (+ 2 index) (length expr))) )
      (append first-expr sec-expr (list (list-ref expr index)) third-expr) ) )

; function to go through expressions with same operations and check if the expression is formated correctly
(define (same-ops-with-vars expr op-count iterator)
  (cond
    ((= 0 op-count) expr)
    ((is-op? (list-ref expr iterator)) (same-ops-with-vars expr (- op-count 1) (+ iterator 1)))
    (else (same-ops-with-vars (return-rearranged-same expr iterator) op-count iterator) ) ) )

; function to go through expressions with different operations and check if the expression is formated correctly
(define (diff-ops-with-vars expr op-count iterator move-count)
  (cond
    ((= 0 op-count) expr)
    ((is-op? (list-ref expr iterator)) (diff-ops-with-vars expr (- op-count 1) (+ 1 iterator) move-count))
    (else (diff-ops-with-vars (return-rearranged expr iterator move-count) op-count iterator (+ 1 move-count))) ) )

; function to check if something is a variable
(define (is-var? x)
  (cond
    ((list? x) (if (char-alphabetic? (car x))
                 #t
                 #f))
    (else #f) ) )

; function to return the expression passed in with the selected index of a variable to be rearranged to a correct position
(define (return-rearranged-vars expr index move-count)
  (let ((first-expr (sublist expr (+ 0 move-count) index))
        (sec-expr (sublist expr (+ 1 index) (length expr))) )
        (if (< 0 move-count)
          (append (sublist expr 0 (+ 0 move-count)) (list (list-ref expr index)) first-expr sec-expr)
          (append (list (list-ref expr index)) first-expr sec-expr))  ) )

; function to go through expression and rearrange variables whenever it finds one
(define (rearrange-vars expr)
  (let ((re-expr (reverse expr)))
    (let loop ((iter-expr re-expr) (index 0) (move-count 0))
      (cond
          ((= index (length expr)) (reverse iter-expr))
          ((is-var? (list-ref iter-expr index)) (loop (return-rearranged-vars iter-expr index move-count) (+ 1 index) (+ 1 move-count)))
          (else (loop iter-expr (+ 1 index) move-count)) ) ) ) )

; if the expression has just one minus operation, it returns the corrected expression of (+ (-c) t)
(define (return-rearranged-with-single-minus expr)
  (if (and (= 3 (length expr)) (char=? #\- (car expr)))
    (append (list #\+) expr)
    expr ))

; function to rearrange expressions with multiple operations
(define (return-rearranged-multi-ops expr)
  (if (or (null? expr) (< (length expr) 3))
    expr
  (if (char? (car expr))
  (if (char=? #\* (car expr))
    (if (> (length expr) 3)
    (let ((times-op (list (car expr) )) (second-operand (list (list-ref expr (- (length expr) 1))) )
          (inner-op (list (list-ref expr 1))) (first-opr (sublist expr 2 (- (length expr) 2))) (second-opr (list (list-ref expr (- (length expr) 2)))) )
          (cond
            ((null? inner-op) expr)
            ((and (is-op? (car inner-op)) (is-var? (car second-operand))) (append inner-op
                                                                                times-op
                                                                                (return-rearranged-multi-ops first-opr)
                                                                                second-operand
                                                                                times-op
                                                                                second-opr
                                                                                second-operand
                                                                                ) )
            ((is-op? (car inner-op)) (append inner-op
                                             times-op
                                             second-operand
                                             (return-rearranged-multi-ops first-opr)
                                              times-op
                                              second-operand
                                              second-opr
                                              ) )
            (else expr) ) )
            expr)
    (if (char=? #\+ (car expr))
      (let ((op (list (car expr))) (first-opr (sublist expr 1 (- (length expr) 1))) (second-opr (list (list-ref expr (- (length expr) 1)))))
        (cond
          ((is-var? (car second-opr)) (append op (return-rearranged-multi-ops first-opr) second-opr) )
          (else (append op second-opr (return-rearranged-multi-ops first-opr) ) ) ))
      (if (char=? #\- (car expr))
        (let ((op (list (car expr))) (first-opr (sublist expr 1 (- (length expr) 1))) (second-opr (list (list-ref expr (- (length expr) 1)))))
          (cond
            ((is-var? (car second-opr)) (append (list #\+) op (return-rearranged-multi-ops first-opr) second-opr) )
            (else (append (list #\+) op second-opr (return-rearranged-multi-ops first-opr)) ) ) )
        expr) ) )
        expr) ) )

; function to decide if the expression needs to be rearranged or calculated
(define (rearrange-same-ops expr c)
 (if (= 1 c)
    (return-rearranged-with-single-minus (rearrange-vars (same-ops-with-vars expr (quotient (length expr) 2) 0)))
    (apply (return-op (car expr)) (get-constants (cdr expr))) ) )

; function to decide if the expression needs to be rearranged or calculated
(define (rearrange-diff-ops expr c)
  (if (= 1 c)
    (return-rearranged-multi-ops (diff-ops-with-vars expr (quotient (length expr) 2) 0 0))
    '() ) )

; function to decide which functions to call
(define (first-branch expr)
  (if (has-same-ops? expr)
    (if (has-vars? expr)
      (rearrange-same-ops expr 1)
      (rearrange-same-ops expr 2) )
    (if (has-vars? expr)
      (rearrange-diff-ops expr 1)
      (rearrange-diff-ops expr 2) ) ))

(define (pretty-print expr count or-count)
  (display " ")
    (if (list? expr)
      (if (= 4 or-count)
        (begin (display "(") (display (car expr)) (display " (") (display (list-ref expr 1)) (display (list->string (list-ref expr 2)) ) (display ") ") (display (list->string (list-ref expr 3)) ) (display " )"))
        (cond
          ;((and (= count 2) (null? expr)) (display "))") '() )
          ((null? expr) (display ")") '())
          ((= count 2) (display ")") (pretty-print expr 0 or-count))
          ((is-op? (car expr)) (display "(") (display (car expr)) (pretty-print (cdr expr) 0 or-count)  )
          (else (display (list->string (car expr))) (pretty-print (cdr expr) (+ 1 count) or-count)) ) )
    (display expr) ))

(define (convert-list x)
  (if (list? x)
    (list->string x)
    x ))

(define (expr->string expr)
  (cond
    ((null? expr) "")
    ((not (list? expr)) expr)
    ((< (length expr) 2) (string (convert-list (car expr))  ) )
    ((< (length expr) 3) (string #\( (convert-list (car expr)) #\space (convert-list(list-ref expr 1)) #\) ) )
    ((is-op? (car expr)) (string #\space #\( #\space (convert-list (car expr)) #\space (expr->string (sublist expr 1 (- (length expr) 1))) #\space (convert-list (list-ref expr (- (length expr) 1))) #\) #\space) )
    (else (string (convert-list (car expr)) #\space (expr->string (sublist expr 1 (- (length expr) 1))) #\space (convert-list (list-ref expr (- (length expr) 1)))) ) ) )

; function to evaluate the list of expressions
(define (eval-expr x)
  (display (newline))
  (cond
    ((null? x) '())
    (else
      ;(display (first-branch (car x))) (eval-expr (cdr x))) ) )
      ;(display (expr->string (first-branch (car x))))
      (let ((new-expr (first-branch (car x))))
            (if (list? new-expr)
              (pretty-print new-expr 0 (length new-expr))
              (pretty-print new-expr 0 0) ) )
                (eval-expr (cdr x)) )) )

(eval-expr stack-exp)
