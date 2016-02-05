;
; load in the file for scheme interpretor by: (load "filename.scm")

; define a simple function
; (define (function-name arg1 arg2 ...)(function body also return value))
(define (do-something) (+ 4 4))

; define variable
(define name "Joshua Matthews")
(define age 22)


(define (greet n a)
	(begin
	 (display n)
	 (newline)
	 (display a)
	 (newline)) '())

(define v1 (list 1 2 3 4))
(define v2 (list 5 6 7 8))

(define (dot a b)
	(if (or (null? a) (null? b)) 0
	(+(* (car a) (car b)) (dot (cdr a) (cdr b))) ))

(cond ((> 3 3) 'greater)
	((< 3 3) 'less)
	((= 3 3) 'equal))

; define structure creates struct keyword-constructor allows you to use the parameter names when creating a node object
; copier creates a convienent copy function for the object
(define-structure (node keyword-constructor copier)
		  val
		  right-child
		  left-child)

(define-structure (tree keyword-constructor copier)
		  node)

(define main-tree (make-tree 'node '()))

(define (insert-node node-obj value)
	(if (null? node-obj)
		(make-node
		'val value
		'right-child '()
		'left-child '() )
		(if (= (node-val node-obj) value)
			node-obj
			(if (< (node-val node-obj) value)
				(make-node 'val (node-val node-obj)
				  'right-child (insert-node (node-right-child node-obj) value)
				  'left-child (node-left-child node-obj))
				(if (> (node-val node-obj) value)
					(make-node 'val (node-val node-obj)
						   'right-child (node-right-child node-obj)
					  	   'left-child (insert-node (node-left-child node-obj) value)) ) ) ) ) )

(define (print-tree head-obj)
	(if (not (null? head-obj))
		(begin
		  (print-tree (node-left-child head-obj))
		  (write (node-val head-obj))(newline)
		  (print-tree (node-right-child head-obj)))))




(define (delete-tree-node head-obj node-obj value)
	(if (null? node-obj)
		('())
		(if (= (node-val node-obj) value)
			()
			() ) ) )

(define (modify-tree tree-obj value)
	(make-tree 'node (insert-node (tree-node tree-obj) value)) )


(define (compose p1 p2) (lambda (x) (p1 (p2 x))) )
