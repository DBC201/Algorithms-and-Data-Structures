;huffman tree implementation in mit-scheme 
;a node is defined as (("input" frequency) (left_child) (right_child))
;frequency is a number while left and right child can be null or other node types

(define get_left
	(lambda (node)
		(car (cdr node))
	)
)

(define get_right
	(lambda (node)
		(car (cdr (cdr node)))
	)
)

(define get_value
	(lambda (node)
		(car node)
	)
)

(define get_input
	(lambda (node)
		(car (get_value node))
	)
)

(define get_frequency
	(lambda (node)
		(car (cdr (get_value node)))
	)
)

(define smallest
	;finds the node with the smallest frequency
        (lambda (nodes node)
                (cond
                        ((null? nodes) node)
                        ((< (get_frequency (car nodes)) (get_frequency node)) (smallest (cdr nodes) (car nodes)))
                        (else (smallest (cdr nodes) node))
                )
        )
)

(define remove
	;removes the given node by frequency
        (lambda (nodes node finalnodes)
                (cond
                        ((null? nodes) finalnodes)
                        ((= (get_frequency (car nodes)) (get_frequency node)) (append finalnodes (cdr nodes)))
                        (else (remove (cdr nodes) node (append finalnodes (list (car nodes)))))
                )
        )
)

(define selection_sort
	;sorts the nodes in increasing order based on their frequency
        (lambda (nodes)
                (cond
                        ((null? nodes) '())
                        (else (cons (smallest nodes (car nodes)) (selection_sort (remove nodes (smallest nodes (car nodes)) '()))))
                )
        )
)

(define build_huffman_tree_helper
	; build a huffman tree with the given list of nodes
	(lambda (nodes)
		(cond
			((null? nodes) '())
			((null? (cdr nodes)) (car nodes))
			(else 
				(build_huffman_tree_helper 
					(selection_sort 
						(append
							(cdr (cdr nodes))
							(list 
								(list
									(list
										 "" 
										 (+ (get_frequency (car nodes)) (get_frequency (car (cdr nodes))))
									)
									(car nodes)
									(car (cdr nodes))
								)
							)
						)
					)
				)
			)
		)
	)
)

(define occurency_to_nodes
	;convert occurency list to node list
	;see occurency_set below to see an example of occurency list
	;pass initial nodes parameter as '()
        (lambda (occurencies nodes)
                (if (null? occurencies)
                        nodes
                        (occurency_to_nodes (cdr occurencies) (append nodes (list (list (car occurencies) '() '()))))
                )
        )
)

(define decode_helper
	;takes a huffman tree created by above functions and a character list
	;decodes the list to find a character
	(lambda (root input tree output)
		(cond
			((not (string=? (get_input tree) "")) (decode_helper root input root (append output (list (get_input tree)))))
			((null? input) output)
			((= (car input) 0) (decode_helper root (cdr input) (get_left tree) output))
			((= (car input) 1) (decode_helper root (cdr input) (get_right tree) output))
		)
	)
)

(define decode_huffman_tree
		;wrapper function for decode_helper
        (lambda (tree input)
                (decode_helper tree input tree '())
        )
)

(define build_huffman_tree
	;wrapper function
        (lambda (occurencies)
                (build_huffman_tree_helper (occurency_to_nodes occurencies '()))
        )
)

(define occurency_set
	;example way to show occurencies to pass into wrapper function above
        '(
                ("a" 5)
                ("b" 9)
                ("c" 12)
                ("d" 13)
                ("e" 16)
                ("f" 45)
        )
)

(define huffman_tree (build_huffman_tree occurency_set))

(define input_f '(0))
(define input_c '(1 0 0))
