(define (sum tree) ; returns the sum of leaves in tree.
        (if (eqv? (car tree) 'node)
            (+ (sum (car(cdr tree))) (sum (car(cdr(cdr tree))))) ; if tree is a node, call sum on node's children.
            (car(cdr tree)) ; (else)if tree is a leaf, return its value.
          ))

(define (inner_product vec_a vec_b) ; used in matrix multiplication
        (if (eqv? vec_a '())
            0
            (+ (* (car vec_a) (car vec_b))
               (inner_product (cdr vec_a) (cdr vec_b))
               )
            )
  )

(define (add_vector vec_a vec_b) ; vector + vector
        (if (eqv? (cdr vec_a) '())
            (cons (+ (car vec_a) (car vec_b)) '())
            (cons (+ (car vec_a)(car vec_b))
              (add_vector (cdr vec_a)(cdr vec_b))
              )
        )
  )

(define (scaler_vector scaler vec_a) ; scaler * vector
        (if (eqv? (cdr vec_a) '())
            (cons (* scaler (car vec_a)) '())
            (cons (* scaler (car vec_a))
              (scaler_vector scaler (cdr vec_a))
              
             )
         )
  )


(display (scaler_vector (car '(2 3 4)) '(2 3 4 5)))
;(display (add_vector '(1 2 3 3) '(1 3 2 3)))
;(display (sum '(node (node (leaf 1) (leaf 2)) (leaf 2)))) ; q1

