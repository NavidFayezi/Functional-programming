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

(define (scaler_vector scaler vec) ; scaler * vector
        (if (eqv? (cdr vec) '())
            (cons (* scaler (car vec)) '())
            (cons (* scaler (car vec))
              (scaler_vector scaler (cdr vec))
              
             )
         )
  )

(define (sum_of_scaled_rows vec matrix) ; sigma(vec[i] * matrix[i])
        (if (eqv? (cdr vec) '())
            (scaler_vector (car vec) (car matrix))
            (add_vector (scaler_vector (car vec)(car matrix)) (sum_of_scaled_rows (cdr vec) (cdr matrix)))
         )
  )



(define (matrix_multiplication mat_a mat_b); matrix multiplication
        (if (eqv? (cdr mat_a) '())
            (cons (sum_of_scaled_rows (car mat_a) mat_b) '())
            (cons (sum_of_scaled_rows (car mat_a) mat_b) (matrix_multiplication (cdr mat_a) mat_b))
         )
  )


(define matrix_a '((1 2 3) (4 5 6) (7 8 9)))
(define matrix_b '((3 9 2) (3 5 4) (7 0 9)))

;(display (sum_of_scaled_rows '(1 1 1) matrix_a))
;(display (scaler_vector (car '(2 3 4)) '(2 3 4 5)))
;(display (add_vector '(1 2 3 3) '(1 3 2 3)))
(display (sum '(node (node (leaf 1) (leaf 2)) (leaf 2)))) ; q1
(display "\n")
(display (matrix_multiplication matrix_b matrix_a)) ;q2

