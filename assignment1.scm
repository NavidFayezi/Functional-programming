(define (sum tree) ; returns the sum of leaves in tree.
        (if (eqv? (car tree) 'node)
            (+ (sum (car(cdr tree))) (sum (car(cdr(cdr tree))))) ; if tree is a node, call sum on node's children.
            (car(cdr tree)) ; (else)if tree is a leaf, return its value.
          ))

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


(define matrix_a '((0 1 2 3 4) (5 6 7 8 9) (1 2 3 4 5)))
(define matrix_b '((1 2) (2 3) (4 5)(6 7) (8 9)))

(display (sum '(node (node (leaf 1) (leaf 2)) (leaf 2)))) ; q1
(display "\n")
(display (matrix_multiplication matrix_a matrix_b)) ;q2

