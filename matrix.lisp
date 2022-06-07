(defvar _0 #((1) (0)))
(defvar _1 #((0) (1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Generates and returns the Haramard Matrix
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun H_matrix ()
    (setf matrix (make-array '(2 2)
            :initial-contents '((1 1) (1 -1)))
            )
    (dotimes (row 2)
        (dotimes (col 2)
            (setf (aref matrix row col) (* (/ 1 2) (aref matrix row col)))
        )
    )
    matrix
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Retruns the transpose of the 2D-matrix passed to it
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Transpose(state)
    (print state)
    (setq dim (array-dimensions state))
    (print dim)
    (setf matrix (make-array dim))

    (dotimes (row (elt dim 0))
        (dotimes (col (elt dim 1))
            (setf(aref matrix row col) (aref state col row)))
    )

    matrix
)


; ----- Testing code here ------
(terpri)
(print _0)
(print _1)
(print (H_matrix))
(print (Transpose #2A((1 2) (4 5))))