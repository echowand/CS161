;;Author: Guanqun Mao
;;Date: 10/24/2016

(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
;(defun solve-cnf (filename)
;  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

(defun reload ()
  (load "hw4.lsp"))

;top-level function where n is an integer and delta is a CNF defined over n variables. 
(defun sat? (n delta)
  (cond 
    ((= 0 (length delta)) nil)
    (t (sat?helper n delta nil))))

;sat? helper function, assign is the set of curr assigned vars
;the initial call to sat?helper should always set assign to nil
;(sat?helper 9 '((-1 -2 -3 7)) '())
(defun sat?helper (n delta assign)
  (if (is-complete n assign) 
    assign
    (let* 
      ((selectedVar (select-var1 n delta assign))
      (newAssign (append assign (list selectedVar)))
      (newAssignNeg (append assign (list (- 0 selectedVar)))))
      
      ;(format t "-----------------------" )
      ;(format t "selectedVar: ~A~%" selectedVar)
      ;(format t "newAssign: ~A~%" newAssign)
      ;(format t "newAssignNeg: ~A~%" newAssignNeg)
      (if (or (< selectedVar 1) (> selectedVar n)) nil) 

      (if (eval-delta newAssign delta)
        (let* ((res (sat?helper n delta newAssign)))
          (if (not (null res))
            res
            (if (eval-delta newAssignNeg delta)
              (sat?helper n delta newAssignNeg)
              nil)))
        (if (eval-delta newAssignNeg delta)
          (sat?helper n delta newAssignNeg)
          nil))
      )))

;removes a clause if it's T, returns a new delta in which every clause is unknown at that point
(defun reduce-delta (delta assign)
  (let* ((head (first delta)))
    (cond 
      ((null delta) nil)
      ((null (reduce-clause head assign)) 
        (cons head (reduce-delta (rest delta) assign)))
      (t 
        (reduce-delta (rest delta) assign))
      )))

;returns T if the clause contains any assigned var in assign
(defun reduce-clause (clause assign)
  (let* ((literal (first assign)))
    (cond 
      ((null literal) nil)
      ((> (count literal clause) 0) t)
      (t (reduce-clause clause (rest assign))))))

;find single element clause
;trim each clause in delta, remove T clauses. 
;for all other clauses, remove the literals whoes negative exists in assign. 
;(find-single-clause '((-1 2 3) (1) (2 -3) (2 3 8) (-3 10)) '(1 2 3)) => (10)
;10 has less degree heuristic so choose 10. 
;actually further optimization should set 10 to positive, tree should stop growing when -10. Unfortunately this is not adjusted in this code.  
(defun find-single-clause (delta assign)
  (let* ((head (trim-neg-clause (first delta) assign)))
    (cond 
      ((null delta) nil)
      ((= 1 (length head)) head)
      (t (find-single-clause (rest delta) assign)))))

;for non-reduce clauses, remove leterals that exists in assign
;(trim-neg-clause '(2 -3 10) '(-1 -2 3)) => (10)
(defun trim-neg-clause (clause assign)
  (let* ((literal (first clause)))
    (cond
      ((null clause) nil)
      ((reduce-clause clause assign) nil)
      ((> (count (- 0 literal) assign) 0) (trim-neg-clause (rest clause) assign))
      (t (cons literal (trim-neg-clause (rest clause) assign))))))

;optimization. 
;find single caluse first and choose that var.
;if not found select var in order from 1 to n.
(defun select-var1 (n delta assign)
  (cond
    ((null assign) 1)
    (t 
      (let* ((singleClause (find-single-clause delta assign)))
        (cond
          ((not (null singleClause)) (first singleClause))
          (t (select-var-in-order n assign 1))
          )))))

;select var in order from 1 to n (var doesnt exist in assign)
(defun select-var-in-order (n assign index)
  (cond 
    ((= n (length assign)) 0)
    ((> (count index (abs-list assign)) 0) (select-var-in-order n assign (+ index 1)))
    (t index)))

;returns the absolute value of this list
(defun abs-list (assign)
  (let* ((element (first assign)))
    (cond
      ((null assign) nil)
      ((>= element 0) (cons element (abs-list (rest assign))))
      (t (cons (- 0 element) (abs-list (rest assign)))))))

;select next var based on n, delta and curr assign
;will optimize later 
(defun select-var (n delta assign)
  (cond 
    ((null assign) 1)
    (t (+ (abs-value (first (last assign))) 1))))

;assumes vars always ranges from 1 to n, returns T is assign size is same as n
(defun is-complete (n assign)
  (cond 
    ((null assign) nil)
    ((>= (length assign) n) t)
    (t nil)))

;returns absolute value of a number num
(defun abs-value (num)
  (cond 
    ((>= num 0) num)
    (t (- 0 num))))

;eval-delta takes input of a list of assigned vars as assign, CNS as delta
;it evaluates if the CNF is valid with the curr assignment.  
;(eval-delta '(-1 2 3) '((1 -2 3) (-1) (2 3))) => T
(defun eval-delta (assign delta)
  (let* ((head (first delta)))
    (cond 
      ((null head) t)
      (t (and (eval-clause-result assign (first delta)) (eval-delta assign (rest delta)))))))

;evalute each clause, returns NIL only if all of the literals's negative exist in assign
;(eval-clause-result '(-1) '(1 -2 3 4)) => T
;assignment of 2 3 4 are unknown, this function returns T in such cases
(defun eval-clause-result (assign clause)
  (let* 
    ((result (eval-clause assign clause))
    (countneg1 (count -1 result)))
    (cond 
      ((= countneg1 (length result)) nil)
      (t t))))

;returns a list of 1, 0, -1, meaning T, unknown, Nil respectively
;(1 2 3 5) (-1 -2 3) => (-1 -1 1)
(defun eval-clause (assign clause)
  (let* ((head (first clause)))
    (cond 
      ((null clause) nil)
      ((= 1 (length clause)) (eval-literal assign head))
      (t (append (eval-literal assign head) (eval-clause assign (rest clause)))))))

;evalute each literal, returns a list of single number: 1, 0, -1, meaning T, unknown, Nil respectively
;(1 2 3 5) (-1) => (-1)
(defun eval-literal (assign literal)
  (cond 
    ((> (count literal assign) 0) (list 1))
    ((> (count (- 0 literal) assign) 0) (list -1))
    (t (list 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;below are testing and debug;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq x (parse-cnf "./cnfs/sat/cnf_10.cnf"))
(setq y (parse-cnf "./cnfs/sat/cnf_20.cnf"))
(setq z (parse-cnf "./cnfs/sat/cnf_30.cnf"))
(setq k (parse-cnf "./cnfs/sat/cnf_50.cnf"))
(setq a (parse-cnf "./cnfs/unsat/cnf_12.cnf"))
(setq b (parse-cnf "./cnfs/unsat/cnf_20.cnf"))
(setq c (parse-cnf "./cnfs/unsat/cnf_30.cnf"))
(setq dd (parse-cnf "./cnfs/unsat/cnf_42.cnf"))
;(sat? (first x) (second x))

;(debug-result x (sat? (first x) (second x)))
;(debug-result k (sat? (first k) (second k)))
(defun debug-result (cnfs assign)
  (format t "~~~~~A~%" assign)
  (loop for cnf in (second cnfs)
    do 
      (loop for c in cnf 
        do 
        (if (> (count c assign) 0)
          (format t "Y ~A~A" c #\tab)
          (format t "N ~A~A" c #\tab))
      )
      (format t "~%")
  )
)
