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

(defun sat? (n delta)
  (cond 
    ((= 0 (length delta)) nil))
)


;(sat?helper 9 '((-1 -2 -3 7)) '() 1 )
(defun sat?helper (n delta assign)
  (if (is-complete n assign) 
    assign
    (let* 
      ((selectedVar (select-var n delta assign))
      (newAssign (append assign (list selectedVar)))
      (newAssignNeg (append assign (list (- 0 selectedVar)))))
      
      ;(format t "-----------------------" )
      ;(format t "selectedVar: ~A~%" selectedVar)
      ;(format t "newAssign: ~A~%" newAssign)
      ;(format t "newAssignNeg: ~A~%" newAssignNeg)
     
      (if (eval-delta newAssign delta)
        (let* ((res (sat?helper n delta newAssign)))
          (if (not (null res))
            res
            (if (eval-delta newAssignNeg delta)
              (sat?helper n delta newAssignNeg)
              nil)))
        (sat?helper n delta newAssignNeg))
      )))
          
(defun select-var (n delta assign)
  (cond 
    ((null assign) 1)
    (t (+ (abs-value (first (last assign))) 1))))

(defun is-complete (n assign)
  (cond 
    ((null assign) nil)
    ((>= (length assign) n) t)
    (t nil)))

(defun abs-value (num)
  (cond 
    ((>= num 0) num)
    (t (- 0 num))))

(defun eval-delta (assign delta)
  (let* ((head (first delta)))
    (cond 
      ((null head) t)
      (t (and (eval-clause-result assign (first delta)) (eval-delta assign (rest delta)))))))

;returns NIL only if all of the literals's negative exist in assign
(defun eval-clause-result (assign clause)
  (let* 
    ((result (eval-clause assign clause))
    (countneg1 (count -1 result)))
    (cond 
      ((= countneg1 (length result)) nil)
      (t t))))

;(1 2 3 5) (-1 -2 3)
(defun eval-clause (assign clause)
  (let* ((head (first clause)))
    (cond 
      ((null clause) nil)
      ((= 1 (length clause)) (eval-literal assign head))
      (t (append (eval-literal assign head) (eval-clause assign (rest clause)))))))

;(1 2 3 5) (-1) => (-1)
(defun eval-literal (assign literal)
  (cond 
    ((> (count literal assign) 0) (list 1))
    ((> (count (- 0 literal) assign) 0) (list -1))
    (t (list 0))))

(setq x (parse-cnf "./cnfs/sat/cnf_10.cnf"))
;(sat? (first x) (second x))