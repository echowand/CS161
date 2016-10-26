;;1. Write a single Boolean LISP function, called TREE-CONTAINS, which takes two arguments N and
;;TREE, and checks whether number N appears in the ordered tree TREE, return T if TREE contains N, otherwise NIL. 

(defun TREE-CONTAINS (N TREE)
  ;special case:
  (if (null N) nil)
  (cond
    ;base case: TREE==null
    ((null TREE) nil)
    ;base case: TREE is a number
    ((atom TREE) (= TREE N))
    ;recursion: N < TREE.second
    ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
    ;recursion: N > TREE.second
    ((> N (second TREE)) (TREE-CONTAINS N (third TREE)))
    ;recursion: else
    (t (TREE-CONTAINS N (second TREE)))))


;;2. Write a single LISP function, called TREE-MAX, which takes one argument TREE, and returns the maximum number appearing in the ordered tree TREE.

(defun TREE-MAX (TREE)
  ;special case:
  (if (null TREE) nil)
  (cond
    ;base case: 
    ((atom TREE) TREE)
    (t (TREE-MAX (third TREE)))))


;;3. Write a single LISP function, called TREE-ORDER, which takes one argument TREE, and returns an in-ordered list of the numbers appearing in the ordered tree TREE.

(defun TREE-ORDER (TREE)
  (cond 
    ((atom TREE) (list TREE))
    (t (append (TREE-ORDER (first TREE)) (list (second TREE)) (TREE-ORDER (third TREE))))))


;;4. Write a single LISP function, called SUB-LIST, that takes a list L and two non-negative integers START and LEN, and returns the sub-list of L starting at position START and having length LEN. Assume that the first element of L has position 0.

(defun SUB-LIST (L START LEN)
  (if (null L) nil)
  (cond
    ((or (= LEN 0) (null L)) nil)
    ((= START 0) (cons (first L) (SUB-LIST (rest L) 0 (- LEN 1))))    
    (t (SUB-LIST (rest L) (- START 1) LEN))))


;;5. Write a single LISP function, called SPLIT-LIST that takes a list L, and returns a list of two lists L1 and L2.

(defun SPLIT-LIST (L)
  (if (<= (length L) 1) L)
  (let* ((len (length L)))
    (if (evenp len) (list (SUB-LIST L 0 (/ len 2)) (SUB-LIST L (/ len 2) len))
      (list (SUB-LIST L 0 (/ (- len 1) 2)) (SUB-LIST L (/ (- len 1) 2) len)))))

      
;;6. Write a single LISP function, called BTREE-HEIGHT, which takes a binary tree TREE, and returns the height of TREE. Note that the height of a binary tree is defined as the length of the longest path from the root node to the farthest leaf node.

(defun BTREE-HEIGHT (TREE)
  (cond
    ((atom TREE) 0)
    ((> (BTREE-HEIGHT (first TREE)) (BTREE-HEIGHT (second TREE))) (+ (BTREE-HEIGHT (first TREE) 1)))
    (t (+ (BTREE-HEIGHT (second TREE)) 1))))


;;7. Write a single LISP function, called LIST2BTREE that takes a non-empty list of atoms LEAVES, and returns a binary tree.

(defun LIST2BTREE (LEAVES) 
  (cond 
    ((= (length LEAVES) 1) (first LEAVES))
    (t (let* ((lists (SPLIT-LIST LEAVES)))
         (list (LIST2BTREE (first lists)) (LIST2BTREE (second lists)))))))
    

;;8. Write a single LISP function, called BTREE2LIST, that takes a binary tree TREE, and returns a list of
;; atoms (assume TREE follows the constraints we defined earlier).

(defun BTREE2LIST (TREE)
  (cond 
    ((atom TREE) (list TREE))
    (t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE)))))) 


;;9. Write a single Boolean LISP function, called IS-SAME, that takes two LISP expressions E1 and E2 whose atoms are all numbers, and checks whether the expressions are identical. In this question, you can only use ‘=‘ to test equality (you cannot use ‘equal’). Recall that a LISP expression is either an atom or a list of LISP expressions.

(defun IS-SAME (E1 E2)
  (cond
    ((and (null E1) (null E2)))
    ((or (null E1) (null E2)) nil)
    ((and (atom E1) (atom E2)) (= E1 E2))
    ((and (listp E1) (listp E2)) (and (IS-SAME (first E1) (first E2)) (IS-SAME (rest E1) (rest E2))))
    (t nil)))
    




