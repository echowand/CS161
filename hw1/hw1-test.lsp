(load "hw1.lsp")

;put this file in the same directory as hw1.lsp and (load "hw1-test.lsp")
;use (functionName-TEST) to run the test for a specific function, no parameters needed
;use (TEST-ALL) to run test on all functions
;if all cases passes it should return t, otherwise the index number of the first failed test case

(defun TREE-CONTAINS-TEST ()
	(cond
		((TREE-CONTAINS 3 nil) 1)
		((not (TREE-CONTAINS 3 3)) nil)
		((not (TREE-CONTAINS 1 '((1 2 3) 7 8))) 2)
		((not (TREE-CONTAINS 7 '((1 2 3) 7 8))) 3)
		((not (TREE-CONTAINS 1 '((1 2 3) 7 8))) 4)
		((TREE-CONTAINS 4 '(1 2 3)) 5)
		((TREE-CONTAINS 4 '((1 2 3) 5 (6 8 (9 10 (11 12 13))))) 6)
		((not (TREE-CONTAINS 9 '((1 2 3) 5 (6 8 (9 10 (11 12 13)))))) 7)
		((not (TREE-CONTAINS 12 '((1 2 3) 5 (6 8 (9 10 (11 12 13)))))) 8)
		(t t)
	)
)

(defun TREE-MAX-TEST ()
	(cond
		((TREE-MAX nil) 1)
		((not (= (TREE-MAX 3) 3)) 2)
		((not (= (TREE-MAX  '((1 2 3) 7 8)) 8)) 3)
		((not (= (TREE-MAX ' ((1 2 3) 5 (6 8 (9 10 (11 12 13))))) 13)) 4)
		(t t)
	)
)

(defun TREE-ORDER-TEST ()
	(cond
		((TREE-ORDER nil) 1)
		((not (equal (TREE-ORDER 3) '(3))) 2)
		((not (equal (TREE-ORDER '((1 2 3) 7 8)) '(1 2 3 7 8))) 3)
		((not (equal (TREE-ORDER '((1 2 3) 5 (6 8 (9 10 (11 12 13))))) '(1 2 3 5 6 8 9 10 11 12 13))) 4)
		((not (equal (TREE-ORDER '(1 2 3)) '(1 2 3))) 5)
		(t t)
	)
)

(defun SUB-LIST-TEST ()
	(cond
		((SUB-LIST nil 5 5) 1)
		((SUB-LIST nil 0 5) 2)
		((SUB-LIST nil 3 0) 3)
		((not (equal (SUB-LIST '(a b c d) 0 3) '(a b c))) 4)
		((not (equal (SUB-LIST '(a b c d) 3 1) '(d))) 5)
		((not (equal (SUB-LIST '(a b c d) 2 0) nil)) 6)
		((not (equal (SUB-LIST '(a) 0 1) '(a))) 7)
		((not (equal (SUB-LIST '(a b c) 0 3) '(a b c))) 8)
		((not (equal (SUB-LIST '(a b c) 1 2) '(b c))) 9)
		(t t)
	)
)

(defun SPLIT-LIST-TEST ()
	(cond 
		((not (equal (SPLIT-LIST '(a b c d e f g)) '((A B C) (D E F G)) )) 1)
		((not (equal (SPLIT-LIST '(a b)) '((A) (B)) )) 2)
		((not (equal (SPLIT-LIST '(a b c)) '((A) (B C)) )) 3)
		((not (equal (SPLIT-LIST '(a b (c d))) '((A) (B (C D))) )) 4)
		((not (equal (SPLIT-LIST '((a b) c d)) '(((A B)) (C D)) )) 5)
		((not (equal (SPLIT-LIST '(a b (c d) f)) '((A B) ((C D) F)) )) 6)
		(t t)
	)
)

(defun BTREE-HEIGHT-TEST ()
	(cond 
		((not (equal (BTREE-HEIGHT 1) 0)) 1)
		((not (equal (BTREE-HEIGHT '(1 2)) 1)) 2)
		((not (equal (BTREE-HEIGHT '(1 (2 3))) 2)) 3)
		((not (equal (BTREE-HEIGHT '((1 2) (3 4))) 2)) 4)
		((not (equal (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) 3)) 5)
		((not (equal (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) 3)) 6)
		(t t)
	)
)

(defun LIST2BTREE-TEST ()
	(cond 
		((not (equal (LIST2BTREE '(1)) 1)) 1)
		((not (equal (LIST2BTREE '(1 2)) '(1 2))) 2)
		((not (equal (LIST2BTREE '(1 2 3)) '(1 (2 3)))) 3)
		((not (equal (LIST2BTREE '(1 2 3 4)) '((1 2) (3 4)))) 4)
		((not (equal (LIST2BTREE '(1 2 3 4 5 6 7)) '((1 (2 3)) ((4 5) (6 7))))) 5)
		((not (equal (LIST2BTREE '(1 2 3 4 5 6 7 8)) '(((1 2) (3 4)) ((5 6) (7 8))))) 6)
		(t t)
	)
)

(defun BTREE2LIST-TEST ()
	(cond 
		((not (equal (BTREE2LIST '(1)) '(1))) 1)
		((not (equal (BTREE2LIST '(1 2)) '(1 2))) 2)
		((not (equal (BTREE2LIST '(1 (2 3))) '(1 2 3))) 3)
		((not (equal (BTREE2LIST '((1 2) 3)) '(1 2 3))) 4)
		((not (equal (BTREE2LIST '((1 (2 3)) ((4 5) (6 7)))) '(1 2 3 4 5 6 7))) 5)
		((not (equal (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) '(1 2 3 4 5 6 7 8))) 6)
		(t t)
	)
)

(defun IS-SAME-TEST ()
	(cond
		((not (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8))) 1)
		((IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)) 2)
		((IS-SAME '(1 2 3 7 8) '(1 2 3 7 8)) 3)
		((IS-SAME '(1) '(2)) 4)
		((not (IS-SAME '((1 2 3) 7 (8 9)) '((1 2 3) 7 (8 9)))) 5)
		((not (IS-SAME '((1 2 3)) '((1 2 3)))) 6)
		(t t)
	)
)

(defun TEST-ALL ()
	(cond 
		((not (TREE-CONTAINS-TEST)) 1)
		((not (TREE-MAX-TEST)) 2)
		((not (TREE-ORDER-TEST)) 3)
		((not (SUB-LIST-TEST )) 4)
		((not (SPLIT-LIST-TEST)) 5)
		((not (BTREE-HEIGHT-TEST)) 6)
		((not (LIST2BTREE-TEST)) 7)
		((not (BTREE2LIST-TEST)) 8)
		((not (IS-SAME-TEST)) 9)
		(t t)
	)
)


