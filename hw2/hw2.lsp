; Author: Guanqun Mao
; Date: 10/11/2016

;;1. Write a single pure LISP function,called DFS,that performs a depth-first search of a tree.  
;;Thefunction should take a single argument that is the list representation of the tree, and returns a single, top-level list of the terminal nodes in the order they would be visited by a left-to-right depth-first search. 
;;For example, (dfs '((A (B)) C (D))) would return (A B C D). Do not use any auxiliary functions.
;;(DFS '((A B) C (D (E (F)))))
(defun DFS (L)
	(cond 
		;base case
		((null L) nil)
		((atom L) (list L))
    ;recursion
    (t (append (DFS (first L)) (DFS (rest L))))))


;;2. Write a set of pure LISP functions that implement depth-first iterative-deepening. 
;;Your top-level func- tion, called DFID, should take two arguments, 
;;the list representation of the tree, and an integer representing the maximum depth of the tree, 
;;and returns a single top-level list of the terminal nodes in the order that they would be visited by a left-to-right depth-first iterative-deepening search. 
;;Note that those nodes that are vis- ited in multiple iterations will appear multiple times in the output list. 
;;For example, (dfid '((A (B)) C (D)) 3) would return (C A C D A B C D).
;;(dfid '((a ((b (c)))) d (e)) 5)
;;(D A D E A D E A B D E A B C D E)
(defun DFID (L DEPTH)
  (cond
    ((= 0 DEPTH) nil)
    (t (append (DFID L (- DEPTH 1)) (DLDFS L DEPTH)))))

;;recursive depth-limited DFS (called DLS).
(defun DLDFS (L LIMIT)
  (cond 
    ((= 0 LIMIT) (if (atom L) (list L) nil))
    ((null L) nil)
    ((atom L) (list L))
    (t (append (DLDFS (first L) (- LIMIT 1)) (DLDFS (rest L) LIMIT)))))

; These functions implement a depth-first iterative-deepening solver for the
; missionary-cannibal problem. In this problem, three missionaries and three
; cannibals are trying to go from the east side of a river to the west side.
; They have a single boat that can carry two people at a time from one side of
; the river to the other. There must be at least one person in the boat to cross
; the river. There can never be more cannibals on one side of the river than
; missionaries. If there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list (MISSIONARIES
; CANNIBALS SIDE). SIDE represents which side the boat is currently on, and is T
; if it is on the east side and NIL if on the west side. MISSIONARIES and
; CANNIBALS represent the number of missionaries and cannibals on the same side
; as the boat. Thus, the initial state for this problem is (3 3 T) (three
; missionaries, three cannibals, and the boat are all on the east side of the
; river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function ID-DFS, which is called
; with the initial state to search from and the depth up to which depth-first
; search will be performed. It returns the complete path from the initial state
; to the goal state: this path is a list of intermediate problem states. The
; first element of the path is the initial state and the last element is the
; goal state. Each intermediate state is the state that results from applying
; the appropriate operator to the preceding state.

; To solve the original problem, one would call (ID-DFS '(3 3 T) 0). 

; Examples of calls to some of the helper functions can be found after the code.


; FINAL-STATE takes a single argument (S), the current state, and returns T if
; it is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (cond 
    ((and (listp s) (= (length s) 3) (= (first s) 3) (= (second s) 3) (null (third s))) t)
    (t nil)))
    

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), a number of
; missionaries to move (M), and a number of cannibals to move (C). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (if (not (listp s)) nil)
  (if (not (= (length s) 3)) nil)
  (if (> (+ m c) 2) nil)
  (let* 
    ((sm (first s)) 
    (sc (second s)) 
    (ss (third s))) 
    (cond
      ((or (> m sm) (> c sc)) nil)
      ;;s side: (sm-m)!=0 and sm-m < sc-c => nil
      ((and (not (= 0 (- sm m))) (< (- sm m) (- sc c))) nil)
      ;;opposite side: 3-sm+m!=0 and 3-sm+m <3-sc+c => nil
      ((and (not (= 0 (+ m (- 3 sm)))) (< (+ m (- 3 sm)) (+ c (- 3 sc)))) nil)
      ;;opposite side: legal state
      (t (list (list (+ m (- 3 sm)) (+ c (- 3 sc)) (not ss)))))))


; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.
(defun succ-fn (s)
  (if (not (listp s)) nil)
  (if (not (= (length s) 3)) nil)
  (append (next-state s 0 1) (next-state s 0 2) (next-state s 1 1) 
    (next-state s 1 0) (next-state s 2 0)))


; MULT-DFS is a helper function for SINGLE-DFS. It takes three arguments: the
; path from the initial state to the current state (PATH), the legal successor
; states to the last state on PATH (STATES), and the depth (DEPTH). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a single depth-first iteration to the given depth on
; each element of STATES in turn. If any of those searches reaches the final
; state, MULT-DFS returns the complete path from the initial state to the goal
; state. Otherwise, it returns NIL.
(defun mult-dfs (states path depth)
  (cond
    ((= 0 (length states)) nil)
    ((> (length path) depth) nil)
    ((final-state (first states)) (append path (list (first states))))
    ((single-dfs (first states) (append path (list (first states))) depth) 
      (single-dfs (first states) (append path (list (first states))) depth))
    (t (mult-dfs (rest states) path depth))))

; SINGLE-DFS does a single depth-first iteration to the given depth. It takes
; three arguments: a state (S), the path from the initial state to S (PATH), and
; the depth (DEPTH). If S is the initial state in our search, PATH should be
; NIL. It performs a depth-first search starting at the given state. It returns
; the path from the initial state to the goal state, if any, or NIL otherwise.
(defun single-dfs (s path depth)
  (cond 
    ((null s) nil)
    ((> (length path) (+ depth 1)) nil)
    ((final-state s) path)
    ((single-dfs (first (succ-fn s)) (append path (list (first (succ-fn s)))) depth) 
      (single-dfs (first (succ-fn s)) (append path (list (first (succ-fn s)))) depth))
    (t (mult-dfs (rest (succ-fn s)) path depth))))

; ID-DFS is the top-level function. It takes two arguments: an initial state (S)
; and a search depth (DEPTH). ID-DFS performs a series of depth-first
; iterations, starting from the given depth until a solution is found. It
; returns the path from the initial state to the goal state. The very first call
; to ID-DFS should use depth = 0.
(defun id-dfs (s depth)
  (cond 
    ((null s) nil)
    ((null (numberp depth)) nil)
    ((single-dfs s nil depth) (append (list s) (single-dfs s nil depth)))
    (t (id-dfs s (+ depth 1)))))


; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (note that NEXT-STATE
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; SUCC-FN returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
