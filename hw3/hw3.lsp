;
; CS161 HW3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload ()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star ()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all ()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

; Assuming index starts from 0. 
; Helper function of getKeeperPosition
; Usage: (getKeeperColumn '(1 0 4 0 4 1 3 0 1) 0) => 6
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; goal-test takes a state as input, returns t if s is a goal state, otherwise nil. 
; This function checks the existance of '2' row by row. 
; Based on assumption that each state contains at least one box, we don't check existance of '5'.
(defun goal-test (s)
  (cond 
  	;base case, this table doesnt contain '2'
  	((null s) t)
  	;recursion check this row and rest table
  	(t (and (goal-test-row (first s)) (goal-test (rest s)))))
);end defun

; Helper function of goal-test, takes a row as input, returns nil if row r contains box
; Assuming each state contains at least one box
(defun goal-test-row (r)
  (cond 
  	;base case, this row doesnt contain '2'
    ((null r) t)
    ;base case, returns nil if r contains '2'
    ((isBox (first r)) nil)
    ;recursion
    (t (goal-test-row (rest r)))))


; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 0) (try-move s 1) (try-move s 2) (try-move s 3)))
	 )
    (cleanUpList result);end
   );end let
  );


;get-square that takes in a State S, a row number r, and a column number c. 
;It should return the integer content of state S at square (r,c). 
;If the square is outside the scope of the problem, return the value of a wall.
;The main idea is extracting the table with row>=r, column>=c, we set this new table to Y. 
;Y can be table/list/nil, the coresponding operations are: extracting the top left element Z/first element Z/return 1. 
;If Z is a number with range [0, 6] it will be returned as it is. Otherwise this function returns 1.
(defun get-square (s r c)
  (cond 
  	;base case, square is out of the scope
  	((null s) 1)
  	;base case, top left element should be the integer content of (r, c), or other corner cases
    ((and (= r 0) (= c 0)) 
      (cond 
        ((and (numberp s) (>= s 0) (<= s 6)) s)
    	((atom s) 1)
    	((numberp (first s)) (get-square (first s) 0 0))
    	(t (get-square (first (first s)) 0 0))))
    ;recursion
    ((and (>= r 0) (>= c 0)) (get-square (first (nthcdr c (first (nthcdr r s)))) 0 0))
    ;all other cases are out of scope
    (t 1)))


;set-square takes in a state S, a row number r, a column number c, and a square content v (integer). 
;This function should return a new state S’ that is obtained by setting the square (r,c) to value v. 
;Similar to get-square, this function does not modify the input state.
;Another main difference is that set-square doesn't validate input. It assumes all input (s r c v) are correct.
(defun set-square (s r c v)
  (cond
  	;set row, s is a list
    ((and (= r 0) (= c 0)) 
      (cond 
        ((null s) nil)
        ;s is a list
        ((numberp (first s)) (cons v (rest s)))
        ;s is a table
      	(t (cons (set-square (first s) 0 0 v) (rest s)))))
    ;extract first row until c = 0, which is the base case above
   	((= r 0) 
   	  (cond 
   	  	((null s) nil)
   	    ((numberp (first s)) (cons (first s) (set-square (rest s) 0 (- c 1) v)))
   	    (t (cons (set-square (first s) 0 c v) (rest s)))))
    (t (cons (first s) (set-square (rest s) (- r 1) c v)))))


;try-move takes in a state S and a move direction D. 
;This function should return the state that is the result of moving the keeper in state S in direction D. 
;NIL should be returned if the move is invalid (e.g., there is a wall in that direction). 
;How you represent a move direction is up to you. Remember to update the content of every square to the right value. 
;d=0, move up. d=1, move right. d=2, move down. d=3, move left.
(defun try-move (s d)
  (let* 
  	((pos (getKeeperPosition s 0))
  	(c (car pos))
 	(r (cadr pos)))
  (cond 
  	((null pos) nil)
  	((= d 0) (move-up s r c))
  	((= d 1) (move-right s r c))
  	((= d 2) (move-down s r c))
	((= d 3) (move-left s r c))
	(t nil))))

;move-up takes state s, row r, column c as inputs. Returns the new state if moving up is valid, otherwise nil. 
;Checks out of bound cases when moving around. 
;nv: next value. nr: next row. nc: next column. nnv: next next value. nnr: next next row. nnc: next next column. 
;Explanations in code. 
(defun move-up (s r c)
  (cond 
  	((= r 0) nil)
  	(t 
  	(let* 
  	  ((v (get-square s r c))
  	  (nr (- r 1))
  	  (nv (get-square s nr c)))
  	  (cond 
  	  	;up is wall.
  	  	((= nv 1) nil)
  	  	;up is blank/goal, curr square is keeper/keeper+goal. 2*2=4 possibilities. move keeper up. 
  	  	((and (isKeeper v) (= nv 0))     (set-square (set-square s r c 0) nr c 3))
  	  	((and (isKeeperStar v) (= nv 0)) (set-square (set-square s r c 4) nr c 3))
  	  	((and (isKeeper v) (= nv 4))     (set-square (set-square s r c 0) nr c 6))
  	  	((and (isKeeperStar v) (= nv 4)) (set-square (set-square s r c 4) nr c 6))
  	  	;up is box/box+goal, need helper function to check two squares above keeper square. 
  	  	((or (= nv 2) (= nv 5))  
  	  	  (let*   	
  	  	    ((nnr (- r 2))
  			(nnv (get-square s nnr c)))
  	  		(cond 
  	  		  ;out of bound --- invalid
  	  		  ((< (- r 2) 0) nil)
  	  		  ;two squares up is wall/box/box+goal --- invalid
  	  		  ((or (= nnv 1) (= nnv 2) (= nnv 5)) nil)
  	  		  ;helper function evaluates moving two squares up --- valid
   	  		  (t (move-up-helper s r c)))))
  	  	(t nil))))))

;move-up-helper takes state s, row r, column c as inputs. Returns the new state. All inputs should be validated in caller function.
;s1: move 1 step. s2: move 2 steps. s3: followup of s2. Return s3 as the final state. 
(defun move-up-helper (s r c)
  (let*
	((v (get-square s r c))
  	(nr (- r 1))
  	(nv (get-square s nr c))
  	(nnr (- r 2))
  	(nnv (get-square s nnr c))
  	;s1: move 1 step up, update (r, c)
	(s1 (if (isKeeper v) (set-square s r c 0)    (set-square s r c 4)))
	;s2: move 2 steps up, update (r-1, c)
	(s2 (if (= nv 2) 	 (set-square s1 nr c 3)  (set-square s1 nr c 6)))
	;s3: update (r-2, c)
	(s3 (if (= nnv 0) 	 (set-square s2 nnr c 2) (set-square s2 nnr c 5))))
    s3))
	
;move-down similar algorithm to move-up.
(defun move-down (s r c)
  (cond 
  	((= r (- (length s) 1)) nil)
  	(t 
  	(let* 
  	  ((v (get-square s r c))
  	  (nr (+ r 1))
  	  (nv (get-square s nr c)))
  	  (cond 
  	  	((= nv 1) nil)
  	  	((and (isKeeper v) (= nv 0))     (set-square (set-square s r c 0) nr c 3))
  	  	((and (isKeeperStar v) (= nv 0)) (set-square (set-square s r c 4) nr c 3))
  	  	((and (isKeeper v) (= nv 4))     (set-square (set-square s r c 0) nr c 6))
  	  	((and (isKeeperStar v) (= nv 4)) (set-square (set-square s r c 4) nr c 6))
  	  	((or (= nv 2) (= nv 5))  
  	  	  (let*   	
  	  	    ((nnr (+ r 2))
  			(nnv (get-square s nnr c)))
  	  		(cond 
  	  		  ((>= (+ r 3) (length s)) nil)
  	  		  ((or (= nnv 1) (= nnv 2) (= nnv 5)) nil)
  	  		  (t (move-down-helper s r c)))))
  	  	(t nil))))))

;move-down-helper similar algorithm to move-up-helper
(defun move-down-helper (s r c)
  (let*
	((v (get-square s r c))
  	(nr (+ r 1))
  	(nv (get-square s nr c))
  	(nnr (+ r 2))
  	(nnv (get-square s nnr c))
	(s1 (if (isKeeper v) (set-square s r c 0)    (set-square s r c 4)))
	(s2 (if (= nv 2) 	 (set-square s1 nr c 3)  (set-square s1 nr c 6)))
	(s3 (if (= nnv 0) 	 (set-square s2 nnr c 2) (set-square s2 nnr c 5))))
    s3))

;move-left similar algorithm to move-up.
(defun move-left (s r c)
  (cond 
  	((= c 0) nil)
  	(t 
  	(let* 
  	  ((v (get-square s r c))
  	  (nc (- c 1))
  	  (nv (get-square s r nc)))
  	  (cond 
  	  	((= nv 1) nil)
  	  	((and (isKeeper v) (= nv 0))     (set-square (set-square s r c 0) r nc 3))
  	  	((and (isKeeperStar v) (= nv 0)) (set-square (set-square s r c 4) r nc 3))
  	  	((and (isKeeper v) (= nv 4))     (set-square (set-square s r c 0) r nc 6))
  	  	((and (isKeeperStar v) (= nv 4)) (set-square (set-square s r c 4) r nc 6))
  	  	((or (= nv 2) (= nv 5))  
  	  	  (let*   	
  	  	    ((nnc (- c 2))
  			(nnv (get-square s r nnc)))
  	  		(cond 
  	  		  ((< nnc 0) nil)
  	  		  ((or (= nnv 1) (= nnv 2) (= nnv 5)) nil)
  	  		  (t (move-left-helper s r c)))))
  	  	(t nil))))))

;move-left-helper similar algorithm to move-up-helper
(defun move-left-helper (s r c)
  (let*
	((v (get-square s r c))
  	(nc (- c 1))
  	(nv (get-square s r nc))
  	(nnc (- c 2))
  	(nnv (get-square s r nnc))
	(s1 (if (isKeeper v) (set-square s r c 0)    (set-square s r c 4)))
	(s2 (if (= nv 2) 	 (set-square s1 r nc 3)  (set-square s1 r nc 6)))
	(s3 (if (= nnv 0) 	 (set-square s2 r nnc 2) (set-square s2 r nnc 5))))
    s3))

;move-right similar algorithm to move-up.
(defun move-right (s r c)
  (cond 
  	((= c (- (length (first s)) 1)) nil)
  	(t 
  	(let* 
  	  ((v (get-square s r c))
  	  (nc (+ c 1))
  	  (nv (get-square s r nc)))
  	  (cond 
  	  	((= nv 1) nil)
  	  	((and (isKeeper v) (= nv 0))     (set-square (set-square s r c 0) r nc 3))
  	  	((and (isKeeperStar v) (= nv 0)) (set-square (set-square s r c 4) r nc 3))
  	  	((and (isKeeper v) (= nv 4))     (set-square (set-square s r c 0) r nc 6))
  	  	((and (isKeeperStar v) (= nv 4)) (set-square (set-square s r c 4) r nc 6))
  	  	((or (= nv 2) (= nv 5))  
  	  	  (let*   	
  	  	    ((nnc (+ c 2))
  			(nnv (get-square s r nnc)))
  	  		(cond 
  	  		  ((>= (+ c 3) (length (first s))) nil)
  	  		  ((or (= nnv 1) (= nnv 2) (= nnv 5)) nil)
  	  		  (t (move-right-helper s r c)))))
  	  	(t nil))))))

;move-right-helper similar algorithm to move-up-helper
(defun move-right-helper (s r c)
  (let*
	((v (get-square s r c))
  	(nc (+ c 1))
  	(nv (get-square s r nc))
  	(nnc (+ c 2))
  	(nnv (get-square s r nnc))
	(s1 (if (isKeeper v) (set-square s r c 0)    (set-square s r c 4)))
	(s2 (if (= nv 2) 	 (set-square s1 r nc 3)  (set-square s1 r nc 6)))
	(s3 (if (= nnv 0) 	 (set-square s2 r nnc 2) (set-square s2 r nnc 5))))
    s3))

; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
; h0 takes a state s as input, returns the constant 0. 
(defun h0 (s)
 '0)

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
; h1 takes a state s as input, returns the number of boxes which are not on goal positions in the given state.
; That is, h1 counts '2' in table s by recursively calling (first s) and counting '2' in (first s)
; h1 is admissible heuristic, Reasoning as following: there are more goals (G) than boxes (B), to place those boxes that are not on goal positions (BX),  
; BX will always be lower than or equal to the steps to reach the goal state, which is the cost from curr state to goal state. 
(defun h1 (s)
  (cond 
  	((null s) 0)
  	(t (+ (count 2 (first s)) (h1 (rest s))))))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;
(defun h204777289 (s)
	(if (null s) 0)
	(let* 
  		((pos (getKeeperPosition s 0))
  		(c (car pos))
 		(r (cadr pos))
 		(boxList (getBoxAsList s))
 		(boxLen (length boxList)))
 		(cond 
 			((null pos) 0)
 			((null c) 0)
 			((null r) 0)
 			((= boxLen 0) 0)
 			(t (+ (minDistKeeperBox pos boxList)
 				  (sumDistGoalBox (getGoalAsList s) boxList))))))

;Takes a state s, a list of goal squares (including goal with keeper, goal with box) goalList, a list of boxes boxList as input. 
;Returns the sum of manhattan distance from each box to nearest goal. (multiple boxes aiming at the same goal is not adjusted)
;Usage: (sumDistGoalBox (getGoalAsList p6) (getBoxAsList p6))
(defun sumDistGoalBox (goalList boxList)
	(cond 
		((null boxList) 0)
		((= (length boxList) 1) (minDistKeeperBox (first boxList) goalList))
		(t (+ (minDistKeeperBox (first boxList) goalList) 
			(sumDistGoalBox goalList (rest boxList))))))
			
;Takes a state s, a keeper position keeperPos, a list of boxes boxList as input. 
;Returns the manhattan distance from keeper to nearest box.
;Usage: (minDistKeeperBox (getKeeperPosition p11 0) (getBoxAsList p11))
(defun minDistKeeperBox (keeperPos boxList)
	(cond 
		((null boxList) 0)
		((= (length boxList) 1) (manhattanDistance (first boxList) keeperPos))
		(t (let* 
			((curr (manhattanDistance keeperPos (first boxList)))
			(minVal (minDistKeeperBox keeperPos (rest boxList))))
			(if (< curr minVal) curr minVal)))))
				
;Output is a list of boxes ('2' only), boxes on the goal square are not counted.
;Example: ((2 3) (3 3))
(defun getBoxAsList (s)
	(let* 
		((pos (getBoxPosition s 0))
  		(c (car pos))
 		(r (cadr pos)))
		(cond 
			((null pos) nil)
			(t (cons pos (getBoxAsList (set-square s r c 0)))))))

;Output is a list of goals ('4' or '6')
(defun getGoalAsList (s)
	(let* 
		((pos (getGoalPosition s 0))
  		(c (car pos))
 		(r (cadr pos)))
		(cond 
			((null pos) nil)
			(t (cons pos (getGoalAsList (set-square s r c 0)))))))

;Calculate manhattan distance. pos=(c, r).
;output = |pos1.c - pos2.c| + |pos1.r - pos2.r|
(defun manhattanDistance (pos1 pos2)
	(let* 
		((x1 (car pos1))
		(y1 (cadr pos1))
		(x2 (car pos2))
		(y2 (cadr pos2))
		(d1 (if (> x1 x2) (- x1 x2) (- x2 x1)))
		(d2 (if (> y1 y2) (- y1 y2) (- y2 y1))))
		(+ d1 d2)))

; Assuming index starts from 0. 
; Helper function of getBoxPosition
; Usage: (getBoxColumn '(1 0 4 0 4 1 3 0 1) 0) => 6
(defun getBoxColumn (r col)
  (cond ((null r) nil)
	(t (if (isBox (car r)) 
	     col
	     (getBoxColumn (cdr r) (+ col 1))
	   );end if
	);end t
  );end cond
)

; getBoxPosition (s firstRow)
; Returns a list indicating the position of the box (c r).
; 
; Assumes that the box is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getBoxPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getBoxColumn (car s) 0)))
     (if x
	 ;keeper is in this row
	 (list x row)
	 ;otherwise move on
	 (getBoxPosition (cdr s) (+ row 1))
	 );end if
    );end let
    );end t
  );end cond
);end defun

; Assuming index starts from 0. 
; Helper function of getGoalPosition
; Usage: (getGoalColumn '(1 0 4 0 4 1 3 0 1) 0) => 6
(defun getGoalColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isStar (car r)) (isBoxStar (car r)) (isKeeperStar (car r)))
	     col
	     (getGoalColumn (cdr r) (+ col 1))
	   );end if
	);end t
  );end cond
)

; getGoalPosition (s firstRow)
; Returns a list indicating the position of the goal (c r).
; 
; Assumes that the goal is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getGoalPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getGoalColumn (car s) 0)))
     (if x
	 ;keeper is in this row
	 (list x row)
	 ;otherwise move on
	 (getGoalPosition (cdr s) (+ row 1))
	 );end if
    );end let
    );end t
  );end cond
);end defun






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
