;;;***********************************************************
;;;CONVERT TO PLINES PROGRAM WITH FULL COMMENTS!
;;;Convert all objects: Line, Pline, Spline, Arc, Circle, Ellipse_
;;;to Plines. Length of 1 segment is specified by user
;;;Copy & Paste to Notepad, Saveas *.lsp, Appload then Type C2P to run
;;;Happy New Year 2008!
;;;Written by ssg - January 2008 - www.cadviet.com
;;;***********************************************************

;;;-------------------------------------------------------------
(defun makepl ( e d1 / ps pe d d2 p2) ;;;Make pline along curve e. Length of 1 segment = d1
(vl-load-com) ;;;Load Visual LISP extensions before use vlax-xxxx functions
(setq
ps (vlax-curve-getStartPoint e) ;;;Start point
pe (vlax-curve-getEndPoint e) ;;;End point
d (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)) ;;;Length of curve e
d2 d1 ;;;Init variable distance
)
(command "pline") ;;;Call pline command
(command ps) ;;;Start point
(while (<= d2 d) ;;;While not over end point pe
(setq p2 (vlax-curve-getPointAtDist e d2)) ;;;Variable point at d2 = length along curve
(command p2) ;;;Continue pline command from current point to p2
(setq d2 (+ d2 d1)) ;;;Increase distance d2 by d1
) ;;;End while
(command pe "") ;;;Pline to pe and finish command
)
;;;-------------------------------------------------------------
(defun C:C2P( / d1 ss oldos i e ans) ;;;Convert to Plines
(if (not d0) (setq d0 0.5)) ;;;Init dividual distance, global variable
(setq d1 (getreal (strcat "\nLength of 1 segment <" (rtos d0) ">:"))) ;;;Input distance
(if d1 (setq d0 d1) (setq d1 d0)) ;;;Reset or get distance
(setq
ss (ssget '((0 . "LINE,LWPOLYLINE,SPLINE,ARC,CIRCLE,ELLIPSE"))) ;;;Selection set
oldos (getvar "osmode") ;;;Save osmode
i 0 ;;;Init counter
)
(setvar "osmode" 0) ;;;Disable osmode
(repeat (sslength ss) ;;;Repeat for all entities in ss
(setq e (ssname ss i)) ;;;Set e for entity with ordinal i in selection set ss
(makepl e d1) ;;;Use makepl function. Make pline along e
(setq i (1+ i)) ;;;Increase counter
)
(initget "Y N") ;;;Init keywords
(setq ans (getkword "\nDelete source objects? [Yes/No] <N>:")) ;;;Get answer from user
(if (= ans "Y") (command "erase" ss "")) ;;;Erase source objects if ans = "y" or "Y"
(setvar "osmode" oldos) ;;;Reset osmode
(princ) ;;;Silent quit
)
;;;-------------------------------------------------------------

 
