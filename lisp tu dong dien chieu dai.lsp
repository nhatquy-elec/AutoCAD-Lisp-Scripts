;;;-------------------------------------------------------------
(defun TBCong (x1 x2) (/ (+ x1 x2) 2)) ;;;Trung binh cong
;;;-------------------------------------------------------------
(defun MidP (p1 p2) ;;;Midpoint
(list (TBCong (car p1) (car p2)) (TBCong (cadr p1) (cadr p2)) (TBCong (caddr p1) (caddr p2)))
)
;;;-------------------------------------------------------------
(defun getVert (e / i L) ;;;Return list of all vertex from pline e
(setq i 0 L nil)
(vl-load-com)
(repeat (fix (+ (vlax-curve-getEndParam e) 1))
(setq L (append L (list (vlax-curve-getPointAtParam e i))))
(setq i (1+ i))
)
L
)
;;;-------------------------------------------------------------
(defun etype (e) (cdr (assoc 0 (entget e)))) ;;;Entity type
;;;-------------------------------------------------------------
(defun dim2p (p1 p2 s) ;;;Dimaligned 2 Point
(command "dimaligned" p1 p2 (polar (MidP p1 p2) (+ (angle p1 p2) (/ pi 2)) s))
)
;;;-------------------------------------------------------------
(defun dimLine(e s) ;;;Dimaligned Line
(dim2p (cdr (assoc 10 (entget e))) (cdr (assoc 11 (entget e))) s)
)
;;;-------------------------------------------------------------
(defun dimPline(e s) ;;;Dimaligned PLine
(setq Lp (getvert e) i 0)
(repeat (1- (length Lp))
(dim2p (nth i Lp) (nth (1+ i) Lp) s)
(setq i (1+ i))
)
)
;;;-------------------------------------------------------------
(defun C:AD( / ss s oldos e) ;;;AutoDimaligned Line & Pline
(if (not s0) (setq s0 10))
(setq
ss (ssget '((0 . "LINE,LWPOLYLINE")))
s (getdist (strcat "\nKhoang cach tu doi tuong den duong kich thuoc <" (rtos s0) ">:"))
oldos (getvar "osmode")
)
(if (not s) (setq s s0) (setq s0 s))
(setvar "osmode" 0)
(while (setq e (ssname ss 0))
(if (= (etype e) "LINE") (dimLine e s) (dimPline e s))
(ssdel e ss)
)
(setvar "osmode" oldos)
(princ)
)
