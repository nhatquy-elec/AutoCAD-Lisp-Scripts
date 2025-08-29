;;***************Xuat khoi luong trac ngang ra Excel******************
;; CSVOUT.LSP  Copyright 2000  Tony Tanzillo  All rights reserved.
;;
;; Adds CSVOUT command to AutoCAD, which exports selected 
;; tablular text to .CSV format.
;;
(prompt "\nDe xuat khoi luong trac ngang ra Excel danh: xt")
(defun list-index (input func / i)
  (setq i -1)
  (mapcar 'cdr
	  (vl-sort
	    (mapcar
	      '(lambda (val)
		 (cons val (setq i (1+ i)))
	       )
	      input
	    )
	    '(lambda (a b)
	       (apply func (mapcar 'car (list a b)))
	     )
	  )
  )
)
(defun ss-index	(ss _getKey _compareKey / keylist i)
  (repeat (setq i (sslength ss))
    (setq keylist
	   (cons
	     (apply _getKey (list (ssname ss (setq i (1- i)))))
	     keylist
	   )
    )
  )
  (list-index keylist _compareKey)
)
(defun compare-points (p1 p2)
  (if (equal (cadr p1) (cadr p2) epsilon)
    (< (car p1) (car p2))
    (> (cadr p1) (cadr p2))
  )
)
(defun Strlcat (delim strlst)
  (apply 'strcat
	 (cons
	   (car strlst)
	   (mapcar
	     '(lambda (s)
		(strcat delim s)
	      )
	     (cdr strlst)
	   )
	 )
  )
)
(defun list->string (lst)
  (strlcat ","
	   (mapcar
	     '(lambda (s)
		(if (numberp s)
		  (rtos s)
		  (strcat (chr 34) s (chr 34))
		)
	      )
	     lst
	   )
  )
)
(defun C:xt (/	      ss       indices	oldtext	 epsilon  colcnt
	     rowcnt   count    j	e	 tmp	  y
	     table    outfile  text-point
	    )
  (prompt "\nBan co muon dong lai hang text khong <c>/(k): ")
  (setq luachon (getstring))
  (if (= "" luachon)
    (setq luachon "K")
  )
  (setq luachon (strcase luachon))
  (if (= "C" luachon)
    (cht)
  )
  (defun text-point (ename / d)
    (if	(eq 2 (cdr (assoc 72 (setq d (entget ename)))))
      (cdr (assoc 11 d))
      (cdr (assoc 10 d))
    )
  )
  (setq epsilon (/ (getvar "TEXTSIZE") 20.0))
  (setq ss (ssget '((0 . "TEXT"))))
  (if (or (not ss) (< (sslength ss) 2))
    (progn
      (alert "Must select a rectangular array of text.")
      (exit)
    )
  )
  (setq indices (ss-index ss 'text-point 'compare-points))
  (setq colcnt 0)
  (setq y (caddr (assoc 10 (entget (ssname ss (car indices))))))
  (while
    (equal
      y
      (caddr
	(assoc
	  10
	  (entget (ssname ss (nth (setq colcnt (1+ colcnt)) indices)))
	)
      )
      epsilon
    )
  )
  (if (zerop colcnt)
    (progn (alert "\nNo columns detected.") (exit))
  )
  (setq count (length indices))

  (if (not (zerop (rem count colcnt)))
    (progn
      (alert
	(strcat
	  "\nTotal number of text items ("
	  (itoa count)
	  ") must be an even "
	  "\nmultiple of the number of detected columns ("
	  (itoa colcnt)
	  ")"
	)
      )
      (exit)
    )
  )

  (princ
    (strcat
      "\nDetected table of "
      (itoa count)
      " items ("
      (itoa colcnt)
      " columns x "
      (itoa (setq rowcnt (/ count colcnt)))
      " rows)."
    )
  )
  (setq i count)
  (repeat rowcnt
    (setq row nil)
    (repeat colcnt
      (setq row
	     (cons
	       (cdr
		 (assoc	1
			(entget (ssname ss (nth (setq i (1- i)) indices)))
		 )
	       )
	       row
	     )
      )
    )
    (setq table (cons row table))
  )

  (setq	table
	 (mapcar
	   'list->string
	   (apply
	     'mapcar
	     (cons
	       'list
	       (mapcar
		 '(lambda (column / numbers)
		    (if	(apply 'and (setq numbers (mapcar 'distof column)))
		      numbers
		      column
		    )
		  )
		 (apply 'mapcar (cons 'list table))
	       )
	     )
	   )
	 )
  )

  (if (setq outfile (getfiled "Export table text to CSV" "" "csv" 1))
    (progn
      (setq fd (open outfile "w"))
      (mapcar '(lambda (line) (write-line line fd)) table)
      (close fd)
    )
  )


  (princ)
)
;;;(defun lsort (input OnCompare / fun)
;;;   (setq fun (cond (OnCompare) (t '>)))
;;;   (lsort-aux input)
;;;)
;;;
;;;(if (not vl-sort)
;;;   (setq vl-sort lsort)
;;;)
;;;
;;;(defun lsort-aux (input)
;;;   (if (cdr input)
;;;      (  (lambda (tlist)
;;;            (lsort-merge 
;;;               (lsort-aux (car tlist))
;;;               (lsort-aux (cadr tlist))
;;;            )
;;;         )
;;;         (lsort-split input)
;;;      )
;;;      input
;;;   )
;;;)
;;;
;;;(defun lsort-split (right / left)
;;;   (repeat (/ (length right) 2)
;;;      (setq
;;;         left (cons (car right) left)
;;;         right (cdr right)
;;;      )
;;;   )
;;;   (list left right)
;;;)
;;;
;;;(defun lsort-merge (left right / out)
;;;   (while (and left right)
;;;      (if (apply fun (list (car left) (car right)))
;;;         (setq
;;;            out (cons (car left) out)
;;;            left (cdr left)
;;;         )
;;;         (setq
;;;            out (cons (car right) out)
;;;            right (cdr right)
;;;         )
;;;      )
;;;   )
;;;   (append (reverse out) left right)
;;;)
;; ********************Can dong text thang hang********************
(defun cht ()
  (setq d (getreal "\n Khoang cach vung anh huong"))
  (setq p (getpoint "\n Chon diem text moi"))
  (While p
    (setq sset (ssget "_C" p (list (- (car p) 6000) (+ (cadr p) d))))
    (setq p22 (cadr p))
    (setq itm 0
	  num (sslength sset)
    )
    (while (< itm num)
      (setq hnd (ssname sset itm))
      (setq ent (entget hnd))
      (Setq pt (assoc 11 ent))
      (setq p1 (cadr pt))
      (setq p2 (caddr pt))
      (setq p3 (cadddr pt))
      (setq p4 (list p1 p22 p3))
      (setq nt (cons '11 p4))
      (setq ent (subst nt pt ent))
      (entmod ent)
      (setq itm (1+ itm))
    )
    (setq p (getpoint "\n Chon diem text moi"))
  )
)
(prompt "\nDe can dong text thang hang ngang danh: ht")
(defun c:ht ()
  (cht)
)