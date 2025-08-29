;;; Touch.LSP                                              *
;;; Small routine to align endpoints of lines to an edge.  *
;;; The edge have to be a line.                            *
;;; The routine works by calculating the point of inter-   *
;;; section and change the nearest endpoint to that point  *
;;; 2001 Stig Madsen, no rights reserved                   *
;;; modified by qjchen, the edge line can be line or polyline *
;
;GREAT for PROJECTING LINES FOR ELEVATIONS !!!!!!!!!!!
;
(defun C:Ttt (/ cmd ent entl spt ept sset a lent lentl lspt lept lint)
  (vl-load-com)
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "UNDO" "Begin")
  (while (not ent)
    (setq ent (car (entsel "Select edge line: ")))
    (if ent
      (progn
	(setq entl (entget ent))
      )
    )
  )
  (if ent
    (progn
      (redraw ent 3)
      (prompt "\nSelect lines to touch edge: ")
      (setq sset (ssget '((0 . "LINE")))
	    a 0
      )
      (if sset
	(repeat (sslength sset)
	  (setq lentl (entget (setq lent (ssname sset a)))
		lspt (cdr (assoc 10 lentl))
		lept (cdr (assoc 11 lentl))
	  )
	  (setq entttt (ssname sset a))
	  (setq lint (nth 0 (x_intlst ent entttt acExtendOtherEntity)))
	  (if lint
	    (progn

	      (if (< (distance lint lspt) (distance lint lept))
		(entmod (subst
			  (cons 10 lint)
			  (assoc 10 lentl)
			  lentl
			)
		)
		(entmod (subst
			  (cons 11 lint)
			  (assoc 11 lentl)
			  lentl
			)
		)
	      )
	    )
	  )
	  (setq a (1+ a))
	)
	(princ "\nNo objects found")
      )
      (redraw ent 4)
    )
    (princ "\nNo edge selected")
  )
  (setvar "CMDECHO" cmd)
  (command "UNDO" "End")
  (princ)
)

;;; by kuangdao at xdcad
(defun x_intlst (obj1 obj2 param / intlst1 intlst2 ptlst)

  (if (= 'ENAME (type obj1))
    (setq obj1 (vlax-ename->vla-object obj1))
  )
  (if (= 'ENAME (type obj2))
    (setq obj2 (vlax-ename->vla-object obj2))
  )
  (setq intlst1 (vlax-variant-value (vla-intersectwith obj1 obj2 param)))
  (if (< 0 (vlax-safearray-get-u-bound intlst1 1))
    (progn
      (setq intlst2 (vlax-safearray->list intlst1))
      (while (> (length intlst2) 0)
	(setq ptlst (cons (list (car intlst2) (cadr intlst2) (caddr intlst2))
			  ptlst
		    )
	      intlst2 (cdddr intlst2)
	)
      )
    )
  )
  ptlst
)