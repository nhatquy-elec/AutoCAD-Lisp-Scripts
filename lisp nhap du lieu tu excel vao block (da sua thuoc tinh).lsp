;; free lisp from cadviet.com
;;; this lisp was downloaded from http://www.cadviet.com/forum/index.php?showtopic=29417
(defun Separate (S sym / i L ch)
(setq i 0 L nil)
(while (< i (strlen S))
      (setq i (1+ i) ch (substr S i 1))
      (if (= ch sym) (progn
	(setq
	      L (append L (list (substr S 1 (- i 1))))
	      S (substr S (1+ i) (- (strlen S) i))
	      i 0
	)
      ))	
)
(append L (list S))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:nhapatt ()
(setq  fn (getfiled "Select Data File" "" "csv" 0)
        ;;;;;;;;;;;;;;; f (open fn "r")
)
;;;;(read-line f)
(command "undo" "be")
(if (= (setq rep (strcase (getstring "\n Ban muon xuat het cac du lieu (y or n): "))) "Y")
    (progn
            (setq PT (getpoint "\n Chon diem nhap"))
            (setq f (open fn "r"))
            (while (setq S (read-line f))
                     (setq L (separate S ",")
		VTC (nth 8 L)
		LC (nth 2 L)
		XA (nth 9 L)
		SU (nth 6 L)
		MONG (nth 3 L)
		NEO (nth 5 L)
		TD (nth 7 L) 
          	      )
          ;;;;;;;;;;(alert (strcat "\n Ten chi tiet là " TenCT))
          ;;;;;;;;;;(if (and (= (setq ans (strcase (getstring "\n Ban muon nhap chi tiet nay (y or n): "))) "Y") PT)
                    (Command "insert" "COT_ATTRIB" PT "" "" ""
		VTC
		LC
		XA
		SU
		MONG
		NEO
		TD )
                    (setq en (entlast)
                            pts (acet-ent-geomextents en)
                            PT (list (caar pts) (+ (cadr (cadr pts)) (- (cadr (cadr pts)) (cadr (car pts)))))
                    )              
              )
              (close f)
     )
     (progn
             (while (/= (setq ddl (getstring "\n Nhap so thu tu cua dong du lieu muon nhap: ")) "" )
             (setq PT1 (getpoint "\n Chon diem nhap"))
             (setq f (open fn "r"))
             (while (setq S (read-line f))
	(setq L (separate S ",")
		VTC (nth 8 L)
		LC (nth 2 L)
		XA (nth 9 L)
		SU (nth 6 L)
		MONG (nth 3 L)
		NEO (nth 5 L)
		TD (nth 7 L)
	)
                    (if (= ddl STT)
                        (Command "insert" "COT_ATTRIB" PT1 "" "" ""
		VTC
		LC
		XA
		SU
		MONG
		NEO
		TD
	       ) 
                    )
            )
            (close f)
            )
      )
)  

(command "undo" "e")

(princ)
)