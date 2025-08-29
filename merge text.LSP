;;;CADALYST 04/04 Tip1941:   JMT.LSP       Merge Text Objects     (c) 2004 Theodorus Winata
;;;
;;; Function: Join MTEXT/TEXT
;;; Command Line: JMT
;;; Description: Joins two line of texts or two mtext entities
;;;
;;;
;;; Developed by Theodorus Winata
;;; March 2003
;;;
;;;

(defun C:JMT (/ CMD DEL DEL1 JO MTA MT1 MT2 MTE MTX)
  (defun *error* (msg)
    (princ msg)
  );;error
  (setq CMD (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (prompt "\nTo join two MTEXT/TEXT entities: ")
  (setq MTE (entget (car (entsel "\nSelect MTEXT/TEXT to expand: "))))
  (if (= MTE nil) (exit))
  (setq DEL1 (cdr (assoc -1 MTE)))
  (redraw DEL1 3)
  (setq MTA (entget (car (entsel "\nSelect MTEXT/TEXT to add: "))))  
  (if (= MTA nil) (exit))
  (setq DEL (cdr (assoc -1 MTA)))
  (redraw DEL 3)
  (setq MT1 (cdr (assoc 1 MTE)))
  (setq MT2 (cdr (assoc 1 MTA)))
  (setq MTX (strcat MT1 " " MT2))
  (command "erase" DEL DEL1 "")
  (setq JO MTE)
  (setq JO (subst (cons 1 MTX) (assoc 1 JO) JO))
  (entmake JO)
  (setvar "CMDECHO" CMD)
  (princ)
);;C:JMT