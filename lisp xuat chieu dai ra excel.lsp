 
;;----------------------------ALDIMTXL.LSP-------------------------------;;
;; fixo (02012 * all rights released
;; 3/1/12
(defun C:ALDIMTXL (/ *error* col data dm elist en fname row sset xlapp xlbook xlbooks xlcells xlsheet xlsheets)
(defun *error* (msg)
(if
(vl-position
msg
'("console break"
"Function cancelled"
"quit / exit abort"
)
)
(princ "Error!")
(princ msg)
)
(princ)
)
;;;local defun
(defun setcelltext(cells row column value)
(vl-catch-all-apply
'vlax-put-property
(list cells 'Item row column
(vlax-make-variant
(vl-princ-to-string value) 8)))
)
 
;;; main part
(if (setq sset (ssget (list (cons 0 "dimension"))))
(progn
(while (setq en (ssname sset 0))
(setq elist (entget en))
(if (member "AcDbAlignedDimension" (mapcar 'cdr elist))
(progn
(if (eq "" (cdr (assoc 1 elist)))
(setq dm (rtos (cdr (assoc 42 elist)) 2 (getvar "dimdec")))
(setq dm (cdr (assoc 1 elist)))
)
(setq data (cons dm data))
)
)
(ssdel en sset)
)
(alert "Wait...")
(setq xlapp (vlax-get-or-create-object "Excel.Application")
xlbooks (vlax-get-property xlapp 'Workbooks)
xlbook (vlax-invoke-method xlbooks 'Add)
xlsheets (vlax-get-property xlbook 'Sheets)
xlsheet (vlax-get-property xlsheets 'Item 1)
xlcells (vlax-get-property xlsheet 'Cells)
)
 
(vla-put-visible xlapp :vlax-true)
(setq row 1)
(setq col 1)
 
(foreach dim data
(setcelltext xlcells row col dim)
 
(setq row (1+ row)
)
)
)
)
 
 
(vlax-invoke-method
(vlax-get-property xlsheet 'Columns)
'AutoFit)
 
(setq fname (strcat (getvar "dwgprefix")(vl-string-right-trim ".dwg" (getvar "dwgname")) ".xlsx"))
(vlax-invoke-method
xlbook
'SaveAs
fname 
nil
nil
nil
:vlax-false
:vlax-false
1
2
)
(vlax-invoke-method
xlbook 'Close)
(vlax-invoke-method
xlapp 'Quit)
(mapcar '(lambda (x)
(vl-catch-all-apply
'(lambda ()
(vlax-release-object x)
)
)
)
(list xlcells xlsheet xlsheets xlbook xlbooks xlapp)
)
(setq xlapp nil)
(gc)(gc)(gc)
(alert (strcat "File saved as:\n" fname))
(*error* nil)
(princ)
)
(prompt "\n\t\t---\tStart command with ALDIMTXL\t---\n")
(princ)
(vl-load-com)
(princ)
;;----------------------------code end-------------------------------;;