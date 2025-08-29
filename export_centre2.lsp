(defun c:CtrCoo (/ findctr a apt)
  
  (defun findctr (en / pt)
    (command "_.Zoom" "_Object" en "")
    (setq pt (getvar 'viewctr))
    (command "_.Zoom" "_Previous")
    pt
    )
  
  (setq a (car (entsel "Select Rectangle: : "))
	apt (findctr a))
  (command "_Text" "_Justify" "_MC" apt 0.1 0 apt)
  (princ)
  )