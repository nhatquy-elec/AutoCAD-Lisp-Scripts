;; CAB 12.02.09
;; Correct angles for lines with angles near 15 and 22.5 degrees +/- fuzz
;; Only LINES & only in Current Space
;; Only UnLocked Layers

(defun c:test (/ filter ss layobj i ent lst angles fuzz obj ang midpt)
  (vl-load-com)
  (setq angles '(15 22.5)       ; degree angles to test (0 through 90)
        fuzz 3.5                ; degree tolerance for adjustments of lines
  )  

  (setq angles (mapcar (function (lambda (x) (/ (* x pi) 180.0))) angles))  ; convert to radians
  (setq fuzz (/ (* fuzz pi) 180.0))  ; convert to radians
  (setq filter "") ;; filter locked layers
  (vlax-for layobj (vla-get-layers (vla-get-activedocument (vlax-get-acad-object)))
    (if (= (vla-get-lock layobj) ':vlax-true)
      (setq filter (strcat filter (vla-get-name layobj) ","))
    )
  )
  (and (= filter "") (setq filter "*"))

  (setq ss (ssget "_X" '((0 . "LINE")(410 . "Model"))))
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark (vla-get-activedocument (vlax-get-acad-object)))
  (setq i -1)
  (while (setq ent (ssname ss (setq i (1+ i))))
    (setq obj (vlax-ename->vla-object ent))
    (setq ang (vla-get-angle obj))
    (if (and (or (equal (setq re (rem ang (cadr angles))) 0.0 fuzz)
                 (and (equal (setq re (rem ang (cadr angles))) (cadr angles) fuzz)
                      (setq re (- re (cadr angles)))
                 )
               (equal (setq re (rem ang (car angles))) 0.0 fuzz)
               (and (equal (setq re (rem ang (car angles))) (car angles) fuzz)
                    (setq re (- re (car angles)))
               )
             )
          (not (zerop re))
        )
      (progn
        (setq midpt (mapcar '(lambda (a b) (/ (+ a b) 2.))
                            (vlax-get obj 'startpoint)
                            (vlax-get obj 'endpoint)
                    )
        )
        (vla-rotate obj (vlax-3d-point midpt) (- re))
      )
    )
  )
  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ)
)