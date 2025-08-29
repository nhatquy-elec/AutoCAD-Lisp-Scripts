;select lines by angle
;Stefan M. 26.03.2021
(defun c:ssla ( / get_line_angle e ss i a)
  
  (defun get_line_angle (e / p1 p2 a)
    (setq e (entget e)
          p1 (cdr (assoc 10 e))
          p2 (cdr (assoc 11 e))
          a  (angle p1 p2)
    )
    ;return
    ;a ; for exact match (0° /= 180°)
    (atan (/ (sin a) (cos a))) ; for visual match (0° = 180°)
  )
 
  (ssget "_I")
  (sssetfirst nil nil)
  
  (if
    (and
      (setq e (car (entsel "\nSelect line to match angle: ")))
      (setq ss (ssget "_X" '((0 . "LINE"))))
    )
    (progn
      (setq a (get_line_angle e))
      (repeat (setq i (sslength ss))
        (setq i (1- i)
              e (ssname ss i)
        )
        (or
          (equal a (get_line_angle e) 1e-6)
          (ssdel e ss)
        )
      )
      (sssetfirst nil ss)
    )
  )
 
  (princ)
)