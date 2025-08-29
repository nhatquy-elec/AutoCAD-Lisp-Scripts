(defun C:s2p ()
(while (not (and
              (setq lstSelection   (entsel "\nSelect Spline: "))
              (setq sngSegment     (getdist "\nGet Segment Length: "))
              (setq objSelection   (vlax-ename->vla-object (car
lstSelection)))
              (wcmatch (vla-get-objectname objSelection) 

"AcDb2dPolyline,AcDbPolyline,AcDbLine,AcDbArc,AcDbSpline"
              )
             )
        )
  (princ "\nError please select again: ")
)
(setq sngLength   (vlax-curve-getDIstAtParam objSelection
                    (vlax-curve-getEndParam objSelection)
                   )
       sngDistance 0.0
)
(vl-cmdf "._pline" (vlax-curve-getpointatDist objSelection 0.0))  (repeat (fix (/ sngLength sngSegment))
  (vl-cmdf (vlax-curve-getpointatDist objSelection sngDistance))
  (setq sngDistance (+ sngDistance sngSegment))
)
(vl-cmdf (vlax-curve-getPointAtParam objSelection
           (vlax-curve-getEndParam objSelection)
          )
          ""
)
)