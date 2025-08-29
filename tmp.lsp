(defun c:pt2txt ( / counter endpoint entitydata filename openfile selection startpoint )

    ;; Define the function and localise the variables.

    (if
        ;; If the following expression returns a non-nil value...
        
        (and

            ;; All of the enclosed expressions must return a non-nil
            ;; value for the AND function to return T.

            (setq selection (ssget '((0 . "LINE"))))

            ;; Provide the SSGET function with a filter list so
            ;; that only LINEs may be selected.
            
            (setq filename (getfiled "Specify Output File" "" "txt" 1))

            ;; The IF statement is required since the user could press
            ;; 'cancel' when prompted to specify an output file.
            ;;
            ;; If 'cancel' is pressed, the GETFILED function would
            ;; return nil.
            ;;
            ;; (open nil "w") will result in an error.

            (setq openfile (open filename "w"))

            ;; A check that the file was able to be opened for writing.
            ;; This is to account for the possibility that the user
            ;; has specified an output file in a location for which they
            ;; do not have write permissions.
            ;;
            ;; In such a case (open <filename> "w") will return nil.
            
        ) ;; End AND

        (progn

            ;; The IF function can only accept at most three parameters:
            ;; a TEST expression, a THEN expression to be evaluated if the
            ;; TEST expression evaluates to a non-nil value, and an ELSE expression
            ;; to be evaluated if TEST evaluates to nil.
            ;;
            ;; Hence, if we wish to evaluate multiple expressions within the THEN
            ;; parameter, we need to wrap these expressions into a single PROGN
            ;; expression to be passed to the IF function.
            ;;
            ;; The PROGN function does nothing more than evaluate the expressions
            ;; passed to it, then return the result of the last expression evaluated.
            
            (setq counter 0)

            ;; Initialise the counter variable at zero
            ;; (since the Selection Set index is zero based).

            (repeat (sslength selection)

                ;; Repeat evaluation of the following expressions a number
                ;; of times equal to the number of objects in the selection set.

                (setq entitydata (entget (ssname selection counter)))

                ;; The entity at index 'counter' in the selection set is returned by
                ;; the SSNAME function, and its corresponding DXF entity data is returned
                ;; by the ENTGET function.

                (setq startpoint (cdr (assoc 10 entitydata))
                      endpoint   (cdr (assoc 11 entitydata))
                )

                ;; Collect the start and end points of the Line entity from the DXF data list
                ;;
                ;; We know that is must be a LINE entity since the SSGET filter list has
                ;; only allowed the user to select lines.

                (write-line
                    (strcat
                        (rtos (car   startpoint)) ";"
                        (rtos (cadr  startpoint)) ";"
                        (rtos (caddr startpoint))
                    )
                    openfile
                )

                (write-line
                    (strcat
                        (rtos (car   endpoint)) ";"
                        (rtos (cadr  endpoint)) ";"
                        (rtos (caddr endpoint))
                    )
                    openfile
                )

                ;; Write the x;y;z values of the start and end points of the Line entity
                ;; to the open file.
                ;;
                ;; RTOS is not supplied with units or precision parameters, so the values
                ;; of the LUNITS and LUPREC System Variables will be used.

                (setq counter (1+ counter))

                ;; Increment the counter to move on to the next line in the Selection Set

            ) ;; End REPEAT

            (close openfile)

            ;; Close the open file.

            (princ (strcat "\nThe data from " (itoa counter) " line(s) has been written to file."))

            ;; Print a message to inform the user that the data has been written successfully.

        ) ;; End PROGN

    ) ;; End IF

    (princ)

    ;; (princ) is used to 'exit cleanly', since otherwise the
    ;; result of the last expression evaluated (the IF expression) would
    ;; be returned.

) ;; End DEFUN