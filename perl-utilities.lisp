;;;;
;;;; perl-utilities.lisp  ---  Perl style utilities
;;;;



;; PERL type stuff in CL:
;; These are from http://a-nickels-worth.blogspot.in/2008/02/scripting-in-cl.html
;; "A Nickle's worth" by Jacob Gabrielson 

(defun do-all-lines (fn &rest filenames)
  (dolist (filename-or-list filenames)
    ;; For convenience we allow you also to pass in lists of filenames
    (dolist (filename (typecase filename-or-list
                        (cons filename-or-list)
                        (t (list filename-or-list))))
      (with-open-file (stream filename)
        (loop
           for line = (read-line stream nil nil)
           while line
           do (funcall fn line))))))


#+nil(defun grep (regexp filename)
  (with-open-file (stream filename)
                  (loop for line = (read-line stream nil nil)
                        while line
                        do (when (scan regexp line)
                             (format t "~A~%" line)))))

(defmacro with-lines-from-files ((var &rest filenames) &body body)
  `(do-all-lines (lambda (,var) ,@body) ,@filenames))

(defun grep (regexp &rest filenames)
  (with-lines-from-files (line filenames)
    (when (scan regexp line)
      (format t "~A~%" line))))


;; The following macro is mostly like Perl's foreach, in the sense
;; that you can pass in as many references to sequences or "scalars"
;; as you want and it will iterate over them and allow you to modify
;; them.  Unlike the Perl code, it sets the variable IT to each
;; element rather than $_.  Also, you have to just pass in the hash
;; table directly, not a flattened list of hash keys.
(defmacro perl-foreach ((&rest refs) &body body)
  (let* ((gensyms (loop repeat (length refs) collect (gensym))))
    (list*
     'let
     (mapcar #'list gensyms refs)
     (loop
        for ref in refs
        and indirect-ref in gensyms
        collect
        `(typecase ,indirect-ref
           (hash-table 
            (maphash #'(lambda (key value)
                         (declare (ignore value))
                         (symbol-macrolet ((it (gethash key ,indirect-ref)))
                           ,@body))
                     ,indirect-ref))
           ((and (or vector list) (not string))
            (map-into ,indirect-ref
                      #'(lambda (it)
                          ,@body
                          it)
                      ,indirect-ref))
           (t 
            (symbol-macrolet ((it ,ref))
              ,@body)))))))

;; trim whitespace in the scalar, the list, the array, and all the
;; values in the hash
#+nil(perl-foreach (scalar my-list my-array my-hash)
  (setf it (regex-replace "^\\s+" it ""))
  (setf it (regex-replace "\\s+$" it "")))


(defmacro perl-splice (sequence-place &optional (offset 0) length replacement-sequence)
  (let* ((seq (gensym "SEQUENCE-PLACE-"))
         (off-arg (gensym "OFFSET-ARG-"))
         (off (gensym "OFFSET-"))
         (len (gensym "LENGTH-"))
         (end (gensym "END-"))
         (rep (gensym "REPLACEMENT-SEQUENCE-"))
         (left-part (list `(subseq ,seq 0 ,off)))
         (right-part (when length
                       (list `(subseq ,seq ,end)))))
    `(let* ((,seq ,sequence-place)
            (,off-arg ,offset)
            (,off (if (minusp ,off-arg)
                      (+ (length ,seq) ,off-arg)
                      ,off-arg))
            (,len ,length)
            (,end (when ,len
                    (if (minusp ,len)
                        (+ (length ,seq) ,len)
                        (+ ,off ,len))))
            (,rep ,replacement-sequence))
       (prog1 (subseq ,seq ,off ,end)
         (when (or ,rep (not (eql ,off ,end)))
           (setf ,sequence-place (concatenate (typecase ,seq
                                                (cons 'list)
                                              (t 'vector))
                                            ,@left-part
                                            ,rep
                                            ,@right-part)))))))

;; Now the syntax is almost exactly the same.
#+nil(setf front (perl-splice my-array 0 n))
#+nil(setf end (perl-splice my-array 0 (- n)))





