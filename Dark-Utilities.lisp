;;;
;;; Dark-Utilities.lisp ---  Utilities I made or found in the public domain while learning lisp.
;;;


;;; Copyright (C) 2012-2017, Adrian Xander Blackstone <DarkrainDragon@gmail.com> All rights reserved.
;;; Copyright (C) 2012-2017, Rian Jensen <DarkrainDragon@gmail.com> All rights reserved.


;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.







;;---------------------------------------------------------------------------
;; CODE BEGINS HEREISH
;;---------------------------------------------------------------------------

(setf *print-pretty* t) ; Print lists out in a pretty way.

;(setf *print-circle* t) ; This causes the REPL to detect and print circular lists.
;  I have no idea how much this slows shit down or what the downside is.



;;;; Starting quicklisp on a Windows 7 box I don't control:
;;;; Since I can not set up a proper .sbcl file I need to load the setup.lisp file each time.
;(sort (copy-seq *features*) #'string-lessp :key #'symbol-name)
#+(or :windows :win32)(load (format nil "~a~a" *default-pathname-defaults* "quicklisp/setup.lisp"))

;;(ql:quickload 'fare-memoization) ;;--! Just for a reminder for later projects.

(ql:quickload 'trivial-features)
(ql:quickload 'trivial-utf-8)

(ql:quickload 'alexandria)
(ql:quickload 'iterate)
(ql:quickload 'cl-ppcre)




(in-package :cl-user)

(defpackage :dark-utilities
  ;;(:use :cl :iterate :cffi :cffi-grovel)
  (:use :cl :iterate)
  (:import-from :alexandria with-gensyms once-only with-output-to-file with-input-from-file)
  (:import-from :cl-ppcre all-matches-as-strings scan split)
  ;;(:import-from :ironclad digest-file)
  ;;(:import-from :cl-base64 usb8-array-to-base64-string)
  (:documentation "Various utility functions/macros I find useful.")
  (:export



:with-multiple-bindings
:with-list-tail
:push-end
:pop-end
:pop-nth
:sethash
:pushhash
:push-hash-if-list
:sethash-if
:push-if
:while-do
:do-until
:convert-by-lines
:with-lines-from-files
:at-least
:mklist
:push-force-list
:push-only-if-list
:force-list
:strcat
:strcat-list
:flatten
:lengthp
:end
:as-keyword
:make-a-vector
:make-a-string
:make-table
:posix-time-string
:time-string
:today-string
:join
:chomp
:chomp-dir
:string-to-dir
:make-path
:print-hash
:print-table
:print-table-entry
:dump-table
:table-to-file
:vector-to-file
:list-to-file
:array-to-file
:array-to-file-crlf
:array-to-file-eol
:write-to-file
:write-objects
:stream-to-vector
:file-to-vector
:meout-now
:text->wrap 
:text->comment 
:select-minimum 
:selection-sort 
:strcat))








(in-package :dark-utilities)



;;---------------------------------------------------------------------------
;; UTILITY STUFF:
;;---------------------------------------------------------------------------

(defmacro with-multiple-bindings (macro)
  "Define a version of `macro' with multiple arguments, given as a
list.  Application of `macro' will be nested.  The new name is the 
plural of the old one (generated using format).

Tamas K Papp placed this into the public domain on 20091210.

http://blog.gmane.org/gmane.lisp.cffi.devel/month=20091201"
  (let ((plural (intern (format nil "~aS" macro))))
    `(defmacro ,plural (bindings &body body)
       ,(format nil "Multiple binding version of ~(~a~)." macro)
       (if bindings
	   `(,',macro ,(car bindings)
		     (,',plural ,(cdr bindings)
			       ,@body))
	   `(progn ,@body)))))

(defmacro with-list-tail (list-var &body body)
  "Macro for operating on the other side of a list,
by Pascal J. Bourguignon"
  `(unwind-protect
	(progn (setf ,list-var (reverse ,list-var))
	       ,@body)
     (setf ,list-var (nreverse ,list-var))))

(defmacro push-end (item place)
  "Push an item to the end of a list."
  `(setf ,place (nconc ,place (list ,item))))

(defmacro pop-end (place)
  "Pops the last element of a list, returning that element."
  (with-gensyms (end)
    `(let ((,end (car (last ,place))))
       (setf ,place (nbutlast ,place))
       ,end)))

(defmacro pop-nth (lst n)
  "Pops the NTH element of a list, returning that element.
Taken from  http://stackoverflow.com/questions/4093845/is-there-a-common-lisp-macro-for-popping-the-nth-element-from-a-list
Original Author: postfuturist
Cleaned up by Adrian Xander Blackstone aka Rian Jensen 20121013

The other entries from stackoverflow.com all had some sort of issue.
The second attempt by postfuturist guy with the question works the best."
  (let ((tail (gensym)) 
	(cell (gensym)))
    `(if (<= ,n 0)
	 (pop ,lst)
	 (let* ((,tail (nthcdr (1- ,n) ,lst))
		(,cell (car (cdr ,tail))))
	   (setf (cdr ,tail) (cddr ,tail))
	   ,cell))))

(defmacro sethash (table key value)
  "Set a hash key to a value."
  `(setf (gethash ,key ,table) ,value))

(defun mklist (thing)
  (if (listp thing) thing (list thing)))

(defun push-force-list (lst item)
  "Push an item to a list.  
If the list given is actually an atom it is turned into a list."
  (if (listp lst) 
      (nconc lst (list item))
      (nconc (list lst) (list item))))

(defmacro pushhash (table key value)
  "Pushes a new value on to the value list at the given key.
If the value already at key is an atom it is turned into a list."
  `(setf (gethash ,key ,table) (push-force-list (gethash ,key ,table) ,value)))

(defun push-only-if-list (lst item)
  "Push an item to a list.  
If the list given is actually an atom an error is spat out."
  (if (listp lst) 
      (nconc lst (list item))
      (error "List given to add a value to is actually an atom.")))

(defmacro push-hash-if-list (table key value)
  "Pushes a new value on to the value list at the given key.
If the value already at key is an atom an error is spat out."
  `(setf (gethash ,key ,table) (push-only-if-list (gethash ,key ,table) ,value)))


(defmacro sethash-if (table key value)
  "Upadates the hash only if key and value are both non-NIL."
  (let ((k key))
    (if k (let ((v value))
	    (if v `(sethash ,table ,k ,v))))))

(defmacro push-if (value list)
  "Only pushes non-nil values on to the list."
  (let ((v value))
    (if v `(push ,v ,list))))

;; Leaving these 2 in just to remind me of the loop way.
(defmacro while-do (condition body) `(loop while ,condition do ,body))
(defmacro do-until (body condition) `(loop do ,body until ,condition))

(defun force-list (x) (if (listp x) x (list x)))

(defun strcat(&rest list)
  "Concatenate strings into 1 string.  Turns all input into strings."
  (format nil "~{~a~}" list))

(defun strcat-list (list)
  "This uses reduce because apply has a limit on the number of strings it can
handle"
  (reduce (lambda (a b)
            (concatenate 'string a b))
          (remove-if-not #'stringp list)))

;; The version in Alexandria loses NILs.
(defun flatten-1 (l)
  "Flattens a nested list.  NILs survive like atoms.
Thanks to Bill, unregistered user of Stack Overflow."
  (cond ((null l) nil)
        ((atom (car l)) (cons (car l) (flatten-1 (cdr l))))
        (t (append (flatten-1 (car l)) (flatten-1 (cdr l))))))

(defun flatten (&rest l)
  "Flattens a nested list.  NILs survive like atoms.
Thanks to Bill, unregistered user of Stack Overflow.

This uses flatten-1 So (flatten 1 2 3 4) works as expected."
  (flatten-1 l))

(defun lengthp (sequence)
  "Returns the length of a sequence or NIL if it is 0."
  (let ((l (length sequence)))
    (and (plusp l) l)))

(defun end (sequence) 
  "Returns the last entry of a sequence, or NIL on empty."
  (let ((length (length sequence)))
    (when (> length 0) (elt sequence (1- length)))))

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun make-a-vector (size &rest rest)
  "I'm sick of typing :adjustable t :fill-pointer t"
  (apply 'make-array `(,size ,@rest :adjustable t :fill-pointer t)))

(defun make-a-string (size &rest rest)
  "I'm sick of typing :adjustable t :fill-pointer t"
  (apply 'make-a-vector `(,size ,@rest :element-type character)))

(defun make-table (&rest rest)
  "I'm sick of typing :test 'equal"
  (apply 'make-hash-table `(,@rest :test equal)))

(defun posix-time-string (time)
  "Returns a string containing the given time in YYYYMMDD format."
  (multiple-value-bind (second minute hour date month year weekday dst-p tz)
      (decode-universal-time (+ time 2208988800))
    (declare (ignore weekday dst-p tz))
    (format nil "~4,'0d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second)))

(defun time-string (&optional (time (get-universal-time)))
  "Returns a string containing the given time in YYYYMMDD format."
  (multiple-value-bind (second minute hour date month year weekday dst-p tz)
      (decode-universal-time time)
    (declare (ignore weekday dst-p tz))
    (format nil "~4,'0d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second)))

(defun today-string (&optional (time (get-universal-time)))
  ;; This is an example of how to use the ~? option in format:
  "Returns a string containing the current date in YYYYMMDD format"
  (format nil "~?" "~5*~4,'0d~2:*~2,'0d~2:*~2,'0d" 
	  (multiple-value-list (decode-universal-time time))))

(defun join (seperator &rest list)
  "Like the Perl join.  Joins strings together with a seperator between."
  (loop for i from 0 below (1- (length list))
     with output = ""
     do (setf output (strcat output (elt list i) seperator))
     finally (return (strcat output (end list)))))

(defun chomp (oldline)
  "Much like the Perl chomp.  Takes in a string and returns a string with all
trailing carriage returns and line feeds removed."
  (iter
     (with line = (make-a-string (length oldline) :initial-contents oldline))
     (while (find (end line) '(#\cr #\lf)))
     (vector-pop line)
     (finally (return line))))

(defun chomp-dir (oldline)
  "Kind of like the Perl chomp.  Takes in a string and returns a string with
all trailing / removed.

I will be using this on posix, so everything but / is fair game, because it
may exist in a file name on Unix."
  (iter
     (with line = (make-a-string (length oldline) :initial-contents oldline))
     #+unix(while (find (end line) '(#\/)))
     #+windows(while (find (end line) '(#\\ #\/)))
     (vector-pop line)
     (finally (return line))))

(defun string-to-dir (path)
  (pathname (strcat (chomp-dir path) "/")))

(defun make-path (path) ; AKA mkdir.
  "This works even if you forget the trailing /"
  (ensure-directories-exist (string-to-dir path))
  #+NIL(meout-now "Ensuring ~a exists~%" path))

(defun print-hash (table &optional (stream t))
  "Prints out a hash in human-readable format."
  (maphash #'(lambda (k v) (format stream "~a => ~a~%" k v)) table))

(defun print-table (table &optional (stream t))
  "Prints out a hash in human-readable format."
  (maphash #'(lambda (k v) (format stream "~s => ~s~%" k v)) table))

(defun print-table-entry (k v &optional (output t))
  (format output "~s~%" (list k v)))

(defun dump-table (table &optional (output t))
  "Prints out a hash in lisp readable format."
  (iter (for (k v) in-hashtable table)
	(print-table-entry k v output)))

(defun table-to-file (table filename)
  "Dumps a hash to a file."
  (with-output-to-file (output filename :if-exists :supersede)
    (iter (for (k v) in-hashtable table)
	  (print-table-entry k v output))))

(defun vector-to-file (sequence filename &key (element-type 'character))
  "Dump a sequence to a file.  Returns the dumped sequence."
  (with-output-to-file (output filename :if-exists :supersede 
			  :element-type element-type)
    (write-sequence sequence output)))

(defun list-to-file (array filename &optional (eol nil))
  "Dump a sequence of objects to a file, with an optional EOL."
  (with-output-to-file (output filename :if-exists :supersede)
    (iter (for element in-sequence array)
	  ;; Only insert an eol if there is one to insert.
	  (format output "~s~:[~;~a~]" element eol eol))))

(defun array-to-file (array filename &optional (eol nil))
  "Dump an array of strings or characters to a file, with an optional EOL."
  (when (lengthp array)
    (format t "Writing an array to ~s ~:[~;with EOL~] gave result ~s~%" filename eol
	    (with-output-to-file (output filename :if-exists :supersede)
	      (iter (for element in-vector array)
		    ;; Only insert an eol if there is one to insert.
		    (format output "~s~:[~;~a~]" element eol eol))))))

(defun array-to-file-crlf (array filename)
  "Dump an array of strings to a file, ending each line with CR/LF."
  (array-to-file array filename (format nil "~c~c" #\cr #\lf)))

(defun array-to-file-eol (array filename)
  "Dump an array of strings to a file, ending each line with the system EOL."
  (array-to-file array filename (format nil "~%")))

(defun write-to-file (object filename)
  "Write an object to a filename.  Useful for writing a list of lists
to be read in later."
  (with-output-to-file (output filename :if-exists :supersede)
    (write object :stream output)))
  
(defun write-objects (objects filename)
  "Write a sequence of objects to a file seperated by line feeds."
  (with-output-to-file (output filename :if-exists :supersede)
    (iter (for object in-sequence objects)
	  (write  object :stream output)
	  (format output "~%"))))




;; This is fun for reading into a byte vector other than 8 bits.
(defun stream-to-vector (stream &key (element-type 'character))
  "Converts a stream into a vector of the given type.
Defaults to dumping the whole file into a single string.
Modified from the original at http://www.ymeme.com/slurping-a-file-common-lisp-83.html
Apparently originally from www.emmett.ca/~sabetts/slurp.html but I can not find the original."
  (let ((length (file-length stream)))
    (assert length)
    (let ((array (make-array (file-length stream)
			     :element-type element-type :fill-pointer t)))
      #+UTF8-protection(setf (fill-pointer array) (read-sequence array stream))
      (read-sequence array stream)
      array)))

(defun file-to-vector (filename &key (element-type 'character))
  "Converts a file into a vector of the given type.
Defaults to dumping the whole file into a single string."
  (with-input-from-file (in filename :element-type element-type)
    (stream-to-vector in :element-type element-type)))


(defmacro convert-by-lines (in-type out-type input 
			 &key (reader 'read-line) (chomp t))
  "Converts files, streams, lists, vectors, and general sequences of lines
to lists, vectors, or keys of a hash table with values all set to T.

Use :chomp NIL to turn off chomping (stripping trailing CR/LF instances).
Use :reader to set the file-reader used for reading files.
The default is 'read-file.  You may wish to use 'read to read forms into a
list.

Usage:
 (convert-by-lines input-type output-type input 
   :reader <reader function> :chomp <T or NIL>)

input-type can be list array strings vector lines file stream sequence keys
or hashkeys

output-type can be list sequence array strings vector lines 

Examples:

Dump a file line by line into a vector with CR/LF stripped off the ends:

 (convert-by-lines file vector some-file)

Dump a file into a list of forms:

 (convert-by-lines file list some-file :reader 'read)

Dump a file line by line into a hash table where each line is a key and each
value is true.  Useful for dumping an old list of files, then checking to see
which files on the system were not there before:

 (convert-by-lines file keys some-file)

Do CR LF stripping on a list of strings:

 (convert-by-lines list list some-list)

Convert a list of strings into hash keys:

 (convert-by-lines list keys some-list)

etc."
  (with-gensyms (line junk output)
    `(iter ,(ecase in-type
;; For each input type pick the way to handle it.
		   (list     `(for ,line in ,input))
		   ((array strings vector lines) `(for ,line in-vector ,input))
		   (file     `(for ,line in-file ,input using ',reader))
		   (stream   `(for ,line in-stream ,input using ',reader))
		   (sequence `(for ,line in-sequence ,input))
		   ((keys hashkeys) `(for (,line ,junk) in-hashtable ,input)))
	   ,@(ecase out-type
;; For each output type pick the way to handle it.
		    ((list sequence) `((collecting ,(if chomp `(chomp ,line) line))))
		    ((array strings vector lines)
		     `((with ,output = 
			     (make-a-vector 0 :element-type 'string))
		       (vector-push-extend 
			,(if chomp `(chomp ,line) line) ,output)
		       (finally (return ,output))))
		    ((hash keys hashkeys hashtable)
		     `((with ,output = (make-table))
		       (sethash ,output ,(if chomp `(chomp ,line) line) t)
		       (finally (return ,output))))))))

(defun meout-now (&rest args)
  "Wrapper for format to get immediate output to STDOUT.
Name inspired by what my cat says, next to the door, because he thinks he can
be trusted alone outside at night.  He also thinks he can beat a car in a
fair fight, so no, he can't go out without adult supervision.

Kinda like the way SBCL treats output to STANDARD IO."
  (apply 'format (flatten t args))
  ;; NIL Denotes STANDARD IO for FINISH-OUTPUT, like T for format.
  (finish-output nil))


(defun text->wrap (width text-string)
  "Wrap a string to the given width."
  (let ((comment-string (make-array '(0) :element-type 'base-char 
				    :fill-pointer 0 :adjustable t)))
    (with-output-to-string (comment comment-string)
      (format comment "~%~{~a~%~}~%" 
	      (remove-if-not #'plusp 
			     (cl-ppcre:split 
			      (format nil "(.{1,~a})\\s" width) 
			      text-string :with-registers-p t)
			     :key #'length)))
    comment-string))


(defun text->comment (text-string)
  "Convert a string input into commented text, wrapped at a width of 77."
  (text->wrap 77 text-string))


(defmacro at-least (n &rest es &aux (nsym (gensym)) (carsym (gensym)))
  "Macro that will return true if at least n items of a list are true.
Author: Raffael Cavallaro"
 (if (null es) nil
   `(let ((,nsym ,n))
      (if (zerop ,nsym) t
        (let ((,carsym ,(car es)))
          (if (= ,nsym 1) (or ,carsym ,@`,(cdr es))
            (at-least (if ,carsym (1- ,nsym) ,nsym) ,@`,(cdr es))))))))
	     

(defun select-minimum (x)
  (cond
   ((null (cdr x)) (list (car x) nil))
   (t (let ((p (select-minimum (cdr x))))
        (if (< (car x) (car p))
            (list (car x) (cdr x))
          (list (car p) (cons (car x) (cadr p))))))))

(defun selection-sort (x)
  (cond
   ((null x) nil)
   (t (let ((p (select-minimum x)))
        (cons (car p) (selection-sort (cadr p)))))))





