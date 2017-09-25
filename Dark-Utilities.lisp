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
;; HEADER CODE BEGINS HEREISH
;;---------------------------------------------------------------------------

(setf *print-pretty* t) ; Print lists out in a pretty way.

(setf *print-circle* t) ; This causes the REPL to detect and print circular lists.
;  I have no idea how much this slows shit down or what the downside is.



;;;; Starting quicklisp on a Windows 7 box I don't control:
;;;; Since I can not set up a proper .sbcl file I need to load the setup.lisp file each time.
;(sort (copy-seq *features*) #'string-lessp :key #'symbol-name)
;;#+(or :windows :win32)(load (format nil "~a~a" *default-pathname-defaults* "quicklisp/setup.lisp"))

;;(ql:quickload 'fare-memoization) ;;--! Just for a reminder for later projects.

(ql:quickload 'trivial-features) ; Use :WINDOWS and :UNIX
;;(ql:quickload 'trivial-utf-8)

(ql:quickload 'alexandria)
(ql:quickload 'iterate)
(ql:quickload 'cl-ppcre)




(in-package :cl-user)

(defpackage :dark-utilities
  ;;(:use :cl :iterate :cffi :cffi-grovel)
  (:use :cl :iterate)
  (:import-from :alexandria with-gensyms once-only with-output-to-file with-input-from-file)
  (:import-from :cl-ppcre all-matches-as-strings scan split)
  ;;(:import-from :cl-base64 usb8-array-to-base64-string)
  (:documentation "Various utility functions/macros")
  (:export
   :with-multiple-bindings
   :with-list-tail
   :push-end
   :pop-end
   :pop-nth
   :sethash
   :push-hash
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
   :unchomp-dir
   :string-to-dir
   :make-path
   :write-utf8-line-with-crlf
   :write-utf8-lines-to-file
   :write-utf16-line-with-crlf
   :write-utf16-lines-to-file
   :print-hash
   :print-table
   :print-table-entry
   :dump-table
   :table-to-file
   :file-to-table
   :vector-push-extend-list
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
   :strcat
   :flatten-or-vector
   :resize-bytes
   :eltnil
   :slice
   :elt-shift
   :join-bytes
   :split-bytes
   :cwd
   :windows-namestring
   :glob
   :globs))


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
Taken from http://stackoverflow.com/questions/4093845/is-there-a-common-lisp-macro-for-popping-the-nth-element-from-a-list
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

(defmacro push-hash (table key value)
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

;;  (flatten '('pancake)) returns 'PANCAKE instead of ('PANCAKE)
;; NOT A BUG BECAUSE (car (flatten '('pancake))) returns QUOTE because 'PANCAKE == (QUOTE 'PANCAKE)
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

(defun posix-time-string (&optional (time (get-universal-time)))
  "Returns a string containing the given time in YYYYMMDDhhmmss format."
  (multiple-value-bind (second minute hour date month year weekday dst-p tz)
      #+nil(decode-universal-time (+ time 2208988800))
      (decode-universal-time time)
    (declare (ignore weekday dst-p tz))
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" year month date hour minute second)))

(defun time-string (&optional (time (get-universal-time)))
  "Returns a string containing the given time in YYYY/MM/DD hh:mm:ss format."
  (multiple-value-bind (second minute hour date month year weekday dst-p tz)
      (decode-universal-time time)
    (declare (ignore weekday dst-p tz))
    (format nil "~4,'0d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second)))

(defun today-string (&optional (time (get-universal-time)))
  ;; This is an example of how to use the ~? option in format:
  "Returns a string containing the current date in YYYYMMDD format"
  (format nil "~?" "~5*~4,'0d~2:*~2,'0d~2:*~2,'0d" 
	  (multiple-value-list (decode-universal-time time))))

(defun today-and-now (&optional (time (get-universal-time)))
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

(defun chomp-dir-old (oldline)
  "Kind of like the Perl chomp.  Takes in a string and returns a string with
all trailing / removed.

I will be using this on posix, so everything but / is fair game, because it
may exist in a file name on Unix."
  (iter
     (with line = (make-a-string (length oldline) :initial-contents oldline))
     #+unix(while (find (end line) '(#\/)))
     #+(or windows win32)(while (find (end line) '(#\\ #\/)))
     (vector-pop line)
     (finally (return line))))

(defun chomp-dir (dir)
  (cl-ppcre:regex-replace "[\\\\/]+$" dir ""))

(defun unchomp-dir (dir)
  (cl-ppcre:regex-replace "[\\\\/]*$" dir "/"))

(defun string-to-dir (path)
  (pathname (strcat (chomp-dir path) "/")))

(defun make-path (path) ; AKA mkdir.
  "This works even if you forget the trailing /"
  (ensure-directories-exist (string-to-dir path))
  #+NIL(meout-now "Ensuring ~a exists~%" path))

(defun write-utf8-line-with-crlf (line stream)
  "Write a full line to a file complete with Carriage Return and Line Feed for Windows systems."
  (write-sequence (babel:string-to-octets line :encoding :utf-8) stream)
  (write-byte 13 stream)
  (write-byte 10 stream))

(defun write-utf8-lines-to-file (lines filename)
  "Saves a list or vector of lines to a file"
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :element-type '(unsigned-byte 8))
    (iter
      (for line in-sequence lines)
      (write-utf8-line-with-crlf line stream))))

(defun write-utf16-line-with-crlf (line stream)
  "Write a full line to a file complete with Carriage Return and Line Feed for Windows systems."
  (write-sequence (babel:string-to-octets line :encoding :utf-16) stream)
  (write-byte 13 stream)
  (write-byte 10 stream))

(defun write-utf16-lines-to-file (lines filename)
  "Saves a list or vector of lines to a file"
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :element-type '(unsigned-byte 8))
    (iter
      (for line in-sequence lines)
      (write-utf16-line-with-crlf line stream))))

(defun print-hash (table &optional (stream t))
  "Prints out a hash in human-readable format."
  #+NIL(maphash #'(lambda (k v) (format stream "~a => ~a~%" k v)) table)
  (maphash #'(lambda (k v) (write-utf8-line-with-crlf (format nil "~a => ~a" k v) stream)) table))

(defun print-table (table &optional (stream t))
  "Prints out a hash in human-readable format with escape characters to make it suitable input to READ."
  #+NIL(maphash #'(lambda (k v) (format stream "~s => ~s~%" k v)) table)
  (maphash #'(lambda (k v) (write-utf8-line-with-crlf (format nil "~s => ~s" k v) stream)) table))

(defun print-table-entry (k v &optional (stream t))
  "Prints out a hash in READ readable format."
  (write-utf8-line-with-crlf (format nil "~s" (list k v)) stream))

(defun dump-table (table &optional (output t))
  "Prints out a hash in lisp readable format."
  (iter (for (k v) in-hashtable table)
	(print-table-entry k v output)))

(defun table-to-file (table filename)
  "Dumps a hash to a file."
  (with-output-to-file (output filename :if-exists :supersede :element-type '(unsigned-byte 8))
    (iter (for (k v) in-hashtable table)
	  (print-table-entry k v output))))

(defun file-to-table (filename)
  ;;--! NEEDS TESTING AS UTILITY FUNCTION.
  "Reads in a table previously dumped to a file by table-to-file.

USAGE: (setf archive-table (file-to-table archive-crcs-file))"
  (iter
   (for line in-file filename)
   (with table = (make-table))
   (for (k v)  = line)
   (if k
       (sethash table k v)
       (error "Hit blank line or something.  Current line is:~%'~S'~%" line))
   (finally
    (return table))))

(defun vector-push-extend-list (list vector)
  ;;--! NEEDS TESTING AS UTILITY FUNCTION.
  "Pushes a list on to a vector one item at a time"
  (iter 
    (for item in list) 
    (vector-push-extend item vector)))

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
	      (iter (for element in-sequence array)
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

input-type can be :list :array :strings :vector :lines :file :stream :sequence :keys
or hashkeys

output-type can be :list :sequence :array :strings :vector :lines 

Examples:

Dump a file line by line into a vector with CR/LF stripped off the ends:

 (convert-by-lines :file :vector some-file)

Dump a file into a list of forms:

 (convert-by-lines :file :list some-file :reader 'read)

Dump a file line by line into a hash table where each line is a key and each
value is true.  Useful for dumping an old list of files, then checking to see
which files on the system were not there before:

 (convert-by-lines :file :keys some-file)

Do CR LF stripping on a list of strings:

 (convert-by-lines :list :list some-list)

Convert a list of strings into hash keys:

 (convert-by-lines :list :keys some-list)

etc."
  (with-gensyms (line junk output)
    `(iter ,(ecase in-type
;; For each input type pick the way to handle it.
		   (:list     `(for ,line in ,input))
		   ((:array :strings :vector :lines) `(for ,line in-vector ,input))
		   (:file     `(for ,line in-file ,input using ',reader))
		   (:stream   `(for ,line in-stream ,input using ',reader))
		   (:sequence `(for ,line in-sequence ,input))
		   ((:keys :hashkeys) `(for (,line ,junk) in-hashtable ,input)))
	   ,@(ecase out-type
;; For each output type pick the way to handle it.
		    ((:list :sequence) `((collecting ,(if chomp `(chomp ,line) line))))
		    ((:array :strings :vector :lines)
		     `((with ,output = 
			     (make-a-vector 0 :element-type 'string))
		       (vector-push-extend 
			,(if chomp `(chomp ,line) line) ,output)
		       (finally (return ,output))))
		    ((:hash :keys :hashkeys :hashtable)
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


;;---------------------------------------------------------------------------
;; BIT and BYTE Manipulation
;;---------------------------------------------------------------------------

	
#+nil(
;;---------------------------------------------------------------------------
;; From http://www.lispforum.com/viewtopic.php?f=2&t=1205
(defun bit-vector-to-integer (bits)
  "Written by wvxvw on http://www.lispforum.com/viewtopic.php?f=2&t=1205
Converts a bit-vector to a regular integer."
   (reduce #'(lambda (a b) (+ (ash a 1) b)) bits))

#+nil(defun bit-vector->integer (bit-vector)
  "Create a positive integer from a bit-vector."
  (reduce #'(lambda (first-bit second-bit) (+ (* first-bit 2) second-bit))
          bit-vector))

#+nil(defun integer->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (coerce (integer->bit-list integer) 'bit-vector)))

(defun digits(n &optional (base 10))
  (multiple-value-bind (q r) (floor n base)
    (if (and (zerop q) (zerop r)) nil
	(cons r (digits q base)))))

(defun reverse-digits(digits &optional (base 10) (power 0))
   (if (null digits) 0
       (+ (* (car digits) (expt base power)) 
	  (reverse-digits (cdr digits) base (incf power)))))
;; END From http://www.lispforum.com/viewtopic.php?f=2&t=1205
;;---------------------------------------------------------------------------


;;---------------------------------------------------------------------------
;; From http://lisptips.com/post/44261316742/how-do-i-convert-an-integer-to-a-list-of-bits
(defun list-of-bits (integer)
  (let ((bits '()))
    (dotimes (index (integer-length integer) bits)
      (push (if (logbitp index integer) 1 0) bits))))

(defun list-of-bits (integer)
  (let ((i integer)
        (bits '()))
    (dotimes (j (integer-length integer) bits)
      (push (logand i 1) bits)
      (setf i (ash i -1)))))

(defun list-of-bits (integer)
  (let ((mask 1)
        (bits '()))
    (dotimes (i (integer-length integer) bits)
      (push (if (logtest mask integer) 1 0) bits)
      (setf mask (ash mask 1)))))

(defun list-of-bits (integer)
  (let ((bits '()))
    (dotimes (position (integer-length integer) bits)
      (push (ldb (byte 1 position) integer) bits))))
;; END From http://lisptips.com/post/44261316742/how-do-i-convert-an-integer-to-a-list-of-bits
;;---------------------------------------------------------------------------



(defun bits-to-integer (bits)
  "Written by wvxvw on http://www.lispforum.com/viewtopic.php?f=2&t=1205
Converts a bit-vector to a regular integer."
  (reduce #'(lambda (a b) (+ (ash a 1) b)) bits))

(defun integer-to-bits (integer &optional (bits (make-array 1 :element-type '(unsigned-byte 1) :adjustable t :fill-pointer 0)))
  #+test(format t "~a~%" bits)
  (multiple-value-bind (i r) (truncate integer 2)
    (vector-push-extend r bits)
    (when (> i 0) (integer-to-bits i bits)))
  bits)
  


#+nil(defun bit-vector-to-integer (bit-vector)
  (iter (for bit in-vector bit-vector)
	(accumulate bit by #'(lambda (abit int) (+ (* int 2) abit)))))

)



(defun flatten-or-vector (sequence)
  "Flattens lists, returns a bare vector or a flattened list of things."
  (cond ((vectorp sequence) sequence)
	((and (vectorp (car sequence)) (null (cdr sequence))) (car sequence))
	(t (flatten sequence))))

(defun resize-bytes (in-width out-width &rest bytes)
  "Takes a sequence of bytes of width in-width.
Returns a sequence of bytes of width out-width.
Special thanks to Juri Pakaste <juri@iki.fi> for the algorithym for this I
took from base64-decode."
  (let ((stored-bits  0)
	(stored-count 0)
	(output (make-array '(0) :element-type `(unsigned-byte ,out-width)
			    :adjustable t :fill-pointer 0)))
    (setf bytes (flatten-or-vector bytes))
    (loop for i from 0 below (length bytes)
       do (progn
	    (setf stored-bits 
		  (logior (ash stored-bits in-width) (elt bytes i)))
	    (incf stored-count in-width)
	    (do () ((< stored-count out-width) nil)
	      (decf stored-count out-width)
	      ;; Set the next element of output to the next out-width bits:
	      (vector-push-extend (ldb (byte out-width stored-count) stored-bits) output)
	      ;; chop any written bits off the top of stored-bits
	      (setf stored-bits (ldb (byte stored-count 0) stored-bits)))))
    (when (> stored-count 0)		; The final byte
      (vector-push-extend (ldb (byte out-width 0) stored-bits) output))
    (nreverse output)))




(defun eltnil (sequence index)
  "Returns the given element of a sequence, or NIL if it is past the end."
  (if (< index (length sequence)) (elt sequence index) nil))


(defun slice (sequence index number)
  "Returns the slice of NUMBER elements of SEQUENCE starting at INDEX.
Will substitute NIL for any missing elements."
  (loop for i from index below (+ index number)
       collecting (eltnil sequence i)))



(defun elt-shift (list index shift)
  "Returns an element of a squence bit shifted by shift, or 0 for out of bounds.
I guess this could have been useful in writing some type of byte-conversion?"
  (ash (if (< index (length list)) (elt list index) 0) shift))


(defun join-bytes (byte-width byte-count &rest bytes)
  "Joins byte-count bytes of width byte-width into a single integer of width
byte-count times byte-width"
  (progn
    (setf bytes (flatten-or-vector bytes))
    (apply #'logior
	   (loop for index from 0 below byte-count
	      collecting (ash (or (eltnil bytes index) 0) 
			      (* byte-width (- byte-count index 1)))))))


(defun split-bytes (byte-width byte-count big-width big-byte)
  "Splits a large byte into multiple smaller bytes."
  (loop for index from 0 below byte-count
       collecting (ldb (byte byte-width (- big-width (* byte-width (+ index 1)))) big-byte)))

;;--! Testing:
#+nil(format t "#x~x" (join-bytes 6 4 '(63 63)))
#+nil(format t "~a" (split-bytes 6 4 24 #xabcdef))




;;---------------------------------------------------------------------------
;; File and Directory manipulation stuff.
;;---------------------------------------------------------------------------

;; From https://stackoverflow.com/questions/10049338/common-lisp-launch-subprocess-with-different-working-directory-than-lisp-proces
;; To run external programs (like your python process portably) see external-program. To change the current working directory, use this slightly modified (public domain) function cwd from the file http://files.b9.com/lboot/utils.lisp, which is reproduced below.

;; The files are now at http://mirror.informatimago.com/lisp/files.b9.com/lboot/index.html

(defun cwd (&optional dir)
  "Change directory and set default pathname"
  (cond
   ((not (null dir))
    (when (and (typep dir 'logical-pathname)
           (translate-logical-pathname dir))
      (setq dir (translate-logical-pathname dir)))
    (when (stringp dir)
      (setq dir (parse-namestring dir)))
    #+allegro (excl:chdir dir)
    #+clisp (#+lisp=cl ext:cd #-lisp=cl lisp:cd dir)
    #+(or cmu scl) (setf (ext:default-directory) dir)
    #+cormanlisp (ccl:set-current-directory dir)
    #+(and mcl (not openmcl)) (ccl:set-mac-default-directory dir)
    #+openmcl (ccl:cwd dir)
    #+gcl (si:chdir dir)
    #+lispworks (hcl:change-directory dir)
    #+sbcl (sb-posix:chdir dir)
    (setq cl:*default-pathname-defaults* dir))
   (t
    (let ((dir
       #+allegro (excl:current-directory)
       #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
       #+(or cmu scl) (ext:default-directory)
       #+sbcl (sb-unix:posix-getcwd/)
       #+cormanlisp (ccl:get-current-directory)
       #+lispworks (hcl:get-working-directory)
       #+mcl (ccl:mac-default-directory)
       #-(or allegro clisp cmu scl cormanlisp mcl sbcl lispworks) (truename ".")))
      (when (stringp dir)
    (setq dir (parse-namestring dir)))
      dir))))


(defun windows-namestring (file)
  (let ((filename (uiop/filesystem:native-namestring file)))
    (cl-ppcre:regex-replace-all "[\\\\/]+" filename "/")))

(defun glob (glob)
  "Takes a wildcard input and returns all files that match the wildcard."
  (when glob
    (mapcar #'windows-namestring
	    #+allegro   (directory glob :directories-are-files nil :follow-symbolic-links nil)
	    #+abcl      (directory glob :resolve-symlinks nil)
	    #+ccl       (directory glob :directories t :follow-links nil)
	    #+clasp     (directory glob :resolve-symlinks nil)
	    #+clisp     (directory glob :full t :if-does-not-exist :keep)
	    #+cmucl     (directory glob :truenamep nil)
	    #+scl       (directory glob :truenamep nil)
	    #+lispworks (directory glob :directories t :link-transparency nil)
	    #+ecl       (directory glob :resolve-symlinks nil)
	    #+mkcl      (directory glob)
	    #+sbcl      (directory glob :resolve-symlinks nil))))


(defun globs (&rest globs)
  "Takes wildcards as input and returns all files that match the wildcards."
  (when globs
    (sort (remove-duplicates (mapcar #'chomp-dir (flatten (mapcar #'glob (flatten globs)))) :test 'equal) #'string-lessp)))





#+nil(
;; with-cmd, get-cwd, and cwd are all from cwd.lisp by Leo Zovic AKA inaimathi
;; This was released without any copyright statement or license at:
;; https://github.com/inaimathi/cl-cwd/blob/master/cl-cwd.lisp

;; Thank you, krzysz00 and Clayton Stanley from
;; (http://stackoverflow.com/questions/10049338/common-lisp-launch-subprocess-with-different-working-directory-than-lisp-proces)
(defmacro with-cwd (dir &body body)
  (with-gensyms (original-directory)
    `(let ((,original-directory (get-cwd)))
       (unwind-protect (progn
			 (cwd ,dir)
			 ,@body)
	 (cwd ,original-directory)))))

(defun get-cwd ()
  "Get the current directory"
  (let ((dir
	 #+allegro (excl:current-directory)
	 #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
	 #+(or cmu scl) (ext:default-directory)
	 #+sbcl (sb-unix:posix-getcwd)
	 #+CCL (ccl:current-directory)
	 #+cormanlisp (ccl:get-current-directory)
	 #+lispworks (hcl:get-working-directory)
	 #+mcl (ccl:mac-default-directory)
	 #-(or allegro clisp cmu scl cormanlisp mcl sbcl lispworks CCL) (truename ".")))
    (if (stringp dir)
	(parse-namestring dir)
	dir)))



(defun cwd (&optional dir)
  "Change directory and set default pathname"
  (cond
    ((not (null dir))
     (when (and (typep dir 'logical-pathname)
		(translate-logical-pathname dir))
       (setq dir (translate-logical-pathname dir)))
     (when (stringp dir)
       (setq dir (parse-namestring dir)))
     #+allegro (excl:chdir dir)
     #+clisp (#+lisp=cl ext:cd #-lisp=cl lisp:cd dir)
     #+(or cmu scl) (setf (ext:default-directory) dir)
     #+cormanlisp (ccl:set-current-directory dir)
     #+(and mcl (not openmcl)) (ccl:set-mac-default-directory dir)
     #+openmcl (ccl:cwd dir)
     #+gcl (si:chdir dir)
     #+lispworks (hcl:change-directory dir)
     #+sbcl (sb-posix:chdir dir)
     (setq cl:*default-pathname-defaults* dir)
     (get-cwd))
    (t (get-cwd))))



)
