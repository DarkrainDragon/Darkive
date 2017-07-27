;;;;
;;;; altered-fad.lisp --- File and Directory manipulation.
;;;;


;;; Copyright (C) 2012-2013, Adrian Xander Blackstone <DarkrainDragon@gmail.com> All rights reserved.
;;; Copyright (C) 2012-2013, Rian Jensen <DarkrainDragon@gmail.com> All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.





;;---------------------------------------------------------------------------
;; CODE BEGINS HEREISH
;;---------------------------------------------------------------------------

(setf *print-pretty* t) ; Print lists out in a pretty way.


(ql:quickload 'trivial-features)
(ql:quickload 'alexandria)
(ql:quickload 'cl-ppcre)
(ql:quickload 'iterate)
(ql:quickload 'cl-base64)


(in-package :cl-user)

(defpackage :altered-fad
  (:use :cl :iterate)
  (:import-from :alexandria with-gensyms once-only with-output-to-file with-input-from-file)
  (:import-from :cl-ppcre all-matches-as-strings split)
  (:documentation "Altered version of cl-fad that guarantees that the directory function will not follow links unless told to.")
  (:export))

(in-package :altered-fad)



;;---------------------------------------------------------------------------
;; UTILITY STUFF:
;;---------------------------------------------------------------------------



(defun stat-file-plist-follow-links (filename)
  "Uses SBCL's FFI to stat a file, returning a P-list of the file properties.
Returns a human readable Plist of file information."
  (when (file-exists-p filename)
    #-sbcl(format t "stat-file-plist-follow-links only works on SBCL for now.~%")
    #+sbcl(let ((stat (sb-posix:stat (coerce filename 'simple-string))))
	    (list
	     :mode  (sb-posix:stat-mode  stat)
	     :ino   (sb-posix:stat-ino   stat)
	     :dev   (sb-posix:stat-dev   stat)
	     :nlink (sb-posix:stat-nlink stat)
	     :uid   (sb-posix:stat-uid   stat)
	     :gid   (sb-posix:stat-gid   stat)
	     :size  (sb-posix:stat-size  stat)
	     :rdev  (sb-posix:stat-rdev  stat)
	     :atime (sb-posix:stat-atime stat)
	     :mtime (sb-posix:stat-mtime stat)
	     :ctime (sb-posix:stat-ctime stat)))))

(defun stat-file-plist (filename)
  "Uses SBCL's FFI to stat a file, returning a P-list of the file properties.
Returns a human readable Plist of file information.
This version will stat the link itself instead of what the link points to
when filename is a link."
  (when (file-exists-p filename)
    #-sbcl(format t "stat-file-plist only works on SBCL for now.~%")
    #+sbcl(let ((stat (sb-posix:lstat (coerce filename 'simple-string))))
	    (list
	     :mode  (sb-posix:stat-mode  stat)
	     :ino   (sb-posix:stat-ino   stat)
	     :dev   (sb-posix:stat-dev   stat)
	     :nlink (sb-posix:stat-nlink stat)
	     :uid   (sb-posix:stat-uid   stat)
	     :gid   (sb-posix:stat-gid   stat)
	     :size  (sb-posix:stat-size  stat)
	     :rdev  (sb-posix:stat-rdev  stat)
	     :atime (sb-posix:stat-atime stat)
	     :mtime (sb-posix:stat-mtime stat)
	     :ctime (sb-posix:stat-ctime stat)))))



#+sbcl(defun posix-ls-bare (dirname)
  "Uses SBCL's FFI to read the contents of a directory.
This is a barebones version, intended to be wrapped externally to localize
where I have to put code for each implementation's FFI."
  (if (file-access-p dirname)
      (if (readable-p dirname)
	  #-sbcl(format t "posix-ls-bare only works on SBCL for now.~%")
	  #+sbcl(let ((dir (sb-posix:opendir (coerce dirname 'simple-string))))
		  (unwind-protect ; Ensure the directory is closed after use.
		       (iter 
			(for file-id = (sb-posix:readdir dir))
			(until (sb-alien:null-alien file-id))
			(for filename = (sb-posix:dirent-name file-id))
			(collect filename))
		    (sb-posix:closedir dir)))
	  (format t "Could not read directory '~a'~%" dirname))
      (format t "Could not find directory '~a'~%" dirname)))


(defun posix-cd (dirname)
  "Change directory to the specified path.
Update *default-pathname-defaults* so that file-access-p still works."
  (when (file-access-p dirname)
    #+:sbcl  (progn (sb-posix:chdir (coerce dirname 'string))
		    (setf *default-pathname-defaults* (string-to-dir dirname))
		    (meout-now "*default-pathname-defaults* is now (~a)~%" *default-pathname-defaults*))
    #+:clisp (ext:cd dirname)
    #+:ccl(:cd dirname)
    #+(and (not (or :ccl :sbcl :clisp)) :cffi) 
        (if (file-access-p dirname) (%chdir dirname)
							  (setf *default-pathname-defaults* truepath))))

(defun posix-chmod (filename mode)
  "Change the mode of the file.  Note that to get the behavior you want
it is advisable to input the mode in #o format."
    (when (file-access-p filename)
      #+:sbcl  (sb-posix:chmod (coerce filename 'simple-string) mode)
      #+:clisp (ext:chmod filename mode)
      #+(and (not (or :sbcl :clisp)) :cffi) (%chmod filename mode)))

(defun posix-ls (dirname 
		 &key (prepend-path dirname) (skip-dots t))
  "Uses SBCL's FFI to read the contents of a directory.
By default prepends the directory to the file name.  Set to NIL to turn off,
or any other needed value.
:skip-dots can turn on or off whether the . and .. are returned."
  (if (readable-p dirname)
      #-sbcl(format t "posix-ls only works on SBCL for now.~%")
      #+sbcl(let* ((dir      (sb-posix:opendir (coerce dirname 'simple-string)))
		   (just-dir (chomp-dir prepend-path)))
	      (unwind-protect ; Ensure the directory is closed after use.
		   (iter 
		     (for file-id = (sb-posix:readdir dir))
		     (until (sb-alien:null-alien file-id))
		     (for filename = (sb-posix:dirent-name file-id))
		     (unless (and skip-dots (find filename '("." "..") :test #'string-equal))
		       (collect (if prepend-path 
				    (strcat just-dir "/" filename)
				    filename))))
		(sb-posix:closedir dir)))
      ;;--! This needs an option or wrapper for signalling an error/log entry.
      (if (file-access-p dirname)
	  (format t "Could not read directory '~a'~%" dirname)
	  (format t "Could not find directory '~a'~%" dirname))))

(defun glob (glob)
  "Takes a wildcard input and returns all files that match the wildcard."
  ;;--! Apparently namestring does not return a simple string, but copy-seq will.
  (when glob
    #-sbcl(format t "glob only works on SBCL for now.~%")
    #+sbcl(mapcar #'namestring (directory glob :resolve-symlinks nil))))

(defun globs (&rest globs)
  "Takes wildcards as input and returns all files that match the wildcards."
  (when globs
    (sort (remove-duplicates (mapcar #'chomp-dir (flatten (mapcar #'glob (flatten globs)))) :test 'equal) #'string-lessp)))

;;---------------------------------------------------------------------------
;; Wrappers for stuff that requires SBCL and/or CFFI.  Currently depends on:
;; stat-file-plist, posix-ls, posix-ls-bare, posix-cd
;; %access %chmod %chdir %getcwd
;;---------------------------------------------------------------------------

(defun posix-ls-sorted (dirname 
		 &key (prepend-path dirname) (skip-dots t))
  (sort (posix-ls dirname :prepend-path prepend-path :skip-dots skip-dots) #'string-lessp))



(defun posix-ls-wrapper (dirname 
			 &key (prepend-path dirname) (skip-dots t))
  "Uses posix-ls-bare to read the contents of a directory.
By default prepends the directory to the file name.  Set to NIL to turn off,
or any other needed value.
:skip-dots can turn on or off whether the . and .. are returned."
  (iter 
    (for filename in (posix-ls-bare dirname))
    (unless (and skip-dots (find filename '("." "..") :test #'string-equal))
      (collect (if prepend-path 
		   (strcat (chomp-dir prepend-path) "/" filename)
		   filename)))))

(defun file-mode-to-type (mode)
  "Translated File Mode information:

File type = (logand #o017 (ash MODE -12))

Socket            = #o014 = 12 = #b1100
Symbolic Link     = #o012 = 10 = #b1010
Regular File      = #o010 =  8 = #b1000
Block Special     = #o006 =  6 = #b0110
Directory         = #o004 =  4 = #b0100
Character Special = #o002 =  2 = #b0010
FIFO Special      = #o001 =  1 = #b0001"
  (when mode
    (case (logand #o017 (ash mode -12))
      (#o014     'socket)
      (#o012     'link)
      (#o010     'file)
      (#o006     'block-special)
      (#o004     'directory)
      (#o002     'character-special)
      (#o001     'fifo-special)
      (otherwise 'unknown))))

(defun file-type (filename)
  "Returns a symbol indicating the type of the file."
  (file-mode-to-type (getf (stat-file-plist filename) :mode)))

(defun file-mode-to-short-type (mode)
  "Translated File Mode information:

File type = (logand #o017 (ash MODE -12))

Socket            = #o014 = 12 = #b1100
Symbolic Link     = #o012 = 10 = #b1010
Regular File      = #o010 =  8 = #b1000
Block Special     = #o006 =  6 = #b0110
Directory         = #o004 =  4 = #b0100
Character Special = #o002 =  2 = #b0010
FIFO Special      = #o001 =  1 = #b0001"
  (when mode
    (case (logand #o017 (ash mode -12))
      (#o014     'sock)
      (#o012     'link)
      (#o010     'file)
      (#o006     'bloc)
      (#o004     'dir)
      (#o002     'char)
      (#o001     'fifo)
      (otherwise 'unkn))))

(defun file-short-type (filename)
  "Returns a symbol indicating the type of the file."
  (file-mode-to-short-type (getf (stat-file-plist filename) :mode)))

(defun file-size (filename)
  "Returns the size of the file."
  (getf (stat-file-plist filename) :size))

(defun file-mode (filename)
  "Returns the mode of the file."
  (getf (stat-file-plist filename) :mode))

(defun file-mode-o (filename)
  "Returns the mode of the file in Octal form."
  (format nil "~6,'0o" (file-mode filename)))

(defun dir-p (filename &key (follow-links nil))
  "Returns true if the file is a directory
or a link that is a directory if follow-links is true,
false otherwise."
  (when (file-access-p filename) ; Broken links, missing files, etc are not directories.
    (let ((type (file-type filename)))
      ;;--! NOTE:  I can not test this part on windows.
      (or (eql type 'directory) 
	  (and follow-links (eql type 'link) (dir-p (file-access-p filename)))))))

(defun link-p (filename)
  "Returns true if the file is a link false otherwise.
Returns NIL without error if given a non-extant file name."
  (when (eql 'link (file-type filename)) t))

(defun ls-file (filename)
  "Prints out Unix ls style information for a single file.
Useful for testing directory walkers."
  (let ((stats (stat-file-plist filename)))
    (if stats 
	(format t "~4o ~3,d ~9,d ~9,d ~12,d ~a ~4,a ~a~%" (logand #o7777 (getf stats :mode)) 
		(getf stats :nlink) (getf stats :uid) (getf stats :gid) 
		(getf stats :size) (posix-time-string (getf stats :mtime)) (file-short-type filename) filename)
	(format t "No such file or directory:  ~a~%" filename))))

(defun ll (directory)
  (mapcar #'ls-file (posix-ls-sorted directory)))



;;; The function posix-walk-directory is taken and modified from cl-fad which was written by Peter Seibel and Dr. Edmund Weitz.
;;; Copyright (c) 2004, Peter Seibel.  All rights reserved.
;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.
;;; Copyright (c) 2012-2013, Adrian Xander Blackstone.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(defun posix-walk-directory
    (dirname fn &key process-dirs (follow-links nil) (process-links t)
		  (if-does-not-exist :error) 
		  (test (constantly t)))
  "Recursively applies the function FN to all files within the
directory named by the non-wild pathname designator DIRNAME and all of
its sub-directories.  FN will only be applied to files for which the
function TEST returns a true value.  If DIRECTORIES is not NIL, FN and
TEST are applied to directories as well.  If DIRECTORIES is :DEPTH-FIRST,
FN will be applied to the directory's contents first.  If
DIRECTORIES is :BREADTH-FIRST and TEST returns NIL, the
directory's content will be skipped. IF-DOES-NOT-EXIST must be
one of :ERROR or :IGNORE where :ERROR means that an error will be
signaled if the directory DIRNAME does not exist."
  (labels ((walk (filename)
             (cond ((dir-p filename :follow-links follow-links)
		    (cond ((not process-dirs) (dolist (file (posix-ls-sorted filename)) (walk file)))
			  ((eql process-dirs :breadth-first)
			   (when (funcall test filename)
			     (funcall fn filename)
			     (dolist (file (posix-ls-sorted filename)) (walk file))))
			  ;; :DEPTH-FIRST by default
			  (t (dolist (file (posix-ls-sorted filename)) (walk file))
			     (when (funcall test filename)
			       (funcall fn filename)))))
		   ((link-p filename)  (when (and process-links (funcall test filename)) (funcall fn filename)))
		   ((funcall test filename) (funcall fn filename)))))
    (if (file-access-p dirname)
	(walk dirname)
	(when (eql if-does-not-exist :error) (error "File ~s does not exist." dirname)))
    (values)))




#+nil(posix-walk-directory "PROJECTS" 'ls-file :process-dirs :breadth-first)



