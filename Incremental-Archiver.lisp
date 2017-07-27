;;;
;;; Darkrain-Archive.lisp --- Archival utility with some Posix bindings.
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
(setf *print-circle* t) ; This causes the REPL to detect and print circular lists.  I have no idea how much this slows things down or what the downside is.



(ql:quickload 'trivial-features)
(ql:quickload 'alexandria)
(ql:quickload 'cl-ppcre)
(ql:quickload 'iterate)
(ql:quickload 'cl-base64)


;; NOTE:  To get cffi-grovel to work on Windows it is neccessary to set the
;; environment variable CC to the C compiler's location.
;; Setting CC lets you bypass the hard coded location of C:\MSYS\bin\
;; Unfortunately this pretty much has to be set by the script calling the
;; Common Lisp executable.
;; So basically SBCL has to be called by a batch file.
(ql:quickload 'cffi-grovel)
(ql:quickload 'cffi)

(ql:quickload 'ironclad) 

;;Not gonna be able to autoload it for a while
(load "PROJECTS/Dark-Utilities.lisp")


(in-package :cl-user)

(defpackage :darkrain-archive
  (:use :cl :iterate :cffi :cffi-grovel :dark-utilities)
  ;;(:use :cl :iterate)
  (:import-from :alexandria with-gensyms once-only with-output-to-file with-input-from-file)
  (:import-from :cl-ppcre all-matches-as-strings split)
  ;;(:import-from :ironclad digest-file)
  (:import-from :cl-base64 usb8-array-to-base64-string)
  (:documentation "Utilitiy for archiving stuff, with incremental backups based on the files' checksums.")
  (:export))

(in-package :darkrain-archive)



;;---------------------------------------------------------------------------
;; UTILITY STUFF:
;;---------------------------------------------------------------------------
;;--! Replaced by: (load "DARK/Dark-Utilities.lisp")



;;---------------------------------------------------------------------------
;;CFFI wrappers needed by other stuff. 
;;---------------------------------------------------------------------------

;;--! I need to obsolete everything CFFI and IRONCLAD to make stuff work on
;;--! android with ECL.

;;--! Looks like it'll be CCL from here on out, so IRONCLAD will work but 
;;--! CFFI would be a worthless nightmare.

(define-foreign-library libc
  (:unix    (:or      "libc.so.6"))
  (:windows (:or      "msvcrt"))
  (t        (:default "msvcrt")))
   
(use-foreign-library libc)



(defcfun (#+windows"_getcwd" #+unix"getcwd" %getcwd) :string)

(defcfun (#+windows"_umask"  #+unix"umask"  %umask)  :int
  "Sets the umask and returns the old one"
  (mode     :int))

;;(cffi:defcfun (#+windows"_chdir"  #+unix"chdir"  %chdir)  :int 
;;  (pathname :string))

(defcfun (#+windows"_chdir"  #+unix"chdir"  %chdir)  :int 
  (pathname :string))

(defcfun (#+windows"_rmdir"  #+unix"rmdir"  %rmdir)  :int 
  (pathname :string))

(defcfun ("rename" %rename) :int 
  "Does this have any advantage over CL:RENAME-FILE?."
  (oldname  :string)
  (newname  :string))

(defcfun (#+windows"_chmod"  #+unix"chmod"  %chmod)  :int 
  (pathname :string)
  (mode     :int))

(defcfun (#+windows"_mkdir"  #+unix"mkdir"  %mkdir)  :int 
  (pathname :string)
  (mode     :int))

(defcfun ("getenv" getenv) :string
  (name :string))





(defcfun (#+windows"_access" #+unix"access" %access) :int
  "Test to see if a file exists and can be accessed by the current process."
  (pathname :string) 
  (mode     :int))

(defun access (filename &optional (mode 0))
  "Wrapper for %access C function.  Mode defaults to 0 to check existance."
  (zerop (%access filename mode)))

(defun file-exists-p (filename)
  "Checks to see if a file exists"
  (access filename))

(defun file-access-p (filename)
  "Still trying to figure out what the modes mean and what to use for tests.
0 Visible at all.
1 Executable
2 Writable.
4 Readable.
8 Not discernible on a Unix machine WTF this is for.

This function takes a file name and returns the mode that you can access the
file at. 0 presumably means you can see that it exists but can't do anything.
NIL means there is no such file that the process can see."
  (when (access filename 0) ; Return NIL if no file.
      (iter (for mode in `(1 2 4 8))
	(when (access filename mode)
	  (sum mode into sum))
	(finally (return sum)))))

(defun readable-p (filename)
  "Checks to see if a file is readable.
Note that this is unreliable on Windows because it can not check OS level
permissions."
  ;;--! Need to verify if 1 is the right mode to check.
  (access filename 1))

(defun getcwd () (%getcwd))

;;---------------------------------------------------------------------------
;;SBCL ONLY Extensions to to get full & correct recursive file operations
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

;;--! NOTE: sb-posix:readlink exists only on non-windows boxes, probably
;;--! because links are all fucked up on windows.
;;--! By fucked up I mean they exist for NTFS partitions only, in a bizarre way.


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

(defun posix-walk-directory
    (dirname fn &key process-dirs (follow-links nil) (process-links t)
		  (if-does-not-exist :error) 
		  (test (constantly t)))
;;; Copyright (c) 2004, Peter Seibel.  All rights reserved.
;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.
;;; Copyright (c) 2012-2013, Adrian Zander Blackstone.  All rights reserved.

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




;;---------------------------------------------------------------------------
;;Replacement for llr_sha256_delta
;;---------------------------------------------------------------------------

#+nil(defun file-crc (filename)
  "Takes a file name and returns the cryptographic collision resistant hash.
Directories always produce a hash of DIRECTORY.
Called CRC to make life easy when I need to update what hash to use."
  (if (dir-p filename) 
      "DIRECTORY"
      (cl-base64:usb8-array-to-base64-string 
       (ironclad:digest-file :sha512 filename))))

(defun file-crc (filename)
  "Returns the cryptographic collision resistant hash for a given filename."
  (cl-base64:usb8-array-to-base64-string 
   (ironclad:digest-file :sha512 filename)))

(defun file-hashkey (filename)
  "Takes a file name and returns the size and cryptographic collision 
resistant hash.
Directories always produce a size of 0 and a hash of DIRECTORY.
Empty files always produce a size of 0 and a hash of EMPTY."
  (if (dir-p filename) 
      (list 0 'directory)
      (if (zerop (file-size filename))
	  (list 0 'emtpy)
	  (list (file-size filename) (file-crc filename)))))







(defun file-perms (filename) 
;;--! May be obsolete shortly.
  "Uses SBCL's FFI to stat a file, returning a list of the file's 
uid, gid, mode, type and filename."
  (let ((stats (stat-file-plist filename)))
    (list (format nil "~6,'0o" (getf stats :mode))
	  (getf stats :uid) (getf stats :gid) 
	  (file-mode-to-type (getf stats :mode)) filename)))

(defun process-crc-listing (archived-crcs-file removed-list-file)
;;--! This function should be redundant shortly.
  "Processes the CRC listing file from the existing main backup.
This produces one .removed file that is used to determine which files should
not be restored from the main backup, or that must be removed afterward.
 This returns a hash where the keys are the filenames from the main backup
and the values are a list containing the size and the checksum to verify
whether or not files that do exist have changed."
  (with-output-to-file (rmlist removed-list-file :if-exists :supersede)
    (iter
      (for line in-file archived-crcs-file)
      (with archived-table = (make-table))
      (with count = 0)
      (for (size crc filename) = line)
      ;;(for (size crc filename) = (cl-ppcre:split "," line))
      ;; when line contains 2 commas it should have valid data.
      (when filename
	    (sethash archived-table filename (list size crc))
	    (unless (file-exists-p filename)
	      (incf count)
	      ;;(format rmlist "sudo rm~:[~;dir~] ~a~%" (dir-p filename) filename)))
	      (write filename :stream rmlist)
	      (format rmlist "~%")))
      (finally
       (format t "There were ~a removed files.~%" count)
       (finish-output nil) ; NIL Denotes STDIO for FINISH-OUTPUT
       (return archived-table)))))
 
(defun diff-crc-to-archive (filename size-crc)
;;--! This function should be redundant shortly.
  "Compares a file on the file system to information from the main backup.
If the file is new or changed returns a comma seperated string of the size,
crc, and filename.  Otherwise returns NIL."
  (let ((newsize (if (dir-p filename) 
		     0
		     (file-size filename)))
	(newcrc  (file-hashkey filename)))
    (if (and (equal (car size-crc) newsize) 
	     (equal (cadr size-crc) newcrc))
	nil
	(list newsize newcrc filename))))

(defun diff-directory-against-archive 
    (source-dir archive-base &optional (archived-crcs-file nil))
;;--! This function should be redundant shortly.
  "Recurses through a directory noting all differences between the last full
archive and the current state of the directory.  These differences are saved
in files listing which files have changed/added or been removed, as well as
the current state of all file permissions.
2 files listing files to archive are produced to be fed into archival programs."
  (let* ((removed-list-file  (strcat archive-base ".removed"))
	 (changed-crcs-file  (strcat archive-base ".crc"))
	 (changed-list-file  (strcat archive-base ".list"))
	 ;; You can not include directories to archive in a listing passed to tar.
	 (changed-nodir-file (strcat archive-base ".listnodir"))
	 (permissions-file   (strcat archive-base ".permissions"))
	 (permissions    nil)
	 (changed-crcs   nil)
	 (changed-list   (make-a-vector 0 :element-type 'string))
	 (changed-nodir  (make-a-vector 0 :element-type 'string))
	 (archived-table (if archived-crcs-file 
				 (process-crc-listing archived-crcs-file removed-list-file)
				 (make-table)))
	 (files-count 0))
    (flet
	((process-a-file (filename)
	   (let ((diff (diff-crc-to-archive filename (gethash filename archived-table))))
	     (push (file-perms filename) permissions)
	     (incf files-count)
	     (when (zerop (mod files-count 1000)) 
	       (meout-now "Checked ~a files so far~%" files-count))
	     (when diff
	       (push               diff     changed-crcs)
	       (vector-push-extend filename changed-list)
	       (unless (dir-p filename) (vector-push-extend filename changed-nodir))))))
      (meout-now "Checking existing files for changes to (~a):~%~%" source-dir)
      (posix-walk-directory source-dir #'process-a-file :process-dirs :breadth-first)
      (write-objects     (nreverse permissions)  permissions-file)
      (meout-now "There were ~a changed files~%" (length changed-crcs))
      (write-objects     (nreverse changed-crcs) changed-crcs-file)
      (array-to-file-eol changed-list            changed-list-file)
      (array-to-file-eol changed-nodir           changed-nodir-file))))

#+TESTING(diff-directory-against-archive 
	  "lisp" 
	  "E:/backup/INCREMENTALS/lisp-20121102-20121103" 
	  "E:/backup/FULL/lisp-20121102.crc")

#+TESTING(diff-directory-against-archive 
	  "PROJECTS" 
	  "C:/Temp/backup/INCREMENTALS/lisp-20121102-20121103" 
	  nil)







;;---------------------------------------------------------------------------
;;SET UP BACKUP LOCATIONS
;;---------------------------------------------------------------------------

;;--! If I am going to publish this, it should use a configuration file or something to set up the directories.
(defvar *backup-base*           "/Temp/backup")
(defvar *incremental-base*      (strcat *backup-base*   "/INCREMENTALS"))
(defvar *full-backup-base*      (strcat *backup-base*   "/FULL"))
(defvar *obsolete-base*         (strcat *backup-base*   "/OBSOLETE"))
(defvar *obsolete-incrementals* (strcat *obsolete-base* "/INCREMENTALS"))


;;--! Er, maybe I should actually use *log* somewhere?
(defvar *log* t) ;; For now logs just go to STDOUT.

;;(defvar *backup-base*)
;;(defvar *incremental-base*)
;;(defvar *full-backup-base*)
;;(defvar *obsolete-base*)
;;(defvar *obsolete-incrementals*)

(defun set-backup-locations (backup-base)
  (setf *backup-base*           backup-base)
  (setf *incremental-base*      (strcat *backup-base*   "/INCREMENTALS"))
  (setf *full-backup-base*      (strcat *backup-base*   "/FULL"))
  (setf *obsolete-base*         (strcat *backup-base*   "/OBSOLETE"))
  (setf *obsolete-incrementals* (strcat *obsolete-base* "/INCREMENTALS")))

;;(set-backup-locations "/backup")

;;---------------------------------------------------------------------------  

(defun find-latest-full-backup (source-dir)
  "Returns the latest full backup of the given directory 
or NIL if there is none.
This looks for .crc files rather than the actual archives
because the incrementals are checked against the .crc files
to allow the archives to be moved off the machine."
    (car (sort (glob (strcat *full-backup-base* "/" source-dir "-*.crc")) #'string-greaterp)))

(defun get-just-the-date (full-backup-filename)
  "Returns the date of the last full backup."
  ;; The car and last is not just sillyness.  I take the last match because
  ;; if the file actually being backed up contains 8 digits then that will
  ;; also match.  Only the final match before the .crc is the right one.
  (car (last (cl-ppcre:all-matches-as-strings "\\d{8}" full-backup-filename))))

(defun make-archive-name (source-dir)
  "Returns the name of the archive to create.
If there is already a full backup it will be an incremental.
If not it will be a full backup."
  (let ((full-backup-base (strcat *full-backup-base* "/" source-dir))
	(latest-fullback (find-latest-full-backup source-dir)))
    (if latest-fullback
	(strcat *incremental-base* "/" source-dir "-" (get-just-the-date latest-fullback) "-" (today-string))
	(strcat full-backup-base "-" (today-string)))))

(defun list-old-incrementals (source-dir)
  "Returns the list of all old incrementals for a particular backup, so they
can be moved to *obsolete-incrementals*."
  (when (find-latest-full-backup source-dir)
    (glob (strcat *incremental-base* "/" source-dir "-????????-????????.*"))))


#+TESTING(list 'diff-directory-against-archive "lisp" (make-archive-name "lisp") (find-latest-full-backup "lisp"))
#+TESTING(diff-directory-against-archive "lisp" (make-archive-name "lisp") (find-latest-full-backup "lisp"))

;;---------------------------------------------------------------------------
;;END SET UP BACKUP LOCATIONS
;;---------------------------------------------------------------------------




;;---------------------------------------------------------------------------
;; Replacement for baknightly batch file.
;;---------------------------------------------------------------------------





(defun diff-directory-against-archive-of-same-name (dirname)
  "Runs diff-directory-against-archive with the assumption that the archive
name matches the directory name."
  (diff-directory-against-archive 
   dirname (make-archive-name dirname) (find-latest-full-backup dirname)))

;;--! HELLO!  TESTING FUNCTIONS RIGHT HERE:

#+TESTING(diff-directory-against-archive-of-same-name "lisp")

(defun flat-file-p (filename)
  "Returns true if the file exists and is not a directory."
  (and (file-access-p filename) (not (dir-p filename))))

(defun rename-file-overwrite (from to)
  (progn
    (when (file-access-p to) (delete-file to))
    (rename-file from to)))

;;--! Needs more testing.
(defun move-file (from to)
  "This will move a single file to a destination file or directory.  
If the source is a directory then that directory will be recursed down and 
all files within will be copied to the destination directory.  If the source 
is a directory and the destination is a file then move_file() will abort.

Usage:	(move-file source_file destination_file)"
  (when from
    #+nil(format t "(move-file ~s ~s)~%" from to)
    (cond
      ((all-matches-as-strings "\(^|/\)..?$" from)
       ;; Do not process . or ..
       nil)				
      ((not (file-access-p from)) 
       (format t "Source file ~s does not exist to be copied to ~s" from to))
      ((and (dir-p from) (flat-file-p to))
       (format t "Can't move a directory ~s to a flat file ~s" from to))
      ((string-equal from to)
       (format t "Source and Destination are both '$to', returning success."))
      ((dir-p to) 
       ;; Directory destination.  Move the files or directories into the destination directory.
       (let ((from-base (car (last (cl-ppcre:split "/" from)))))
	 (rename-file-overwrite from (strcat to "/" from-base))))
      (t 
       ;; Source is not a directory and Destination is a file or non-extant.  Simply move and overwrite if needed.
       (rename-file-overwrite from to)))))

;;--! Needs more testing.
(defun move-files (raw-from to)
  "This will recursively move a single file to a destination file or 
directory or a glob or globs of files to a destination directory.

Usage:
	(move-files source_file   destination_file)
	(move-files source_file   destination_dir)
	(move-files source_glob   destination_file) # This will abort if the source matches more than 1 file.
	(move-files source_globs  destination_dir)"
  (when raw-from
    (let* ((from (globs raw-from)))
      #+nil(format t "(move-files ~s ~s)~%" raw-from to)
      (cond
	((null (car from)) 
	 (progn 
	   (format t "No files matching ~s were found." raw-from)
	   (return-from move-files nil)))
	((and (> (length from) 1) (flat-file-p to))
	 (error "Can't move multiple files ~s to an existing flat file ~s" from to))
	((and (> (length from) 1) (not (file-access-p to)))
	 ;; If more than one file is going to be moved to $to then it should be created as a directory.
	 (make-path to))) 
      (iter 
	(for file in from) 
	(move-file file to)))))
      








;;#+nil(documentation 'sb-ext:run-program 'function)



#+nil(defun sh (&rest commands)
  (with-output-to-string (stream)
    ;;(run-program "/bin/sh" (list "-c" command) :output stream)))
    ;;(sb-ext:run-program "/bin/sh" (list (apply #'strcat "-c " commands)) :output stream)
    (format t "~a" commands) ; Send this to some log file instead of stdout later.
    (format t "!!!! CALL TO /bin/sh IN THIS PROGRAM REALLY NEEDS TO BE FIXED !!!!")))

(defun bash (&rest command)
  (format t "~%NOT Executing Command:  \"~a\"~%" (apply 'join (flatten " " command))))

  
(defun cmd (command &rest args)
    (meout-now "~%Executing Command:  \"~a\"~%" (apply 'join (flatten " " command args)))
    #+sbcl(sb-ext:run-program command (flatten args))
    #+ccl(ccl:run-program command (flatten args))
    )

(defun archive-it (working-directory source-dir &optional (archive-type nil))
  "--! PLEASE INSERT DOCUMENTATION HERE:
Usage: (archive-it working-directory source-dir '7z)"
  (let ((archive-base (make-archive-name       source-dir))
	(full-backup  (find-latest-full-backup source-dir)))
    (posix-cd working-directory)
    #+nil(%chdir working-directory)
    (meout-now "~%Supposed to be in ~a~%Actually in ~a~%~%" working-directory (getcwd))
    (dolist (filename (list *backup-base* *full-backup-base* *incremental-base* *obsolete-base* *obsolete-incrementals*))
      (make-path   filename)
      (posix-chmod filename #o557))
    (move-files (list-old-incrementals source-dir) *obsolete-incrementals*)
    (diff-directory-against-archive source-dir archive-base full-backup)
    (finish-output nil) ; NIL Denotes STDIO for FINISH-OUTPUT
    (if (file-access-p (strcat archive-base ".list"))
	(ecase archive-type
	  ;;--! I need a better way to handle the path to the archivers.
	  (7z      (cmd "/Temp/LispBox/7za.exe" 
			"a" "-t7z" "-m0=lzma2" "-mx=9" "-mfb=64" "-md=32m" "-ms=on" 
			(strcat archive-base ".7z") (strcat "@" archive-base ".listnodir")))
	  ;; Arc a -mt0 -mx -p -hp --encryption=aes-256 -lc768m %*
	  (arc     (cmd "/Temp/LispBox/Arc.exe" 
			"a" "-mt0" "-mx" "-lc768m" 
			(strcat archive-base ".arc") (strcat "@" archive-base ".listnodir")))
	  ;;--! Probably gonna want more than just 7z available.
	  (rar     (bash "rar something"))
	  (zip     (bash "zip something"))
	  (tar.gz  (bash "tar zcvf something.tar.gz"))
	  (tar.xz  (bash "tar acvf something.tar.xz"))
	  (tar.bz2 (bash "tar jcvf something.tar.gz"))
	  ('nil (format t "Skipping any archiving, just getting checksums.~%")))
	(format t "~%~%No files were added or updated since the full backup~%~%"))
    ;; The par2 needs to happen if any files were added, updated, or removed.
    (if (or (file-access-p (strcat archive-base ".list")) (file-access-p (strcat archive-base "-rm.sh")))
	(cmd "/Temp/LispBox/par2.exe" "c" "-r10" "-n1" archive-base (strcat archive-base "*")))
    (bash "sudo chown " (getenv "SUDO_USER") ":" (getenv "SUDO_USER") " " (strcat archive-base "*"))))

#+TESTING(archive-it "/Temp/LispBox" "PROJECTS" '7z)
#+TESTING(archive-it "C:/Temp/LispBox" "MinGW" 'nil)
#+TESTING(archive-it "/Temp" "LispBox" '7z)




























;;===========================================================================
;;N     N  EEEEEE  W     W
;;NN    N  E       W     W
;;N N   N  E       W     W
;;N  N  N  EEEEEE  W     W
;;N   N N  E       W  W  W
;;N    NN  E       W W W W
;;N     N  E       WW   WW
;;N     N  EEEEEE  W     W
;;===========================================================================



;;===========================================================================
;; NEW STYLE ARCHIVE WHERE FILES CAN BE MOVED AND STILL ONLY BE STORED ONCE:
;;===========================================================================

#+nil"
NOTE:  This is actually more current than any above or below documentation.
Started 20141011
Updated 20150423


The goal of restoration is to make the filesystem match the 
incremental-backup table.


The program should be called with the working directory and incremental to 
restore, then deduce the base archive of the incremental.


BACKUP:

Like the old version but only save the first file for each file-hash:

For all instructions below the tables have file-hash as the key and the list 
of file names with that hash as the value.


FULL BACKUP:

-- Make the full-backup table and write it to disk.
-- Output the list of the first file in each value of the full-backup table.
-- Backup those files into an archive.
-- Save a list of the permissions for every file, not just the first.

DONE!



INCREMENTAL BACKUP:

-- Make the incremental-backup table and write it to disk.
-- Output the list of the first file in each value of incremental-backup
that is not in full-backup.
-- Backup the listed files into an archive.
-- Output the list of the first file in each value of full-backup that is 
not in incremental as <whatever>-full-backup-files-to-delete.list
-- Output the list of the first file in each value of full-backup that is 
in incremental as <whatever>-full-backup-files-to-extract.list
-- Save a list of the permissions for every file. 

DONE! 



RESTORE:

NOTE: regular restore has to wipe the existing directory or otherwise get 
rid of stray files that shouldn't be there. 

NOTE:  The copy subroutine should be responsible for touching 0 length files. 



RESTORE FULL:

-- Wipe anything existing at the restore site.
-- Extract full backup.
-- Copy the files to all their other names.
-- Restore all file permissions. 

The filesystem is now restored to it's state at the time of Full Backup.

DONE!



RESTORE INCREMENTAL:

-- Wipe anything existing at the restore site.
-- Extract the files in full-backup that are in the incremental table.
    Either extract all and delete the extras, or extract by list-file.
-- Move the files to the first incremental name for each filehash.
-- Extract the incremental.
-- Copy each file to all it's other names as listed in inremental-table.
-- Restore all file permissions. 

The filesystem is now restored to it's state at the time of incremental.

DONE!
"






;;===========================================================================
;; BACKING UP:
;;===========================================================================


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

(defun make-archive-table (archive-crcs-file)
  "Reads in the archive-crcs-file file if it exists and returns a table
containing the information in the file, or an empty table if
archive-crcs-file is nil."
  (if archive-crcs-file 
      (file-to-table archive-crcs-file)
      (make-table)))

;;--! Left off here:
(defun make-incremental-files
    (source-dir archive-base &optional (archive-crcs-file nil))
  "Recurses through a directory noting all differences between the last full
archive and the current state of the directory.  These differences are saved
in files listing which files have been changed, added or removed, as well as
the current state of all file permissions.

Files listing files to archive or remove are created to be fed into archival 
programs.

NOTE: If you pass tar a directory it will always back up all it's 
contents even if you only want some of the files saved, eating up space with 
duplicates of files.
For other archivers use a file of just the current directory structure.
This can be prepended to the flat file incrementals list."
  (let* ((source-list-file   (strcat archive-base ".list"))
	 (source-dirs-file   (strcat archive-base ".dirs"))
	 (permissions-file   (strcat archive-base ".permissions"))
	 (current-table-file (strcat archive-base ".crcs"))
	 (delete-list-file   (strcat archive-base ".delete-list")) ;;--! Is this neccessary, or can it be deduced in restoration?
	 (source-list   (make-a-vector 0 :element-type 'string))
	 (source-dirs   (make-a-vector 0 :element-type 'string))
	 (delete-list   (make-a-vector 0 :element-type 'string))
	 (permissions   (make-a-vector 0))
	 (archive-table (make-archive-table archive-crcs-file))
	 (current-table (make-table))
	 (all-file-keys (make-table)))
    (flet ((add-file-to-current-table (filename)
	     "Populates the permissions list and the current-table"
	     (vector-push-extend (file-perms filename) permissions)
	     (when (zerop (mod (length permissions) 100))
	       (meout-now "Hash summed ~a files so far~%" (length permissions)))
	     (if (dir-p filename)
		 (vector-push-extend filename source-dirs)
		 ;;This is the ony place file-hashkey should normally be used.
		 (push-hash-if-list current-table (file-hashkey filename) filename))))
      (meout-now "Checking existing files for changes:~%~%")
      (posix-walk-directory source-dir #'add-file-to-current-table :process-dirs :breadth-first)
      (iter (for (k v) in-hashtable archive-table) (sethash all-file-keys k t))
      (iter (for (k v) in-hashtable current-table) (sethash all-file-keys k t))
      (iter 
       (for (filehash v) in-hashtable all-file-keys)
       (with archive-files = (gethash filehash archive-table))
       (with current-files = (gethash filehash current-table))
       ;(with size          = (car     filehash)) ; This relies on file-hashkey including the size.
       (cond ((and archive-files current-files)
	      ;; Hashkey is in both so it's already backed up.  Move/copy can be worried about at restore.
	      nil)
	     (archive-files 
	      ;; Not on filesystem.  Kill it.
	      (vector-push-extend-list archive-files delete-list))
	     (current-files 
	      ;; Not in archive.  Save it.
	      (vector-push-extend (car current-files) source-list)) ; Save one, copy to the rest.
	     (t (error "~%Hash ~s Not found in backup or current system.
Where did it come from?.~%~%" filehash))))
      (meout-now "There were ~a total files checked.~%" (length permissions))
      (meout-now "There were ~a removed files.~%"       (length delete-list))
      (meout-now "There are ~a files to be archived.~%" (length source-list))
      (array-to-file-eol permissions   permissions-file)
      (array-to-file-eol delete-list   delete-list-file)
      (array-to-file-eol source-list   source-list-file)
      (array-to-file-eol source-dirs   source-dirs-file)
      ;; This file gives the restoration step the full final state to produce:
      (table-to-file     current-table current-table-file))))


;;(defun cmd (command &rest args)
;;  "Run an external program."
;;    (meout-now "~%Executing Command:  \"~a\"~%" (apply 'join (flatten " " command args)))
;;    (sb-ext:run-program command (flatten args)))


(defun backup (working-dir source-dir &optional (archive-type nil))
  "Takes in the working directory and the source directory from the point
of view of the working directory.  
Changes directory to the working directory, and ensures all the archival 
directories exist and have the right permissions.
Moves any old incremental backups to the *obsolete-incrementals* directory.
Runs make-incremental-files and feeds the output list to the archiver.

Usage: (backup working-directory source-dir '7z)
Usage: (backup \".\" \"PROJECTS\" '7z)"
  (let ((archive-base (make-archive-name       source-dir))
	(full-backup  (find-latest-full-backup source-dir)))
    (posix-cd working-dir)
    (meout-now "~%Supposed to be in ~a~%Actually in ~a" working-dir (getcwd))
    (dolist (filename (list *backup-base* *full-backup-base* *incremental-base* *obsolete-base* *obsolete-incrementals*))
      (make-path   filename)
      (posix-chmod filename #o557))
    (move-files (list-old-incrementals source-dir) *obsolete-incrementals*)
    (make-incremental-files source-dir archive-base full-backup)
    (if (file-access-p (strcat archive-base ".list"))
	(ecase archive-type
	  (7zpath  (cmd "/Temp/LispBox/7za.exe" 
			"a" "-t7z" "-m0=lzma" "-mx=9" "-mfb=64" "-md=32m" "-ms=on" 
			(strcat archive-base ".7z") (strcat "@" archive-base ".list")))
	  (7z      (cmd "7za.exe" 
			"a" "-t7z" "-m0=lzma" "-mx=9" "-mfb=64" "-md=32m" "-ms=on" 
			(strcat archive-base ".7z") (strcat "@" archive-base ".list")))
	  ;;--! Probably gonna want more than just 7z available.
	  (rar     (bash "rar something"))
	  (zip     (bash "zip something"))
	  (tar.gz  (bash "tar zcvf something.tar.gz"))
	  (tar.xz  (bash "tar acvf something.tar.xz"))
	  (tar.bz2 (bash "tar jcvf something.tar.gz"))
	  ('nil (format t "Skipping any archiving, just getting checksums.~%")))
	(format t "~%~%No files were added or updated since the full backup~%~%"))
    ;; The par2 needs to happen if any files were added, updated, or removed.
    (if (or (file-access-p (strcat archive-base ".list")) 
	    (file-access-p (strcat archive-base "-rm.sh")))
	(cmd "par2 -r10 -n1 " archive-base archive-base "*"))
    (bash "sudo chown " (getenv "SUDO_USER") ":" (getenv "SUDO_USER") " " archive-base "*")))







;;===========================================================================
;; Restoration:
;;===========================================================================













;;--! Left off here:


;;--! Test this:
(defun find-latest-incremental-backup (source-dir)
  "Returns the latest incremental backup of the given directory or NIL if 
there is none.
This looks for .crc files rather than the archives for convienience, and 
because the archive type is not yet known at this stage."
    (car (sort (glob (strcat *incremental-base* "/" source-dir "-*.crc")) 
	       #'string-greaterp)))

;;--! Test this:
(defun get-archive-name (source-dir)
  "Returns the base name of the archive to restore.
If there is already an incremental that will be returned.
If not it will be a full backup."
  (or (find-latest-incremental-backup source-dir) (find-latest-full-backup source-dir)))


#+nil(let (
	(incremental-base   (strcat *incremental-base* "/" source-dir))
	(full-backup-base   (strcat *full-backup-base* "/" source-dir))
	(latest-incremental (find-latest-incremental-backup source-dir)))
    (if latest-incremental
	latest-incremental
	(find-latest-full-backup source-dir)))




#+nil(defun restore-it-basic (working-dir restore-dir)
  "Takes in the directory to restore"
  (let* ((archive-base (get-archive-name               restore-dir))
	 (begin-crcs   (find-latest-full-backup        restore-dir))
	 (final-crcs   (find-latest-incremental-backup restore-dir))
	 (begin-arc    (find-latest
	  )
    (posix-cd working-dir)
    (meout-now "~%Supposed to be in ~a~%Actually in ~a" working-directory (getcwd))


))))



#+nil(defun process-a-key (filehash)
  "Checks out a particular file key passed to it.
Will have to be called by iterating though all keys in archive-table and current-table
after they are both created, since it has to assume both are full."
  (let ((archive-files (gethash filehash archive-table))
	(current-files (gethash filehash current-table)))
    (cond ((and archive-files current-files) ()) ; Hashkey is in both tables so it's ALREADY BACKED UP.
	  ;; (if (equal archive-files current-files)) no changes, do nothing.
	  ;; Otherwise the file was moved from archive-files to current-files.
	  #+nil(unless (equal archive-files current-files)
		 ;;--! Fuck.  I think I got archive and restore switched in my mind here:
		 (Extract the first entry of archive-files)
		 (move file to first entry of current-files)
		 (copy file to all additional entries of current-files))
	  ;; No current file has the same contents.  Remove it.
	  (archive-files (vec-pushlist archive-files delete-list)) ; No current file has the same contents.  Remove it.
	  ;; No archive file has the same contents.  Save it.
	  (current-files (vec-pushlist current-files source-list))
	  (t (error "~%File Hash ~s Not found in old or new table.~%~%" (filehash))))))



;;--! I need process-crc-list to populate a removed list, with CRCs, instead of file, 
;;--! it can go through the list after crc-table is created to see if the
;;--! file was just moved.




(defun diff-directory-against-archive-of-same-name-new (dirname)
  "Runs diff-directory-against-archive with the assumption that the archive
name matches the directory name."
  (diff-directory-against-archive-new dirname (make-archive-name dirname) (find-latest-full-backup dirname)))

;;--! HELLO!  TESTING FUNCTIONS RIGHT HERE:

#+TESTING(diff-directory-against-archive-of-same-name-new "PROJECTS")






























































































































;;===========================================================================
;; POSSIBLE ARCHIVE WHERE RESTORE REUSES AS MANY FILES AS POSSIBLE:
;;===========================================================================

#+nil"
LATER PROJECT:
ALTERNATIVE RESTORATION:
Restoring may be faster if the program starts by seeing what files are on
the file system already that can simply be moved into place rather than
removed just to be extracted.  file hashes are naturally used to determine 
what to keep. 

NOTE: This can only work if there is a good way to do partial restores from
the archival program.  If you have to stick with restore then delete above,
then this is effectively worthless.

NOTE: This method also has issues relating to clobbering files moving them 
around.  All file clobbering issues can be solved by using a temporary 
directory and naming each file for it's file-hash until the final move into 
place.  Problem is where do I put a temp directory?  If it isn't on the same
device the whole effort is worthless.  But if I use a subdirectory then what
if the directory name already exists as something to back up?  Answer: The
subdirectory must be named after looking through all directory names in 
full, incremental, and current and picking something that doesn't exist yet.

I almost wish I could just save each file named for it's sha256 crc in the 
archive file, and then have a seperate file of what file name(s) each file
is to be extracted as.

I could actually do that if I rolled my own archiver, but oh dear the 
problems that would have attendant to it.  Later.



ALTERNATE-RESTORE-FULL BACKUP:

-- create current table, same way as incremental table for a backup.

-- delete all files with hashes not in full backup.

-- move the files to the first full backup name for each filehash.
--! NOTE: This may cause issues overwriting files when moving current files. 

-- extract only the files from full backup that are not in current.

-- copy each file to all it's other names as listed in full-backup-table.

-- restore all file permissions. 

The filesystem is now restored to it's state at the time of full backup.

Done!




ALTERNATE-RESTORE-INCREMENTAL:

-- create current table, same way as incremental table for a backup.

-- delete all files with hashes not in full backup.

-- move the files to the first full backup name for each filehash.
--! NOTE: This may cause issues overwriting files when moving current files. 

-- extract only the files from full backup that are in incremental but not 
in current.

-- move the files to the first incremental name for each filehash.

-- extract only the files from incremental that are not in current.

-- copy each file to all it's other names as listed in inremental-table.

-- restore all file permissions. 

The filesystem is now restored to it's state at the time of incremental.

Done!


"











