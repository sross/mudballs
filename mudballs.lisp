;; Copyright (c) 2008 Sean Ross
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;;
;; other operations
;;
;; doc, remove, upgrade, update


(defpackage :mudballs
  (:use :cl :mb.sysdef)
  (:nicknames :mb)
  (:shadow #:load #:compile #:search #:remove)
  (:export
   #:load #:compile #:search #:lisp-level #:clean #:test #:stat 
   ;; download related
   #:install #:update #:update-system #:self-update #:remove #:add #:upgrade #:document #:uninstall
   ;; from sysdef
   #:find-system #:find-component

   ;; support for single file systems
   #:component #:*search-paths*)
  (:documentation "The :MUDBALLS package provides a number of utility functions for easier interaction with
the :MB.SYSDEF package. It also provides some useful search and install functions."))

(in-package :mudballs)


(defun autoload (func package system)
  (let ((loaded nil))
    (setf (symbol-function func)
          #'(lambda (&rest args)
              (when loaded (error "Specified system (~A) has been loaded but has not redefinied ~A.~%"
                                  system func))
              (register-sysdefs)
              (perform system 'load-action)
              (if package
                  (let ((package-sym (find-symbol (string func) package)))
                    (setf (symbol-function func) (symbol-function package-sym))
                    (apply package-sym args))
                  (apply (symbol-function func) args))))))


(autoload 'scan :cl-ppcre :cl-ppcre)
(autoload 'split :cl-ppcre :cl-ppcre)
(autoload 'create-scanner :cl-ppcre :cl-ppcre)

(dolist (fn '(install update add-definition remove-definition upgrade))
  (autoload fn :installer :installer))

(defun add (url)
  "Downloads the system definition file specified by URL and makes them available.
Once downloaded all systems specified in the file will be downloadable \(provided the
author of the file specifies the necessary download url."
  (add-definition url)
  (register-sysdefs))

(defun remove (url)
  (remove-definition url))

(defvar *system-installed-marker* "*")

(defun report-on-system (name)
  (let ((systems (systems-for name)))
    (format t "~&~%~S (~{~{~A~@[~A~]~}~^, ~})~%" name
            (mapcar (lambda (x) (list (sysdef::version-string (version-of x))
                                      (when (component-exists-p x) *system-installed-marker*)))
                    systems))
    (format t "~5T~{~<~%~5T~1,75:; ~A~>~}~%"
            (split " " (or (documentation (first systems) 'system) "No documentation present.")))))

(defun systems-matching-regex (regex)
  (systems-matching #'(lambda (sys)
                        (or (scan regex (documentation sys 'system))
                            (scan regex (string (name-of sys)))
                            (some (lambda (x) (scan regex x)) (keywords-of sys))))))


(defun search (regex)
  "Prints out all available systems who have either a name, description or keywords matching regex.
NOTE: Versions with an asterisk next to them are installed."
  (register-sysdefs)
  (terpri) 
  (let ((regex (create-scanner (string regex) :case-insensitive-mode t)))
    (dolist (name (system-names (systems-matching-regex regex)))
      (report-on-system name))))

(defun system-names (systems)
  (sort (delete-duplicates (loop for x in systems
                                 collect (name-of x)))
        'string<))


(defun resolve-name (name &key version)
  (apply 'find-system name (when version (list :version version))))

(defmacro defaction-wrapper (name action &key documentation needs default-args)
  `(defgeneric ,name (name &rest args &key)
     ,@(when documentation `((:documentation ,documentation)))
     ,@(when needs
         `((:method :before ((name t) &rest args &key)
            (dolist (need ',needs)
              (apply 'load (if (listp need) need (list need)))))))
     (:method ((name t) &rest args &key version &allow-other-keys)
      (apply ',name (resolve-name name :version version)
             :allow-other-keys t args))
     (:method ((name component) &rest args &key)
      (apply 'perform name ,action :allow-other-keys t ,@default-args args))))


(defaction-wrapper load 'load-action :documentation
                   "Loads the system designated by NAME into the lisp image.
-VERSION, if supplied, specifies the version of the system to load.
-If NAME is a string or symbol the system is looked up using FIND-SYSTEM.")

(defaction-wrapper compile 'compile-action
                   :documentation "Compiles the system designated by NAME, this does not necessarily mean
-that the system will have been loaded.")

(defaction-wrapper clean 'sysdef::clean-action
                   :documentation "Removes all generated FASL files for the system designated by NAME.")

(defaction-wrapper test (find-symbol "TEST-ACTION" 'sysdef)
                   :needs (:test-action)
                   :default-args (:name nil))

(defaction-wrapper stat 'sysdef::stat-action :needs (:stat-action))
(defaction-wrapper document (intern "DOCUMENT-ACTION" :sysdef.document-action) :needs (:document-action))
(defaction-wrapper uninstall (intern "UNINSTALL-ACTION" :installer) :needs ((:installer :version (>= 0 2 4))))


(defun self-update ()
  "Updates the system bootstrapping mechanism. This should only be necessary when a bug is found in the boot.lisp file."
  (mb:load :installer :version '(>= 0 2 4))
  (funcall (find-symbol "SELF-UPDATE" :installer)))

(defun update-system ()
  "An alias for self-update."
  (self-update))

(defvar *lisp-level* :expert
  "one of :beginner, :intermediate or :expert")

(defun lisp-level () *lisp-level*)
(defun (setf lisp-level) (level)
  (check-type level (member :beginner :intermediate :expert))
  (setup-level (setf *lisp-level* level)))

(defun setup-level (value)
  (case value
    (:beginner (beginner-setup))
    (:intermediate (expert-setup))
    (:expert (expert-setup))))

(defun beginner-setup ()
  (setf *debugger-hook* #'(lambda (c hook)
                            (declare (ignore hook))
                            (invoke-restart (find-restart 'abort (princ c))))))

(defun expert-setup ()
  (setf *debugger-hook* nil))


;; EOF
