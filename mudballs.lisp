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
   #:install #:update #:remove #:add #:upgrade))

(in-package :mudballs)


(defun autoload (func package system)
  (let ((loaded nil))
    (setf (symbol-function func)
          #'(lambda (&rest args)
              (when loaded (error "Specified system (~A) has been loaded but has not redefinied ~A.~%"
                                  system func))
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

(defun load (name &rest args &key version &allow-other-keys)
  (declare (system-designator name))
  "Loads the system designated by NAME into the lisp image.
VERSION, if supplied, specifies the version of the system to load.
If NAME is a string or symbol the system is looked up using FIND-SYSTEM."
  (apply #'perform (resolve-name name :version version)
         'load-action :allow-other-keys t  args))

(defun compile (name &rest args &key version)
  (declare (system-designator name))
  "Compiles the system designated by NAME, this does not necessarily mean
that the system will have been loaded."
  (apply #'perform (resolve-name name :version version) 'compile-action args))

(defun clean (name &rest args &key version)
  (declare (system-designator name))
  "Removes all generated FASL files for the system designated by NAME."
  (apply #'perform (resolve-name name :version version) 'sysdef::clean-action args))

(defun test (name &rest args &key file version)
  (declare (system-designator name)
           (ignore args))
  "Performs a TEST-ACTION on the system designated by name."
  (mb:load :test-action)
  (perform (resolve-name name :version version)
           (find-symbol "TEST-ACTION" 'sysdef)
           :name file))

(defun stat (name &key version)
  (declare (system-designator name))
  "Prints out some statistical information pertaining to the system designated by name."
  (mb:load :stat-action)
  (perform (resolve-name name :version version) 'sysdef::stat-action))


(defun update-system ()
  "Updates the system bootstrapping mechanism. This should only be necessary when a bug is found in the boot.lisp file."
  (mb:load :installer)
  (funcall (find-symbol #:system-update :installer)))
  
  

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
