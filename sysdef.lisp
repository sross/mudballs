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
;;; TODO:
;; change *systems* to be a hash-table

;;;; Mudballs System Definitions
;;;

;;; POSSIBLE ABSTRACTIONS
;; abstract out mapping over loaded systems. (used by ensure-dependencies-up-to-date)

(declaim (optimize debug safety (speed 0)))
(defpackage :mb.sysdef (:use :cl)
  (:nicknames :sysdef)
  (:export

   ;; Uppercase symbols still need to be documented.
   ;; special vars
   #:*info-io* #:*compile-fails-behaviour* #:*compile-warns-behaviour* #:*load-fails-behaviour*
   #:*fasl-output-root* #:*finders* #:*custom-search-modules*
   #:*root-pathname* #:*sysdef-path* #:*systems-path* #:*DEFAULT-DEVELOPMENT-MODE*

   ;; types
   #:system-name #:system-designator

   ;; core protocol
   #:NAME-OF #:process-options #:PROCESS-OPTION #:EXECUTE #:PERFORM #:SUPPORTEDP  #:MODULE-DIRECTORY
   #:COMPONENT-PATHNAME #:INPUT-FILE #:INPUT-WRITE-DATE #:FASL-PATH #:OUTPUT-FILE #:OUTPUT-WRITE-DATE
   #:ALL-FILES  #:OUT-OF-DATE-P #:DEPENDENCY-APPLICABLEP #:NAME= #:READABLEP
   #:COMPONENT-DEPENDENCIES #:ACTION-DEPENDENCIES #:DEPENDENCIES-OF #:COMPONENT-EXISTS-P
   #:COMPONENT-OUTPUT-EXISTS-P  #:APPLICABLE-COMPONENTS #:COMPONENT-APPLICABLE-P 
   #:FIND-SYSTEM #:FIND-COMPONENT #:TOPLEVEL-COMPONENT-OF #:CHECK-SUPPORTED-P #:SKIP

   ;; Dependencies
   #:DEPENDENCY #:MATCH-ACTION-OF #:COMPONENT-OF #:CONSEQUENT-ACTION-OF

   ;; ACCESSORS
   #:DIRECTORY-OF #:OUTPUT-PATHNAME-OF #:VERSION-OF #:KEYWORDS-OF #:COMPONENTS-OF #:ALL-FILES #:MD5SUM-OF
   #:PROVIDER-OF

   ;; COMPONENTS AND ACTIONS
   #:COMPONENT #:MODULE #:LAZY-MODULE #:PATCHABLE #:SYSTEM #:ACTION #:FILE-ACTION #:SOURCE-FILE-ACTION
   #:COMPILE-ACTION #:LOAD-ACTION #:LOAD-SOURCE-ACTION #:CLEAN-ACTION #:FILE #:STATIC-FILE #:SOURCE-FILE
   #:LISP-SOURCE-FILE #:STUB-SYSTEM

   ;; CONDITIONS
   #:SYSDEF-CONDITION #:SYSDEF-ERROR #:SYSDEF-WARNING #:NO-SUCH-COMPONENT #:COMPONENT-NOT-SUPPORTED
   #:DEPRECATED-SYSTEM #:FASL-ERROR #:FASL-OUT-OF-DATE #:FASL-DOES-NOT-EXIST
   #:COMPILATION-ERROR #:COMPILE-FAILED #:COMPILE-WARNED #:DUPLICATE-COMPONENT #:SYSTEM-REDEFINED
   #:SYSTEM-NOT-INSTALLED #:MISSING-COMPONENT #:SYSTEM-ALREADY-LOADED
   
   ;; SYSTEM DEFINITION
   #:register-sysdefs #:define-system #:undefine-system #:define-system-template #:define-available-system

   ;; WILDCARD MODULES
   #:WILDCARD-MODULE #:WILDCARD-PATHNAME-OF #:WILDCARD-SEARCHER

   ;; NAMED MODULES
   #:NAMED-MODULE

   ;; CONDUIT SYSTEMS
   #:CONDUIT-SYSTEM #:DEFINE-CONDUIT-SYSTEM

   ;; PATCHES
   #:LOAD-PATCHES #:CREATE-PATCH-MODULE #:PATCH

   ;; SYSTEM TRAVERSAL
    #:systems-matching #:do-systems #:map-systems #:systems-for

   ;; CONFIG FILE
   #:*load-config* #:load-config #:create-config-component

   ;; PREFERNCES
   #:preference #:*load-preferences*

   ;; MISC
   #:RUN-SHELL-COMMAND #:implementation #:os #:platform

   ;; PROVIDER RELATED
   #:with-provider #:URL-OF #:provider

   ;; SINGLE FILE SYSTEMS
   #:*multiple-matching-files-behaviour* #:multiple-file-condition #:multiple-file-warning #:multiple-file-error
   #:*search-paths* #:single-file-system

   ;; Requirements
   #:create-requirement #:requirement
   )

  (:import-from  #.(package-name 
                    (or (find-package :clos)
                        (find-package :pcl)
                        (find-package :sb-pcl)
                        (find-package :mop)
                        (find-package :openmcl-mop)
                        (error "Can't find suitable CLOS package.")))
   :class-precedence-list :generic-function-methods :method-specializers #-abcl :effective-slot-definition
   #-abcl :class-slots #-abcl :slot-definition-initargs :class-default-initargs
   :method-qualifiers :class-direct-superclasses :class-direct-subclasses :finalize-inheritance
   :funcallable-standard-class)
  (:documentation "The :MB.SYSDEF package contains all the plumbing for defining your own systems.
Typically you would not use this package directly but rather define your systems while `in-package` :sysdef-user."))

(in-package :sysdef)

;;; TYPES
(deftype system-name ()
  "The type which is a valid system name to define-system.
This is either a symbol, which designates a system definition
or a list of the form (PARENT-SYSTEM-NAME [MODULE-NAME]* SYSTEM-NAME])
which will define a module on PARENT-SYSTEM-NAME or on module found by
descending into PARENT-SYSTEM-NAME's components using MODULE-NAMES."
  '(and (not null) (or symbol cons)))

;;; SPECIALS
(defparameter *systems* ()
  "List containing all the defined systems in the Lisp image.")

(declaim (stream *info-io*)
         (type (member nil :warning :error) *compile-fails-behaviour*)
         (type (member nil :error) *compile-warns-behaviour*)
         (type (member :error :compile :load-source) *load-fails-behaviour*)
         (pathname *root-pathname* *systems-path* *sysdef-path*)
         (type (or null pathname) *fasl-output-root*)
         (list *finders* *custom-search-modules*))

(defvar *info-io* *standard-output* "*info-io* intended to be bound to an output stream where information messages will be sent.
Eg. On CLEAN-ACTION, when a file is actually deleted a message will be printed to *info-io*")

(defvar *compile-fails-behaviour* :warning
  "This describe the behaviour to take when compiling a file fails.
When NIL then the error is ignored, when :WARNING then a WARNING is signalled
and when :ERROR an error of type COMPILE-FAILED is signalled.")


(defvar *compile-warns-behaviour* nil
    "This describe the behaviour to take when compiling a file signals a warning.
When NIL then the warning is ignored and when :ERROR an error of type
COMPILE-WARNED is signalled.")


(defvar *load-fails-behaviour* :compile
  "*LOAD-FAILS-BEHAVIOUR* controls the behaviour taken when loading a FASL file fails.
When :ERROR then the error signalled by the call to LOAD is signalled. When the
values is :COMPILE, the COMPONENT in question is compiled and then reloaded and finally
when the value is :LOAD-SOURCE the source file of the COMPONENT is loaded.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *root-pathname* mudballs.boot::*root-path*
    "*ROOT-PATHNAME* is the root of the mudballs directory and is used to compute
*systems-path* and *sysdef-path*.")


  (defparameter *systems-path*
    (merge-pathnames (make-pathname :directory '(:relative "systems"))
                     *root-pathname*)
    "This is the root directory for all of the systems defined.
System paths take the form *systems-path* /SYSTEM-NAME/VERSION/")

  (defparameter *sysdef-path*
    (merge-pathnames (make-pathname :directory '(:relative "system-definitions"))
                     *root-pathname*)
    "This is the root folder of all of the mudballs system definition files.")
  )

(defparameter *saved-slots* '(operation-times components config-component)
  "Slots which are saved on system redefinition.")

(defparameter *inherited-slots* '(if-needs-fails if-supports-fails default-component-class serial)
  "Slots which get inherited from parent components.")

(defparameter *processed-actions* nil)

(defvar *fasl-output-root* (or mudballs.boot:*fasl-root* nil) ;; this is the preference :fasl-output-root
  "When non-nil specifies the root directory where all compiled lisp files are to be
compiled to.
This can be set using the preference (in ~/.mudballs.prefs) :fasl-output-root")

(defvar *builtin-systems* '("mudballs" "sysdef-definitions"))

(defun builtin-system-p (name)
  (member (component-name name) *builtin-systems* :test #'name=))

(defparameter *finders* '(find-single-file-system default-system-finder)
  "A list of functions with the arglist (name &rest args &key errorp version) which are used
by FIND-SYSTEM and FIND-COMPONENT to lookup the system named by NAME.
The function must either return the system with NAME or NIL indicating that it has failed
to find an appropriate system and the next function on *FINDERS* should be used.")


(defparameter *complex-spec-operators* '(and or not))
(defparameter *boundary-spec-tests* '(> < <= >= = /=))

;; it would be nice if we could use name= for the test here
(defparameter *loaded-versions* (make-hash-table :test 'equalp))

(defvar *default-development-mode* nil)

(defparameter *custom-search-modules* ()
  "This list contains components (typically modules) which are used to supplement the default
search paths for mudballs system definition files.
The components on this list become part of the components of the standard sysdef-definition system
which loads the system definitions. Adding components to this list will result in said components
being loaded by register-sysdefs.")

(defparameter *default-provider* ()
  "Bound to an instance of provider or NIL. Used as a default initarg for systems. ")

(defvar *load-config* t "When bound to a non-nil value, indicates that the config file for
a system should be processed after loading a sytem. See the :config-file option in define-system")

(defvar *bound-preferences* "Bound to the preferences of the system currently being loaded.")
(defvar *load-preferences* t "When bound to a non-nil will ensure that the preferences file
for a system is made available during system load and compilation via the preference function.")

(defvar *sysdef-system* nil "Set to the system definition system once during mudballs load.")


;;; UTILITIES
 ;; Stolen from ASDF
(defvar *verbose-out* nil)

(defun run-shell-command (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *VERBOSE-OUT*.  Returns the shell's exit code."
  (declare (string control-string))
  (let ((command (apply #'format nil control-string args)))
    (format *verbose-out* "; $ ~A~%" command)
    #+sbcl
    (sb-ext:process-exit-code
     (sb-ext:run-program  
      #+win32 "sh" #-win32 "/bin/sh"
      (list  "-c" command)
      #+win32 :search #+win32 t
      :input nil :output *verbose-out*))
    
    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program  
      "/bin/sh"
      (list  "-c" command)
      :input nil :output *verbose-out*))

    #+allegro
    (excl:run-shell-command command :input nil :output *verbose-out*)
    
    #+lispworks
    (system:call-system-showing-output
     command
     :shell-type "/bin/sh"
     :output-stream *verbose-out*)
    
    #+clisp				;XXX not exactly *verbose-out*, I know
    (or (ext:run-shell-command  command :output :terminal :wait t)
        0)

    #+openmcl
    (nth-value 1
	       (ccl:external-process-status
		(ccl:run-program "/bin/sh" (list "-c" command)
				 :input nil :output *verbose-out*
				 :wait t)))
    #+ecl ;; courtesy of Juan Jose Garcia Ripoll
    (si:system command)
    #-(or openmcl clisp lispworks allegro scl cmu sbcl ecl)
    (error "RUN-SHELL-PROGRAM not implemented for this Lisp")
    ))

;;; From Christophe Rhodes post on cclan-list
(define-method-combination standard-sysdef-method-combination ()
  ((around-sysdef (around))
   (around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let* ((form (if (or before after (rest primary))
                     `(multiple-value-prog1
                          (progn ,@(call-methods before)
                            (call-method ,(first primary)
                                         ,(rest primary)))
                        ,@(call-methods (reverse after)))
                     `(call-method ,(first primary))))
           (standard-form (if around
                              `(call-method ,(first around)
                                            (,@(rest around)
                                             (make-method ,form)))
                              form)))
      (if around-sysdef
          `(call-method ,(first around-sysdef)
                        (,@(rest around-sysdef) (make-method ,standard-form)))
          standard-form))))


(defun without-leading (item list &key (test 'eql))
  "Returns LIST without any leading ITEM's."
  (labels ((iter (list acc)
             (cond ((endp list) acc)
                   ((funcall test (car list) item)
                    (iter (cdr list) acc))
                   (t (nconc acc list)))))
    (iter list ())))

(defun singlep (thing)
  (and (consp thing) (not (cdr thing))))

(defmacro when-let ((name test) &body body)
  `(let ((,name ,test))
     (when ,name
       ,@body)))

(defmacro orf (place value &environment env)
  (multiple-value-bind (vars vals store-vars writer reader)
      (get-setf-expansion place env)
    (when (cdr store-vars) (error "Cannot handle multiple values."))
    (let ((tmp-read (gensym)))
      `(let* (,@(mapcar 'list vars vals)
              (,tmp-read ,reader)
              (,@store-vars (or ,tmp-read ,value)))
         (or ,tmp-read (progn ,writer ,@store-vars))))))

(defun first-version ()
  (list 0 0 1))

(defun mklist (x)
  (if (listp x) x (list x)))

(defmacro revpush (obj place &environment env)
  "Adds obj to the end of place."
  (multiple-value-bind (vars vals set-var set get) (get-setf-expansion place env)
    `(let* (,@(mapcar #'list vars vals)
            (,@set-var ,get))
       (setf ,@set-var (nconc ,@set-var (list ,obj)))
       ,set)))


(defun make-restarter (restart &rest args)
  "Returns a function of 1 argument, which, when called, will invoke the restart RESTART with args."
  #'(lambda (c) (apply #'invoke-restart (find-restart restart c) args)))

(defun toplevel-component-of (component)
  "Returns the toplevel system for component."
  (if (null (parent-of component))
      component
      (toplevel-component-of (parent-of component))))

(defun split (string &key max (ws '(#\Space #\Tab)))
  "Split `string' along whitespace as defined by the sequence `ws'.
Whitespace which causes a split is elided from the result.  The whole
string will be split, unless `max' is provided, in which case the
string will be split into `max' tokens at most, the last one
containing the whole rest of the given `string', if any."
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
        (when (and max (>= words (1- max)))
          (return (cons (subseq string start) list)))
        (setf end (position-if #'is-ws string :start start))
        (push (subseq string start end) list)
        (incf words)
        (unless end (return list))
        (setf start (1+ end)))))))


(defun safe-write-date (comp)
  (if (component-output-exists-p comp)
      (output-write-date comp)
      0))

(eval-when  (:compile-toplevel :load-toplevel :execute)
  (defun implementation ()
    "Returns a canonical symbol which can be used to identify a particular implementation.
The values are as follows.
Lispworks   :lispworks
SBCL        :sbcl
CMUCL       :cmucl
CLISP       :clisp
Allegro     :allegrocl
ABCL        :abcl
ECL         :ecl
MCL         :mcl
SCL         :scl
OPENMCL/CCL :openmcl
If an implementation other than the ones mentioned above are used an error will be signalled.

The result of invoking this function is made present on *features*"    
    #+lispworks :lispworks #+sbcl :sbcl #+cmu :cmucl #+clisp :clisp
    #+allegro :allegrocl #+abcl :abcl #+ecl :ecl #+gcl :gcl
    #+mcl :mcl  #+openmcl :openmcl #+scl :scl
    #-(or lispworks sbcl cmu clisp allegro abcl ecl gcl mcl openmcl scl)
    (error "We don't know what this implementation is.
Please update the implementation function."))

  (defun os ()
    "Returns a canonical symbol which can be used to identify the operating system upon which
the Lisp image is running.
The values are as follows.

Microsoft Windows      :mswindows
Linux                  :linux
Mac                    :mac

If running on an operating system other than the ones mentioned above NIL will be returned.
The result of invoking this function is made present on *features*"
    (or #+(or :mswindows :windows) :mswindows
        #+(or :macosx :darwin :macos) :mac
        #+(or :linux (and :clisp :unix)) :linux))

  (defun platform ()
    "Returns a canonical symbol which can be used to identify the underlying architecture upon
which the Lisp image is running.

The currently understood values are as follows.

:x86, :x86-64, :ppc, :hppa.

The result of invoking this function is made present on *features*"
    (or #+(or :amd64 :x86-64 :x64) :x86-64
        #+(or :x86 :pc386 :i486) :x86
        #+(or :ppc :powerpc) :ppc
        #+:hppa :hppa))
    
  (pushnew (os) *features*)
  (pushnew (platform) *features*)
  (pushnew (implementation) *features*)
  )



;;; CORE PROTOCOL
;;;;;;;;;;;;;;;;;
(defgeneric name-of (object)
  (:method (object)
   (error "The required method NAME is not implemented by ~S." object)))



;;; CORE CLASSES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPONENT, MODULE & SYSTEM
;; Our base classes which we create systems with.
;; I'm extremely fond of asdf's view of a system being
;; a tree of components.
(defclass COMPONENT ()
  ((name :accessor name-of :initarg :name :initform :unspecific)
   (parent :accessor parent-of :initarg :parent :initform nil)
   (needs :accessor needs-of :initarg :needs :initform nil)
   (requires :accessor requires-of :initform nil)
   (supports :accessor supports-of :initform nil :initarg :supports)
   (documentation :initarg :documentation :accessor doc :initform nil)
   (operation-times :initform (make-hash-table) :accessor operation-times)
   (version :accessor version-of :initform (first-version) :initarg :version)
   (patch-version :accessor patch-version-of :initform nil)
   (if-supports-fails :accessor if-supports-fails :initarg :if-supports-fails :initform :error
                      :documentation "<strong>:if-supports-fails</strong> <i>value</i>
This sets the slot for component to VALUE, which can be the keyword :error or a symbol.
When it is :error then if a component is not supported and attempt to perform a source-file-action
upon the component is attempted a condition of type component-not-supported is signalled.
When it is a non-nil symbol then the function designated by that symbol will be called with the
component as an argument.")
   (pathname :accessor pathname-of :initarg :pathname :initform nil
             :documentation "<strong>:pathname</strong> <i>path</i>
Sets the root pathname of the system to PATH. (See component-pathname for more info)")
   (output-pathname :accessor output-pathname-of :initarg :output-pathname :initform nil
                    :documentation "<strong>:output-pathname</strong> <i>pathname</i>
This specifies the destination of the output when operating on a component.
This is only meaningful for files and doesn't apply to modules or systems.
The output-pathname option is primarly used to specify a specific file to which
a lisp-file is compiled.")
   (for :accessor for-of :initarg :for :initform nil :documentation "Source files can only be applicable
for certain operating systems or implementations. When the :for option is provided the file is only considered
for source-file-actions when the provided form satisfies FEATUREP.")))



(defclass MODULE (component)
  ((components :accessor components-of :initform () :initarg :components)
   (uses-macros-list :accessor uses-macros-from :initarg :uses-macros-from :initform nil)
   (serial :accessor serialp :initarg :serial :initform nil
           :documentation "<strong>:serial</strong> <i>boolean</i>
Specifying serial indicates that all components adding to the system will :REQUIRE the component
before it. This is identical to the ASDF meaning of :serial.")
   (directory :accessor directory-of :initarg :directory
              :documentation "<strong>:directory</strong> <i>name</i>
This specifies the directory where the source of system which will be reside. This directory is
appended to the *systems-path*. If not specified it defaults to (string-downcase name-of-system).
This option is defined on the module class and as such is available for modules as well.
A value of NIL is also permissible and specifies that the pathname of the component will be the
same as the pathname of the components parent (see .
A List is permissible as well and is a way to indicate multiple directories in a portable manner.
eg. (:directory (\"dir-1\" \"dir-2\"))")
   (default-component-class :accessor default-component-class-of :initform 'lisp-source-file
                            :initarg :default-component-class
                            :documentation "<strong>:default-component-class</strong> <i>class-name</i>
This specifies the default class for components of this system. The default is LISP-SOURCE-FILE.")))

(defclass LAZY-MODULE (module) ())

(defclass PATCHABLE ()
  ((patch-module :accessor patch-module-of :initform ())))

(defclass RELEASABLE ()
  ((release-date :accessor release-date :initform nil :initarg :release-date)
   (released-by :accessor released-by :initform nil :initarg :released-by)))

(defclass SYSTEM (module patchable releasable)
  ((maintainer :accessor maintainer-of :initform nil :initarg :maintainer)
   (author :accessor author-of :initform "Unknown" :initarg :author)
   (license :accessor license-of :accessor licence-of :initform "Unknown"
            :initarg :licence :initarg :license :documentation "<strong>:licence/:license</strong> <i>Licence-or-Symbol</i>
This specifies what licence the system is distributed under, it can be a string, which is the full licence description
or one of the following symbols designating a licence. :gpl2, :gpl3, :lgpl, :bsd, :mit, :public-domain.
These predefined licence types may be treated specially in autogenerated documentation")
   (development :accessor development-mode :initform *default-development-mode* :initarg :development
                :documentation "<strong>:development</strong> <i>boolean</i>
When set to a non nil value this indicates that the source of the system resides in the same location
as the sysdef file that the system was defined in. This defaults to *default-development-mode* and
can be bound by wildcard-searcher.")
   (keywords :accessor keywords-of :initarg :keywords :initform ())
   (deprecated :accessor deprecatedp :initarg :deprecated :initform nil
               :documentation "<strong>:deprecated</strong> <i>name-or-t</i>
Setting deprecated indicates that this system is depcrecated and a warning will be signalled
when loading this system. if NAME-OR-T is a system name then that name will be recommended in2
the warning as a replacement.")
   (contact :accessor contact-of :initform "The Author" :initarg :contact)
   (config :initform nil :initarg :config-file
           :documentation "<strong>:config-file</strong> <i>pathname</i>
Config files are files which are loaded post system load to customize a systems behaviour.
This can be controlled by the special variable *load-config*. The config file will only
be loaded when necessary and utilizes the sysdef machinary to achieve this, this includes
the creation of a config component \(the type of which can by customized using the :default-config-class
option\) and is loaded by \(execute CONFIG-COMPONENT 'load-source-action\)
Config paths can be portably specified using #\; as directory seperators.
The home directory specifier #\~ can be used as the first character of a config pathname which will be
expanded to the (user-homedir-pathname), the results of #\~ appearing anywhere else in the config path is undefined.")
   (config-component :accessor config-component-of :initform nil)
   (preference-file :accessor preference-file-of :initform nil :initarg :preferences
                    :documentation "<strong>:preferences</strong> <i>pathname</i>
Preference files are files with contents of the form (keyword value)* and these
preferences are made available during the compilation and loading of the system
via the preference function, this can be useful when you need a value to be set
during loading. Mudballs uses a ~/.mudballs.prefs file for this purpose to set
the *fasl-output-root* variable with a :fasl-output-root preference.

ie.  ~/.mudball.prefs can have the following value

\(:fasl-output-root \"/tmp/\"\)

and *fasl-output-root* is defined as (or (preference :fasl-output-root) ....))

Preferences paths can be portably specified using #\; as directory seperators.
eg. \"~;lisp;config.lisp\"
The home directory specifier #\~ can be used as the first character of a preference file pathname which will be
expanded to the (user-homedir-pathname), the results of #\~ appearing anywhere else in the namestring is undefined.")
   (default-config-class :accessor default-config-class-of :initform 'config-file
                         :initarg :default-config-class
                         :documentation "<strong>:default-config-class</strong> <i>class-name</i>
This options specifies the default type of the component created for a systems config file.")   
   (provider :reader provider-of :initarg :provider :initform *default-provider*)
   (md5sum :reader md5sum-of :initarg :md5sum :initform nil
           :documentation "<strong>:md5sum</strong> <i>string</i>
This is used in conjunction with with-provider and is used to specify the md5sum of the
download file for this system and will be checked when a system is installed.")
   (home-page :reader home-page-of :initarg :home-page :initform :unknown
              :documentation "<strong>:home-page</strong> <i>url</i>
A url can be provided to systems to indicate the home page of the system.
Since mudballs is an external collector of systems, rather than a hosting site, systems
will generally have a home page which is not governed by mudballs. That site can be found
by following this link.")
   (system-definition-component :accessor system-definition-component :initform nil)))



(defmethod print-object ((system component) stream)
  (flet ((name-and-version ()
           (let ((my-name (name-of system)))
             (if (patch-version-of system)
                 (format stream "~S ~A (patched from ~A)" my-name
                         (version-string (patch-version-of system))
                         (version-string (version-of system)))
                 (format stream "~S ~A" my-name
                         (version-string (version-of system)))))))
    (if *print-escape*
        (print-unreadable-object (system stream :type t :identity t)
          (name-and-version))
        (name-and-version))))

(defmethod documentation ((comp component) (type t))
  (doc comp))

(defmethod documentation ((comp symbol) (type (eql 'system)))
  (documentation (find-component comp) t))

(defmethod documentation ((comp string) (type (eql 'system)))
  (documentation (find-component comp) t))


;;; ACTIONS
(defclass ACTION ()
  ((needs :initarg :needs :initform nil :accessor needs-of)
   (force :initarg :force :initform nil :reader forcep))
  (:documentation "The core action class which will be applied to component's using execute."))

(defmethod name-of ((action action))
  (class-name (class-of action)))

(defgeneric coerce-to-action (thing &rest initargs)
  (:documentation "Converts THING (a class designator) into an action")
  (:method ((action action) &rest initargs)
   (declare (ignore initargs))
   action)
  (:method ((action symbol) &rest initargs)
   (apply 'make-instance action initargs))
  (:method (action &rest initargs)
   (declare (ignore initargs))
   (error "Don't know how to coerce ~S into an action." action)))


(defclass FILE-ACTION (action) ())
(defclass SOURCE-FILE-ACTION (file-action) ())


;;Basic actions operating on source-files.
(defclass COMPILE-ACTION (source-file-action) ())


(defclass LOAD-ACTION-MIXIN () ())

(defclass LOAD-ACTION (source-file-action load-action-mixin) ()
  (:default-initargs :needs '(compile-action)))

(defclass LOAD-SOURCE-ACTION (source-file-action load-action-mixin) ())



(defclass CLEAN-ACTION (source-file-action) ())
(defclass INSTALL-ACTION (action) ())
  

;;; COMPONENTS

(defclass FILE (component)
  ((type :accessor file-type :initarg :type :initform nil)))


(defclass STATIC-FILE (file) ())
(defclass SOURCE-FILE (file) ())



(defclass LISP-SOURCE-FILE (source-file) () (:default-initargs :type "lisp"))

(defmethod print-object ((file file) stream)
  (with-slots (name type) file
    (if *print-escape*
        (print-unreadable-object (file stream :type t :identity t)
          (format stream "~A~@[.~A~]" name type))
        (format stream "~A~@[.~A~]"  name type))))



;;; CONDITIONS
(define-condition file-mixin ()
  ((file :reader file-of :initarg :file)))

(define-condition sysdef-condition (condition) ())
(define-condition sysdef-error (error sysdef-condition) ())
(define-condition sysdef-warning (warning sysdef-condition) ())

(define-condition no-such-component (sysdef-error)
  ((name :reader name-of :initarg :name)
   (version :reader version-of :initarg :version :initform nil)
   (parent :reader parent-of :initarg :parent :initform nil))
  (:report (lambda (c s)
             (format s "No component named ~S~@[ version ~A~] ~@[ in ~S~]."
                     (name-of c) (version-of c) (parent-of c)))))

(define-condition component-not-supported (sysdef-error)
  ((%component :reader component-of :initarg :component))
  (:report (lambda (c s)
             (format s "~&Component ~A is not supported.~%The following checks failed~%~
~{~S~%~}" (component-of c)
                     (remove-if #'(lambda (x)
                                    (apply #'check-supported-p x))
                                (supports-of (component-of c)))))))

(define-condition missing-component (sysdef-error)
  ((component :reader component-of :initarg :component))
  (:report (lambda (c s)
             (format s "Component ~A does not exist.~%" (input-file (component-of c)))))
  (:documentation "The error which is signalled when an operation is requested on a component which is not present on the file system."))

(define-condition system-not-installed (sysdef-error)
  ((system :reader system-of :initarg :system))
  (:report (lambda (c s)
             (format s "System ~A is not installed.~%" (system-of c)))))

(define-condition deprecated-system (warning sysdef-condition)
  ((system :reader system-of :initarg :system)
   (by :reader deprecated-by :initarg :by))
  (:report (lambda (c s)
             (format s "System ~:@(~A~) is deprecated~@[, Please use ~:@(~A~) instead~]."
                     (system-of c) (deprecated-by c)))))

(define-condition fasl-error (sysdef-error file-mixin) ())

(define-condition fasl-out-of-date (fasl-error)
  ()
  (:report (lambda (c s)
             (format s "The FASL ~A is out of date." (file-of c)))))

(define-condition fasl-does-not-exist (fasl-error)
  ()
  (:report (lambda (c s)
             (format s "The FASL ~A does not exist." (file-of c)))))

(define-condition compilation-error (sysdef-error file-mixin)
  ()
  (:report (lambda (c s)
             (format s "Compiling ~S failed." (file-of c)))))

(define-condition compile-failed (compilation-error)
  ()
  (:report (lambda (c s)
             (format s "Compilation for file ~A failed." (file-of c)))))

(define-condition compile-warned (compilation-error)
  ()
  (:report (lambda (c s)
             (format s "Compilation for file ~A warned." (file-of c)))))

(define-condition duplicate-component (sysdef-error)
  ((system :accessor system-of :initarg :system)
   (name :accessor name-of :initarg :name))
  (:report (lambda (c s)
             (format s "Duplicate components named ~S in system ~S." (name-of c) (system-of c)))))

(define-condition system-redefined (sysdef-warning)
  ((name :initarg :name :accessor name-of)
   (version :initform nil :initarg :version :accessor version-of))
  (:report (lambda (c s)
             (format s "Redefining ~A version ~A." (name-of c) (version-of c)))))

(define-condition system-already-loaded (sysdef-error)
  ((name :initarg :name :reader name-of)
   (requested-version :initarg :requested-version :reader requested-version-of)
   (loaded-version :initarg :loaded-version :reader loaded-version-of))
  (:report (lambda (c s)
             (format s "Attempting to perform an action upon version ~A of ~A ~
but version ~A is already loaded." (version-string (requested-version-of c)) (name-of c)
                     (version-string (loaded-version-of c))))))


;;;; CORE EXECUTION PROTOCOL
(defgeneric process-options (component options)
  (:method (component options)
   (declare (ignore options))
   (error "The required method PROCESS-OPTIONS is not implemented by ~S." component))
  (:documentation "Takes a list options, each option being a list of the form (<i>option-name</i> values) and processes
them against component."))


(defgeneric process-option (component option-key &rest option-data)
  (:method  (system option-key &rest option-data)
   (declare (ignore option-data))
   (error "The required method PROCESS-OPTION is not implemented by ~S for key ~S."
          system option-key )))

(defgeneric execute (system action)
  (:method-combination standard-sysdef-method-combination)
  (:documentation "The private exection method, this will NOT create a new context when running action.")
  (:method (system action)
   (error "The required method EXECUTE is not implemented for ~S and ~S." system action))
  
  (:method ((system symbol) action)
   (execute (find-component system) action))
  
  (:method ((system component) action-key)
   (execute system (coerce-to-action action-key))))

(defgeneric perform (system action &rest initargs &key &allow-other-keys)
  (:documentation "The public exection method, this WILL create a new context when running action.")
  (:method (system action &rest initargs &key &allow-other-keys)
   (declare (ignore initargs))
   (error "The required method PERFORM is not implemented for ~S and ~S." system action))

  (:method ((system symbol) action &rest initargs &key &allow-other-keys)
   (apply #'perform (find-component system) action initargs))

  (:method ((system component) action-key &rest initargs &key &allow-other-keys)
   (perform system (apply 'coerce-to-action action-key initargs)))

  (:method ((system module) (action action) &rest action-data &key &allow-other-keys)
   (declare (ignore action-data))
   (let ((*processed-actions* (acons nil nil nil)))
     (execute system action)))
  
  (:method ((component component) (action action) &rest action-data &key &allow-other-keys)
   (declare (ignore action-data))
   (let ((*processed-actions* (acons nil nil nil)))
     (process-parents-dependencies component action)
     (execute component action))))


;;; READABLEP
(defgeneric readablep (file)
  (:documentation "A predicate which returns true if the implementation believes that it can read FILE. This also works on modules.")
  (:method ((file component))
   (if (for-of file)
       (and (featurep (for-of file)) t)
       t)))

;;; OPTION PROCESSING
;; Valid options can also be initargs in this scheme.
(defmethod process-options ((obj component) options)
  "The default method on COMPONENT is to run PROCESS-OPTION on each option."
  (dolist (data options)
    (when data
      (apply #'process-option obj (mklist data))))
  obj)

;; Lispworks doesn't do initialization argument validity checking for
;; reinitialize-instance so we do it manually here.
;; This is to stop me from going insane when i wonder why an, unnoticed,
;; initarg typo has no effect.
#+lispworks
(defun valid-initarg-p (object initarg)
  (find initarg (clos:class-potential-initargs (class-of object))))

(defmethod process-option ((comp component) key &rest data)
  #+lispworks
  (assert (valid-initarg-p comp key) (key) "~S is an illegal DEFINE-SYSTEM option." key)
  (reinitialize-instance comp key (first data)))

;; SUPPORTS
(defgeneric featurep (feature)
  (:method ((feature symbol))
   (find feature *features*))
  (:method ((feature cons))
   (let ((dispatch (first feature)))
     (ecase dispatch
       ((:and and) (every 'featurep (rest feature)))
       ((:or or) (some 'featurep (rest feature)))
       ((:not not) (not (featurep (second feature))))))))

(defgeneric check-supported-p (key &rest options)
  (:method ((key (eql :implementation)) &rest options)
   (find (implementation) options))
  (:method ((key (eql :platform)) &rest options)
   (find (platform) options))
  (:method ((key (eql :os)) &rest options)
   (find (os) options))
  (:method ((key (eql :feature)) &rest options)
   (every 'featurep options))
  (:method ((key (eql :not)) &rest options)
   (not (apply #'check-supported-p (first options))))
  (:method ((key (eql :and)) &rest options)
   (every #'(lambda (option)
              (apply #'check-supported-p option))
          options))
  (:method ((key (eql :or)) &rest options)
   (some #'(lambda (option)
              (apply #'check-supported-p option))
          options)))

(defgeneric supports-failed (component)
  (:method ((comp component))
   (let ((if-fails (if-supports-fails comp)))
     (cond ((eql if-fails :error)
            (restart-case (error 'component-not-supported :component comp)
              (continue () :report "Ignore the failure and continue."
                t)))
           ((and if-fails (symbolp if-fails)) (funcall if-fails comp))
           (t (error "~S is an invalid option for :IF-SUPPORTS-FAILS." if-fails))))))

(defmethod component-supported-p ((component component))
  (every #'(lambda (supports)
             (apply #'check-supported-p supports))
         (supports-of component)))

(defgeneric supportedp (component)
  (:documentation "Returns true if component is supported.")
  (:method ((component component))
   "The default method on COMPONENT runs check-supported-p on the entries of the support slot." 
   (if (component-supported-p component)
       t
       (supports-failed component))))

(defmethod process-option ((comp component) (key (eql :supports)) &rest data)
  "<strong>:supports</strong> <i>supports-spec</i>
This keyword is used to indicate which combination of OS/Implementation/Features
must be present in order for the system to be loaded.
Supports spec is of the following form (TYPE . VALUES*)
which specifies that the supports TYPE must be a member of VALUES or a condition of type
component-not-supported is signalled (this can be customized with the :if-supports-fails options).

The following types are specified. <strong>:implementation</strong>, <strong>:os</strong>, <strong>:platform</strong>, <strong>:feature</strong>.

Additionaly <strong>:and</strong>, <strong>:or</strong> and <strong>:not</strong> are allowed for more fine grained control.

eg. (:supports (:and (:os :mswindows) (:implementation :lispworks)))
See check-supported-p, os, implementation, platform"
  (setf (supports-of comp) data))
 
;; NEEDS, REQUIRES AND USES-MACROS-FROM OPTION 
;; Needs definitions are ultimately of the form (match-action component-name [action-to-take])*
;; and are converted into a list of dependency objects.
;; Match action is used to determine wether the dependency is applicable, and is done in the
;; following manner, If match-action is a subclass of the action for the dependency check then
;; the dependency should be processed against component-name (which is resolved to an actual
;; component at dependency check time) using either action-to-take or, if it was not supplied,
;; the action for which the test is happening

(deftype requires-spec ()
  `(satisfies requires-spec-p))

(defun requires-spec-p (thing)
  (flet ((2-elt-list (thing)
           (singlep (cdr thing)))
         (second-elt-is-for-spec (thing)
           (and (consp (second thing))
                (eql (car (second thing))
                     :for))))
    (or (atom thing)
        (singlep thing)
        (and (2-elt-list thing)
             (second-elt-is-for-spec thing)))))

(defmethod process-option ((comp component) (key (eql :requires)) &rest data)
  "REQUIRES is used for implementation specific loading of code. eg. sb-posix in sbcl and osi in acl.
The syntax for specifying the option is
SYNTAX = \(NAME [FOR-SPEC]\)
FOR-SPEC = (:for FEATURE-TESTS)
FEATURE-TESTS = A form suitable for #+ or #-

If no FOR-SPEC is provided the form (NAME) can be simplified to NAME.

eg.
CL-FAD requires OSI for ACL and sb-executable for SBCL, the requires form for this would be
\(:requires (:sb-executable (:for :sbcl)) (:sb-posix (:for :sbcl)) (:osi (:for :acl)))
"
  (setf (requires-of comp)
        (loop for x :in data :collect (make-requires-dependency comp x))))

(defclass requirement ()
  ((on :accessor requirement-on :initarg :on)
   (for :accessor for-of :initarg :for)))

(defun make-requires-dependency (comp spec)
  "Conversts a requires spec into a requirement instance"
  (check-type spec requires-spec)
  (destructuring-bind (on for) (normalize-requirement-spec spec)
    (create-requirement comp on for)))

(defmethod create-requirement ((component component) on for)
  "Create an instance of requirement for COMPONENT using ON and FOR."
  (make-instance 'requirement :on on :for for))

(defun normalize-requirement-spec (spec)
  (cond ((atom spec) (normalize-requirement-spec (list spec)))
        ((singlep spec) (append spec '(nil)))
        (t spec)))

(defmethod component-requirements ((comp component))
  (remove-if-not #'(lambda (req)
                     (if (for-of req)
                         (and (every 'featurep (cdr (for-of req))) t)
                         t))
                 (requires-of comp)))

(defmethod load-requirement ((requirement requirement))
  (require (requirement-on requirement)))


;; Dependencies
(defclass dependency ()
  ((match-action :reader match-action-of :initarg :match-action)
   (component :reader component-of :initarg :component)
   (consequent-action :reader consequent-action-of :initarg :consequent)
   (for :reader for-of :initarg :for :initform nil)))

(defmethod print-object ((dep dependency) stream)
  (print-unreadable-object (dep stream :type t :identity t)
    (format stream "on ~S" (component-of dep))))

(defmethod process-option ((comp component) (key (eql :needs)) &rest data)
"  <strong>:needs</strong> <i>specification</i>*
specification  => ([<imatch-action</i>] <i>dependency</i> [<i>action-to-take</i>] [(:for <i>feature</i>)])
match-action   => class name
dependency     => component name
action-to-take => class name
for            => a symbol or list suitable as an argument to featurep

The :needs option specifies the dependencies that this component has on other components in the same module.

The full specification of a :needs option is as follows
   <tt>([match-action] dependency-name [action-to-take])</tt>
   
match-action is a class name which is used to match the action currently being processed.
action-to-take is a class name designating the class of an action to apply to the dependency.
dependency name is the name of a component in the same module as the component for which the :needs option is being 
processed.
Both match-action and action-to-take can be left unspecified which results in a dependency specification
for the form (<i>dependency-name</i>) and, for the sake of sanity, can be simplified to <i>dependency-name</i>.
When left unspecified, match-action defaults to 'ACTION resulting in a match to all actions while action-to-take
defaults to nil which will result in the current action being invoked being applied to the dependency.

The for option provides a method of specifying per implementation dependencies without relying on #+ or #- reader macros.
This feature is used to allow the creation of a portable `release-action` which can be reliably used across all platforms.

It is not valid to specify action-to-take and leave match-action unspecified.
ie. specifications of the form <tt>(\"macros\" (:needs (\"package\" load-action)))</tt> are NOT allowed.
But it is valid to leave action-to-take unspecified while specifying match action
ie. specifications of the form <tt>(\"macros\" (:needs (source-file-action \"package\")))</tt> are allowed.

Dependencies are processed as follows:
Before an action is processed on a component the applicable dependencies for a particular action & component
combination are run first. A dependency is applicable (see dependency-applicablep) to an action if the match-action
of the dependency is a subclass of the action being processed. 
The appropriate action to apply is then computed using the following rules;
If action-to-take is non-NIL then a new action is created using <tt>(make-instance action-to-take)</tt>, if it is NIL
then the current action being processed is used. This action is then applied to the dependency component which is
looked for in the parent of the component being processed (using find-component).

As an additional note, when using :needs to specify the dependencies between systems a system can be specified
using the (<i>name</i> :version <i>version-specifier</i>) syntax. This can allow us to depend on a particular version of a system.
For example, to depend on version 1.3 of :cl-ppcre you can use the following form.
<tt>(:needs (:cl-ppcre :version (1 3)))</tt>

Example 1.
<tt>(:components \"package\" (\"macros\" (:needs \"package\")))</tt>
=> This specifies that, before performing any action upon the \"macros\" component that the action should 
   first be applied to the package component.
   
Example 2.
<tt>(:components \"package\" 
             (\"macros\" (:needs (source-file-action \"package\"))))</tt>
=> This specifies that, before performing any subclass of source-file-action (eg. compile-action or load-action) against 
   \"macros\" that the action must be applied to \"packages\" first.
   Actions which are not subclasses of source-file-action will not consider this dependency.
   
   
Example 3.
<tt>(:components \"package\"
            (\"my-generated-file\" generated-file)
            (\"macros\"            (:needs (source-file-action \"my-generated-file\" generate-action))))</tt>
=> This will cause a generate-action to be applied to the \"my-generated-file\" component before any
   source-file-action's are applied to the \"macros\" component.


Example 4.
<tt>(define-system :server ()
       (:needs (:sb-posix (:for :sbcl))))</tt>
=> This will create system with a dependency on :sb-posix. However, this dependency will only be processed
   when the system is operated upon by SBCL.
"
   
  (dolist (one-dep (mapcar #'make-dependency-spec data))
    (revpush (make-compilation-dependency one-dep) (needs-of comp))
    (revpush one-dep (needs-of comp))))

(defun make-compilation-dependency (dep)
  (make-instance 'dependency :match-action 'compile-action :component (component-of dep)
                 :consequent 'load-action :for (for-of dep)))

;; Dependency specs (while fully specified are (match-action component-name [action-to-take])
;; match-action can be left out allowing specs of the form (component-name) at which
;; point we allow the parenthesis to be dropped leaving component-name
;; when match-action is not specified it is assumed to be ACTION.
(defun make-dependency-list (spec &aux match comp consequent for)
  (when-let (for-spec (for-spec-p spec))
    (setf spec (butlast spec)
          for for-spec))
  (cond ((atom spec) (setf match 'action comp spec))
        ((versioned-system-name-p spec) (setf match 'action comp spec))
        ((singlep spec) (setf match 'action comp (first spec)))
        ((= (length spec) 2) (setf match (first spec)
                                   comp (second spec)))
        ((= (length spec) 3) (setf (values match comp consequent)
                                   (values-list spec)))
        (t (error "Invalid spec ~S." spec)))
  (list match comp consequent for))

(defun for-spec-p (spec)
  (and (consp spec)
       (consp (car (last spec)))
       (let ((for-spec (car (last spec))))
         (and (= (length for-spec) 2)
              (when (eql (first for-spec) :for)
                (second for-spec))))))

(defun dependency-name (list)
  "Returns the name of the dependency from dependency list LIST."
  (second list))

(defun make-dependency-spec (spec)
  (destructuring-bind (match comp consequent for) (make-dependency-list spec)
    (make-instance 'dependency :match-action match
                   :component comp :consequent consequent
                   :for for)))

(defun versioned-system-name-p (spec)
  "Returns true if spec is a list of the form (name :version version-spec)"
  (and (consp spec)
       (= (length spec) 3)
       (eql (second spec) :version)
       (version-spec-p (third spec))))

(defmethod process-option ((module module) (key (eql :uses-macros-from)) &rest data)
  "<strong>:uses-macros-from</strong> <i>dependency-spec*</i>
:uses-macros-from is a special form of :needs and indicates that, not only does the system
have the indicated dependencies (which are specified in the same manner as :needs),
but that if any dependent system has been modified more recently since the systems
last compilation that the system should be recompiled. This is to ensure that any
changes in macros/inline-functions etc. are updated in systems which specify
dependencies using :uses-macros-from. This option is only used on modules and systems."
  (apply 'process-option module :needs data)
  ;; and add the name of the dependent systems to the uses-macros-from list
  (dolist (dep data)
    (push (dependency-name (make-dependency-list dep))
          (uses-macros-from module))))



;; :COMPONENTS OPTION
;; All component creation form look something like (name [type] . options)
;; type defaults to the default-component-class the parent.
;; options is optional and when the form is only (name) the parenthesis can
;; be dropped.
;; 
;; component-spec -> name || (name [type] . options*)
;; type -> class-name
;; options => (option-key . option-data)
(defun mkspec (spec)
  "Converts all forms of a component into the full expanded (name [type] options*) form"
  (flet ((normalize-spec ()
           (cond ((atom spec) (mkspec (list spec)))
                 ((not (cdr spec)) (list (first spec) ()))
                 ((atom (second spec)) (list* (first spec) (list (second spec))
                                              (cddr spec)))
                 ((consp (second spec)) (list* (first spec) (list)
                                               (cdr spec)))
                 (t spec))))
    (normalize-spec)))


(defvar *previous-components* ())

(defmethod process-option ((system module) (key (eql :components)) &rest args)
  "<strong>:components</strong> <i>component-spec</i>
Adds all components specified by component spec to system.

component-spec is a list of the form
<tt>
COMPONENT*
COMPONENT =&gt; (name [type] [OPTIONS*))
</tt>
If only NAME is provied, ie the component spec is (NAME), then it may be simplified to NAME.
This creates a component of type TYPE with a name of NAME, updates the component with OPTIONS
and adds the component to the systems component list.

examples
 (:components \"packages\" \"macros\" \"functions\")
 (:components \"packages\" (:contrib module (:components \"file1\")))"
  (let ((*previous-components* (components-of system)))
    (setf (components-of system) nil) ; clear out components
    (dolist (file-spec args)
      (apply #'process-option system :component (mkspec file-spec)))))

(defgeneric add-component-to (component system)
  (:method ((comp component) (system module))
   (values comp (revpush comp (components-of system)))))

(defun ensure-uniqueness (name system)
  (when (%find-component system name :errorp nil)
    (error 'duplicate-component :system system :name name)))

(defmethod process-option ((system module) (key (eql :component)) &rest args)
  (destructuring-bind (name (&optional (type (default-component-class-of system)))
                            &rest args) args
    (ensure-uniqueness name system)
    (let ((comp (create-component system name type args)))
      (when (serialp system)
        (when-let (previous-comp (car (last (components-of system))))
          (process-option comp :needs (name-of previous-comp))))
      (add-component-to comp system)
      comp)))


;;; These are options which, instead of only using the first value in DATA use
;;; the entire DATA list.
(defmacro full-data-option (reader key &optional doc)
  `(defmethod process-option ((thing component) (key (eql ,key)) &rest data)
     ,@(when doc (list doc))
     (setf (,reader thing) data)))

(full-data-option version-of :version
                  "<strong>:version</strong> <i>version-numbers*</i>
This sets the version of the system, if not supplied it default to (0 0 1).
Versions are specified as a list of numbers eg.
\(:version 0 1 2)")

(full-data-option keywords-of :keywords "<strong>:keywords</strong> <i>keywords*</i>
This adds various keywords to the system which are used when mb:search'ing through systems.")


;; Development Mode
(defun current-directory ()
  (pathname (directory-namestring *load-truename*)))

(defun setup-development-system-path (system)
  (when (development-mode system)
    (setf (pathname-of system) (current-directory))))

(defmethod process-option :after ((system system) (key (eql :development)) &rest data)
  (declare (ignore data))
  (setup-development-system-path system))

(defmethod initialize-instance :after ((system system) &rest initargs &key)
  (declare (ignore initargs))
  (setup-development-system-path system))

(defmethod reinitialize-instance :after ((system system) &rest initargs &key)
  (declare (ignore initargs))
  (setup-development-system-path system))



;;; VERSION PROCESSING AND DEFAULT SYSTEM FINDER

;; VERSION PROCESSING
(defgeneric version-string (obj)
  (:documentation "Returns the version or version of obj as a string of the form x.y.z")
  (:method ((version string))
   version)
  (:method ((version list))
   (format nil "~{~D~^.~}" version))
  (:method ((comp component))
   (version-string (or (patch-version-of comp) (version-of comp)))))

(defgeneric version-satisfies-p (spec against-spec)
  (:method ((spec t) (against-spec t))
   (let ((version (coerce-to-version spec))
         (against (coerce-to-version against-spec)))
     (assert (exact-version-spec-p version) (version)
       "must designate an exact version")
     (cond ((exact-version-spec-p against)
            (version= version against))
           ((bounding-version-spec-p against)
            (funcall (version-test (first against)) version (rest against)))
           ((complex-version-spec-p against)
            (process-complex-spec version against))
           (t (error "Invalid version spec ~S." against)))))
  (:method ((spec t) (against-spec null))
   t))

(defun create-comparable-version (speca specb &aux (size (max (length speca) (length specb))))
  (values (map-into  (make-list size :initial-element 0)
                     #'identity speca) ;
          (map-into  (make-list size :initial-element 0)
                     #'identity specb)))

(defun version-test (test )
  (let ((sym (intern (format nil "VERSION~:@(~S~)" test) :sysdef)))
    (if (fboundp sym)
        (symbol-function sym)
        (error "No such test ~S."  sym))))

; EXACT AND BOUNDING VERSION TESTS
(defun version= (test against)
  (multiple-value-bind (version against)
      (create-comparable-version test against)
    (equal version against)))

(defun version> (test against)
  (multiple-value-bind (test against)
      (create-comparable-version test against)
    (loop :for v1-component :in test
          :for v2-component :in against
          :when (< v1-component v2-component)
          :do (return nil)
          :when (> v1-component v2-component)
          :do (return t))))

(defun version>= (test against)
  (or (version> test against)
      (version= test against)))

(defun version< (test against)
  (not (version>= test against)))

(defun version<= (test against)
  (not (version> test against)))

(defun version/= (test against)
  (not (version= test against)))


;; PROCESSING COMPLEX DIRECTIVES
;; we only handle AND, OR and NOT at present.
(defun process-complex-spec (version against)
  (ecase (first against)
    (and (every (lambda (sub)
                  (version-satisfies-p version sub))
                (rest against)))
    (or (some (lambda (sub)
                (version-satisfies-p version sub))
              (rest against)))
    (not (assert (eql 1 (length (rest against))) ()
           "NOT specifiers only take 1 argument")
        (not (version-satisfies-p version (first (rest against)))))))

;;; COERCE-TO-VERSION
;; We expect versions to be
;; a) a list of integers
;;    eg. (1 2 3) designating the version 1.2.3
;; b) a list of integers which is preceded by the symbol > or  < or <= or >=
;;    eg. (> 1 2 3) designating a version greater than 1.2.3
;;     or (<= 1 2 3) designating a version less than or equal to 1.2.3
;; c) (a list composed with the symbols AND, OR or NOT in the first position
;;    with the rest of the list composed of versions eg. (and (not (1 2)) (not 1 3))
;; d) a string of the form x.y.z which will be converted to (x y z)
;; e) a wild spec, which has the wildcard, *, as the last component in a version specifiers. eg. (1 *)

(defun coerce-to-version (spec &key (errorp t))
  (cond ((integerp spec) (list spec))
        ((wild-spec-p spec) (normalize-spec spec))
        ((string-version-spec-p spec) (coerce-to-version (split-version-string spec)))
        ((version-spec-p spec) spec)
        (t (if errorp
               (error "Invalid version spec ~S." spec)
               nil))))

(defun versionp (thing)
  (coerce-to-version thing :errorp nil))

(deftype version-designator ()
  `(satisfies versionp))

(defun split-version-string (spec)
  (mapcar (lambda (part)
            (if (string= part "*")
                '*
                (parse-integer part)))
          (split spec :ws '(#\.))))

(defun exact-version-spec-p (spec)
  (or (and (consp spec)
           (car spec)
           (every #'integerp spec))
      (string-version-spec-p spec)))

(defun bounding-version-spec-p (spec)
  (and (consp spec)
       (find (first spec) *boundary-spec-tests*)
       (exact-version-spec-p (rest spec))))

(defun complex-version-spec-p (spec)
  (and (consp spec) (member (first spec) *complex-spec-operators*)
       (every 'versionp (rest spec))))


(defun string-version-spec-p (spec)
  "A string version is an exact version spec in string form. ie. 1.2.3"
  (and (stringp spec)
       (plusp (length spec))
       (every #'(lambda (x) (or (digit-char-p x) (eql x #\.) (eql x #\*))) spec)))

(defun wild-spec-p (spec)
  (and (consp spec)
       (cdr spec) ;; length must be > 1
       (every #'(lambda (x) (or (integerp x) (eql x '*))) spec)
       (eql (first (last spec)) '*)))

(defun normalize-spec (wild-spec)
  (let ((spec (butlast wild-spec)))
    `(and (>= ,@spec)
          (< ,@(butlast spec) ,(1+ (first (last spec)))))))

(defun version-spec-p (spec)
  (or (bounding-version-spec-p spec)
      (complex-version-spec-p spec)
      (exact-version-spec-p spec)
      (wild-spec-p spec)))


;;;PATHNAMES FOR COMPONENTS & MODULES
(defgeneric module-directory (module)
  (:method ((module module))
   (with-slots (directory name) module
     (if (slot-boundp module 'directory)
         (if directory
             (cons :relative (mapcar 'string-downcase (mklist directory)))
             nil)
         (cons :relative (mapcar 'string-downcase (mklist name)))))))

(defgeneric component-name (thing)
  (:method ((thing string)) thing)
  (:method ((thing symbol)) (string-downcase thing)))

(defun absolute-pathname-p (pathname)
  (eql (car (pathname-directory pathname))
       :absolute))

(defun file-name (component)
  (component-name (if (pathname-of component)
                      (name-of (pathname (pathname-of component)))
                      (name-of component))))

(defun file-directory (component)
  (when (pathname-of component)
    (pathname-directory (pathname-of component))))

(defgeneric component-pathname (file)
  (:method :around ((obj component))
   (if (and (pathname-of obj) (not (typep obj 'module)))
       (if (absolute-pathname-p (pathname-of obj))
           (pathname (pathname-of obj))
           (call-next-method))
       (or (pathname-of obj) (call-next-method))))
  (:method ((sys null)) (merge-pathnames (make-pathname :version :newest)
                                         *systems-path*))
  (:method ((system system))
   (merge-pathnames (make-pathname :directory (list :relative (version-string (version-of system))))
                    (call-next-method)))
  (:method ((module module))
   (merge-pathnames (make-pathname :directory (module-directory module))
                    (component-pathname (parent-of module))))
  (:method ((file file)) ;;what about the rest of the components. version etc.
   (merge-pathnames (make-pathname :directory (file-directory file) :type (file-type file) :name (file-name file))
                    (component-pathname (parent-of file)))))


(defgeneric input-file (component)
  (:method ((component file))
   (component-pathname component)))

(defgeneric input-write-date (component)
  (:method ((component file))
   (file-write-date (input-file component))))


(defun legalify (string)
  (let ((sans (remove-if-not #'alphanumericp string)))
    (map-into (make-string (min 10 (length sans)) :element-type 'base-char) 'identity sans)))

(defgeneric fasl-directory (component)
  (:method ((component component)) ".fasl"))
 
(defgeneric fasl-path (component)
  (:method ((component component))
   (make-pathname :directory (cons :relative
                                   (mapcar 'string-downcase
                                           (list (fasl-directory component)
                                                 (legalify (software-type))
                                                 (legalify (lisp-implementation-type))
                                                 (legalify (lisp-implementation-version))))))))


;; While this is remarkably similar to component-pathname we keep them in 2 different methods to
;; allow for seperate customization on file location and the output of FASL's
(defgeneric output-file (component)
  (:method ((component component)) nil)
  (:method ((sys null)) (merge-pathnames (make-pathname :version :newest)
                                         (or *fasl-output-root* *systems-path*)))
  (:method ((module module))
   (or (output-pathname-of module)
       (pathname-of module)
       (merge-pathnames (make-pathname :directory (module-directory module))
                        (output-file (parent-of module)))))
  
  (:method ((system system))
   (or (output-pathname-of system)
       (pathname-of system)
       (merge-pathnames (make-pathname :directory (list :relative (version-string (version-of system))))
                        (call-next-method))))

  (:method :around ((file file))
   (or (output-pathname-of file)
       (call-next-method)))

  (:method ((file lisp-source-file))
   (compile-file-pathname (merge-pathnames (fasl-path file) (call-next-method file))))
  
  (:method ((file file)) ;;what about the rest of the components. version etc.
   (merge-pathnames (make-pathname :type (file-type file)
                                   :name (string-downcase (name-of file)))
                    (output-file (parent-of file)))))

  
(defgeneric output-write-date (component)
  (:method ((component file))
   (file-write-date (output-file component))))

(defgeneric all-files (module &key type)
  (:documentation "Returns a list of all components of TYPE in the modules subtree")
  (:method ((module module) &key (type 'file))
   (loop :for comp :in (components-of module)
         :when (typep comp type) :collect comp
         :when (typep comp 'module) :append (all-files comp :type type)))
  (:method ((comp component) &key (type 'file))
   (when (typep comp type) (list comp))))

(defmacro do-components ((var system &key) &body body)
  "Applies BODY for each component in SYSTEM with VAR bound to the component in question."
  `(dolist (,var (components-of ,system))
     ,@body))


;;; APPLYING ACTIONS TO COMPONENTS

;; Actions Are Only Applicable if they are NOT out of date.
(defgeneric out-of-date-p (component action)
  (:method-combination standard-sysdef-method-combination)
  (:documentation "Returns true if applying ACTION to COMPONENT is necessary.")
  (:method ((component t) (action t))
   (error "The required method OUT-OF-DATE-P is not implemented for ~S and ~S ." component action))
  (:method around ((component component) (action action))
   (if (forcep action)
       t
       (call-next-method)))
  (:method ((component component) (action action))
   t))


; For Load Action
(defmethod out-of-date-p ((file lisp-source-file) (action load-action))
  (or (out-of-date-p file (make-instance 'compile-action))
      (if (and (component-output-exists-p file)
               (component-exists-p file))
          (> (output-write-date file)
             (or (time-of file action) 0))
          t)))

(defmethod out-of-date-p ((file lisp-source-file) (action load-source-action))
  (if (time-of file action)
      (> (input-write-date file) (time-of file action))
      t))

; Compile Action
(defgeneric last-compile-time (component)
  (:method ((system module))
   (let ((all-files  (all-files system :type 'source-file)))
     (if all-files
         (loop :for file :in  all-files
               :maximize (last-compile-time file))
         0)))
  (:method ((component component))
   (safe-write-date component)))

(defgeneric on-macro-use-list (containing-system system)
  (:documentation "Returns true if SYSTEM is on CONTAINING-SYSTEM's uses-macros-from list.")
  (:method ((containing-system module) (system module))
   (find (name-of system) (uses-macros-from containing-system))))

(defun parent-deps-out-of-date-p (action component)
  "Returns true if any of COMPONENT's toplevel dependencies for ACTION are in the :uses-macros-from list
and have a last compile time which is greater than the last compile time of COMPONENT"
  (let* ((my-time (last-compile-time component))
         (toplevel (toplevel-component-of component)))
    (unless (eql toplevel component) ;; need this in case we create a component with no parent.
      (loop for (nil . system) in (dependencies-of action toplevel)
            :thereis (and (on-macro-use-list toplevel system)
                          (> (last-compile-time system) my-time))))))


(defmethod out-of-date-p ((file lisp-source-file) (action compile-action))
  (if (and (output-file file)
           (component-output-exists-p file))
      (or (> (input-write-date file) (output-write-date file))
          (parent-deps-out-of-date-p action file)
          (dependencies-out-of-date-p action file))
      t))

(defun dependencies-out-of-date-p (action file)
  (loop :with my-time = (last-compile-time file)
        :for (nil . dep-comp) :in (dependencies-of action file)
        :thereis (> (last-compile-time dep-comp) my-time)))

(defmethod out-of-date-p ((file lisp-source-file) (action clean-action))
  (component-output-exists-p file))

;; TIME-OF. 
(defgeneric time-of (component action)
  (:documentation "Gives the last(universal-time) that action was applied to component (or nil)")
  (:method ((component component) (action action))
   (time-of component (class-name (class-of action))))
  (:method ((component component) (action symbol))
   (gethash action (operation-times component))))

(defgeneric (setf time-of) (value component action)
  (:method (value (component component) (action action))
   (setf (time-of component (class-name (class-of action))) value))
  (:method (value (component component) (action symbol))
   (setf (gethash action (operation-times component))
         value)))

(defmethod time-of ((component file) (action compile-action))
  (unless (call-next-method)
    (setf (time-of component action) (safe-write-date component)))
  (call-next-method))

(defmethod time-of ((mod module) (action compile-action))
  (reduce #'max (mapcar #'(lambda (file) (time-of file action)) (all-files mod))))

            

;; CALCULATING DEPENDENCIES
(defmethod process-parents-dependencies (component action)
  "This method ensures that all of components parents dependencies are loaded.
This is so we can load any component in a system without loading the entire system and
still be sure that all of the components dependencies are met."
  (when-let (parent (parent-of component))
    (loop for (action . component) in (component-dependencies parent action) :do
          (execute component action))))
  
(defgeneric dependency-applicablep (dependency action)
  (:method ((dependency dependency) (action action))
   (and (if (for-of dependency)
            (and (featurep (for-of dependency)) t)
            t)
        (subtypep (class-of action) (match-action-of dependency))))
  (:method ((dependency dependency) (action clean-action))
   ;; clean-action should not descend to dependencies
   nil))

(defun to-instance (sym-or-object)
  "If argument is an object then return it otherwise call make-instance with it."
  (if (typep sym-or-object 'standard-object)
      sym-or-object
      (make-instance sym-or-object)))


(defmethod immediate-dependencies ((component component) (action action))
  "Returns a list of dependencies for the component which are applicable for action."
  (loop :for dep :in (needs-of component)
        :when (and (dependency-applicablep dep action)
                   (component-applicable-p (find-component (parent-of component) (component-of dep))
                                           action))
        :collect (cons (to-instance (or (consequent-action-of dep) action))
                       (find-component (parent-of component) (component-of dep)))))


(defmethod non-obvious-dependencies ((component component) (action action))
  "This (poorly) named methed returns dependencies for components upon which COMPONENT depends
which have been removed due to not being applicable.
This is to solve a bug (or non obvious behaviour) which occurs when having a serial system and
one of the components has a :for specification which is not applicable. Under this case a break in
the dependency chain is caused. eg
\(define-system :test () (:serial t) (:components \"package\" (\"fixes\" (:for :sbcl)) \"features\")) asdf
This component, when compiled in implementations other than SBCL will not load package before
compiling features as the FIXES components dependencies do not get processed."
  (loop :for dep :in (needs-of component)
        :when (and (dependency-applicablep dep action)
                   (not (component-applicable-p (find-component (parent-of component) (component-of dep)) action)))
        ;; now collect all the component-dependencies of the dependent component
        :append (component-dependencies (find-component (parent-of component) (component-of dep))
                                        action)))
        
(defgeneric component-dependencies (component action)
  (:method ((component component) (action action))
   (append (non-obvious-dependencies component action)
           (immediate-dependencies component action)))
          
  ;; we rely on th install action to process the dependcies for us
  ;; If this method is not present then the :before action on execute
  ;; for processing dependencies will download the systems in a non
  ;; obvious order
  (:method ((component system) (action install-action)) nil))

(defgeneric action-dependencies (action component)
  (:method ((action action) (component component))
   (loop for dep-action in (needs-of action)
         collect (cons (make-instance dep-action) component))))

(defgeneric dependencies-of (action comp)
  (:method ((action symbol) component)
   (dependencies-of (make-instance action) component)))

(defmethod dependencies-of ((action action) (comp component))
  (append (action-dependencies action comp) (component-dependencies comp action)))

;; The following ensures that we only ever execute a particular (action . component)
;; combination once in the context of an execute call
(defun already-processed-p (component action)
  (cdr (assoc (cons component (class-of action))
              *processed-actions* :test 'equalp)))

(defun add-processed-action (component action)
  (acons (cons component (class-of action)) t *processed-actions*))

(defmethod execute around ((component component) (action action))
  (labels ((process-action ()
             (cond ((already-processed-p component action) t)
                   (t (prog1
                          (setf *processed-actions*
                                (add-processed-action component action))
                        (when (out-of-date-p component action) (call-next-method))))))
           (process-with-bound-var ()
             (if *processed-actions*
                 (process-action)
                 (let ((*processed-actions* (acons nil nil nil)))
                   (process-action)))))
    (loop
     (restart-case (progn (let ((*package* *package*)
                                (*readtable* *readtable*))
                            (process-with-bound-var))
                     (setf (time-of component action) (get-universal-time))
                     (return component))
       (retry () :report (lambda (s) (format s "Retry ~A on ~A." (name-of action) component)))
       (ignore () :report (lambda (s) (format s "Ignore ~A on ~A." (name-of action) component))
         (return))))))


;; AND THE EXECUTE METHODS THEMSELVES
(defmethod installablep ((system system))
  (provider-of system))

(defmethod execute :before ((system system) (action file-action))
  (unless (component-exists-p system)
    (restart-case (when (installablep system)
                    (error 'system-not-installed :system system))
      (install () :report "Install" (execute system 'install-action)))))

(defmethod execute :before ((component component) (action action))
  (when (supportedp component)
    (dolist (requirement (component-requirements component))
      (load-requirement requirement))
    (loop for (dep-action . dep-component) in (dependencies-of action component) :do
          (execute dep-component dep-action))))


(defgeneric component-exists-p (component)
  (:documentation "Returns T if the component is present on the file system.")
  (:method ((component module))
   ;; clisp doesn't support probe-file on directories
   #+clisp
   (not (null (directory (component-pathname component))))
   #-clisp (call-next-method))
  (:method ((component component))
   (probe-file (component-pathname component))))

(defgeneric component-output-exists-p (component)
  (:method ((component source-file))
   (probe-file (output-file component))))

(defmethod execute :before ((component source-file) (action source-file-action))
  (unless (component-exists-p component)
    (error "Source File ~S does not exist." component)))

(defmethod execute ((module module) (action action))
  (dolist (comp (applicable-components module action))
    (execute comp action)))

(defgeneric applicable-components (module action)
  (:method ((module module) (action action))
   (remove-if-not #'(lambda (comp) (component-applicable-p comp action))
                  (components-of module))))

(defgeneric component-applicable-p (component action)
  (:method ((component component) (action action))
   t)
  (:method ((component component) (action source-file-action))
   (readablep component))
  (:method ((modue lazy-module) (action action))
   nil))

;; Our install action
;; We do some special stuff here for to tie the install action in such that
;; a) we don't have to load the dependencies required to install until the last moment
;; b) we can provide an `install` restart when the system is not installed.
(defmethod execute ((system system) (action install-action))
  (execute 'installer 'load-action)
  (funcall (find-symbol "INSTALL" (find-package :installer))
           system))

;;; LOAD ACTIONS
(defmethod execute :before ((system system) (action load-action-mixin))
  (when (deprecatedp system)
    (warn 'deprecated-system :system system :by (unless (eql (deprecatedp system) t)
                                                  (deprecatedp system)))))

;; Load Source Action
(defmethod execute ((component lisp-source-file) (action load-source-action) )
  (load (input-file component)))


; Load Action
(defgeneric ensure-output-path-exists (file)
  (:method ((file lisp-source-file))
   (ensure-directories-exist (output-file file))))

(defmethod ensure-existence-of ((component component))
  (unless (component-exists-p component)
    (error 'missing-component :component component)))

(defmethod execute :before ((comp lisp-source-file) (action source-file-action))
  (ensure-existence-of comp)
  (ensure-output-path-exists comp))

(defmethod execute around ((file lisp-source-file) (action load-action))
  (handler-bind ((fasl-error (ecase *load-fails-behaviour*
                               (:error (constantly nil))
                               (:compile (make-restarter 'compile-system))
                               (:load-source (make-restarter 'load-source)))))
    (handler-case (call-next-method)
      ;; invalid FASL recompilation
      (#+sbcl sb-ext:invalid-fasl #+allegro (or excl::file-incompatible-fasl-error file-error)
        #+lispworks conditions:fasl-error #+cmu ext:invalid-fasl
        #-(or sbcl allegro lispworks cmu) fasl-error ()
        (execute file 'clean-action)
        (execute file 'compile-action)
        (call-next-method)))))

(defmethod execute ((component lisp-source-file) (action load-action))
  (flet ((do-load ()
           (cond ((not (component-output-exists-p component))
                  (error 'fasl-does-not-exist :file (output-file component)))
                 ((> (input-write-date component) (output-write-date component))
                  (error 'fasl-out-of-date :file (output-file component)))
                 (t (load (output-file component))))))
    (restart-case (do-load)
      (compile-system ()
        :report "Compile the component and retry."
        ;; for some reason using :test fails in CLISP 2.41 and CMUCL (19d)
        ;:test (lambda (c) (typep c 'fasl-error)) 
        (execute (parent-of component) 'compile-action)
        ;; the act of compiling the parent can complete the action for us.
        (unless (out-of-date-p component action)
          (execute component action)))
      (load-source ()
        :report "Load source file instead of fasl."
        ;:test (lambda (c) (typep c 'fasl-error))
        ;; TODO: shouldn't this be (execute comp (make-instance 'load-source-action))
        (load (input-file component)))))
  component)

; Compile Action
(defmethod execute around ((sys system) (action compile-action))
  (with-compilation-unit ()
    (call-next-method)))

(defmethod execute ((component lisp-source-file) (action compile-action))
  (multiple-value-bind (output-file warnings-p failure-p)
      (compile-file (input-file component)
                    :output-file (output-file component))
    (when warnings-p
      (case *compile-warns-behaviour*
        (:error (error 'compile-warned :file component))
        (t nil)))
    (when failure-p
      (case *compile-fails-behaviour*
        (:warning (warn "COMPILE-FILE failed while compiling ~S." component))
        (:error (error 'compile-failed :file component))
        (t nil)))
    (unless output-file (error 'compilation-error :file (input-file component))))
  component)

; Clean Action
(defmethod execute ((component source-file) (action clean-action))
  (when-let (output-file (component-output-exists-p component))
    (format *verbose-out* "~&DELETE ~A~%" output-file)
    (delete-file output-file)))

(defun eql-specializer-p (specializer)
  (and (consp specializer) (eql (car specializer) 'eql)))

(defun specialized-options (methods)
  (loop :for method :in methods
        :for key-specializer = (second (method-specializers method))
        :when (eql-specializer-p key-specializer) :collect (second key-specializer)))

(defgeneric specialized-option (thing)
  #-abcl
  (:method ((thing effective-slot-definition))
   (copy-list (slot-definition-initargs thing)))
  (:method ((thing standard-method))
   (specialized-options (list thing))))

(defvar *do-not-document*
  '(:contact :author :maintainer :licence :license :contact
    :documentation :name :component :parent :provider))

(defun symbol< (a b)
  (string< (string a) (string b)))

#-abcl
(defun possible-define-system-options ()
  "this is a helper method which creates some html text from slots and
process-option methods to create a guaranteed up to date option list
for define-system. This has only been tested with Lispworks at the moment. Sorry :("
  (flet ((initargable-slots ()
           "Returns all slots for the system class which can be set with an initarg"
           (remove-if-not 'slot-definition-initargs (class-slots (find-class 'system)))))
    (let* ((methods (remove-if 'method-qualifiers (generic-function-methods #'process-option)))
           (method-keys (specialized-options methods))
           (slots (remove-if #'(lambda (slot) (intersection (slot-definition-initargs slot) method-keys))
                             (initargable-slots)))
           (to-document (nconc slots methods)))
      (let ((documented (sort (remove-if-not (lambda (x) (documentation x t)) to-document) #'symbol<
                              :key (lambda (x) (first (specialized-option x))))))
        (values (format nil "<strong>Possible Options:</strong>~%~{~@[~&~%~A~^~%~]~}"
                        (mapcar (lambda (x) (documentation x t)) documented))
                (set-difference (mapcan 'specialized-option (set-difference to-document documented))
                                *do-not-document*))))))

(defmethod documentation ((name (eql 'define-system)) (key (eql 'function)))
  (multiple-value-bind (documented undocumented) (possible-define-system-options)
    (with-output-to-string (outs)
      (princ (call-next-method) outs)
      (princ documented outs)
      (when undocumented
        (format outs "~&~%The following options are not currently documented;~%~{~S~^, ~}" undocumented)))))

;;; SYSTEM TRAVERSAL
(defmacro do-systems ((var) &body body)
  "Executes body for each system defined with VAR bound to an instance of SYSTEM."
  `(dolist (,var *systems*)
     (declare (ignorable ,var))
     ,@body))

(defun map-systems (fn)
  "Applies fn to each system available."
  (mapcar fn *systems*))

(defun systems-matching (fn &key (key #'identity))
  "Returns all systems for which (funcall FN system) returns true."
  (remove-if-not fn *systems* :key key))

;;; SYSTEM CREATION AND LOCATION
(defun undefine-system (system)
  "Removes SYSTEM from the set of defined systems."
  (setf *systems* (delete system *systems*)))
  
(defmacro define-system (name (&rest superclasses) &body options)
  (declare (system-name name))
  "Define a system called NAME of a class with superclasses SUPERCLASSES and customized using OPTIONS.
NAME may be a symbol, which defines a system called name, or a list
of the form (SYSTEM [MODULES*] NAME) which defines a new MODULE on component
found by descending into systems components. See SYSTEM-NAME.
SUPERCLASSES is a list then an anonymous class is created which is used as the class for the system.

None of the arguments are evaluated.
OPTIONS = OPTION*
OPTION = (key . values)
OPTIONS are processed using process-options.

Systems are unique on a name (tested using string-equal), version basis.

"
  (if (multiple-version-definitions-p options)
      `(progn
         ,@(expand-multiple-versions name superclasses options))
      `(fn-define-system ',name ',superclasses nil ',options)))



;; Templates and Stub systems
;;
;; At one point system definition files contained all the code to define a system, including
;; supporting lisp code. When I created the website and system upload and creation part of
;; the site, giving anonymous users (and in order to have good uptake anonymous users are a must)
;; the ability to run random code whoever downloads a system definition file created a rather
;; ugly security hole.
;; So we reverted to Templates and stub systems.
;; Templates are just that, a template which is used to fill in a system at definition time.
;; Stub systems, created by define-available-version creates a system of type stub-system with
;; some searchable information filled in. At system load time (before the perform method)
;; stub systems get resolved to proper systems by loading the system definition file which is to
;; reside in the systems root. This system definition file can contain all the code necessary
;; to properly define the system.
(defun create-template-holder ()
  (make-hash-table :test #'equalp))

(defvar *templates* (create-template-holder))

(defun get-template (name)
  (gethash name *templates*))

(defun (setf get-template) (new-value name)
  (if new-value
      (setf (gethash name *templates*) new-value)
      (remhash name *templates*)))

(defmacro define-system-template (name &body options)
  `(setf (get-template ',name) ',options))


(defclass stub-system (system) ())

(defclass system-definition-file (lisp-source-file) ())

(defmacro define-available-system (name (&rest classes) &body options)
  `(define-system ,name (,@classes stub-system) ,@options))

;; This magic works thanks to the fact that create-component uses change-class to
;; change to stub system to a proper system when the new system definition is loaded
;; at which point the normal perform method kicks in and passes the system to execute
(defmethod perform :before ((system stub-system) (action action) &rest action-data &key &allow-other-keys)
  (load-system-definition system))

(defmethod execute :before ((system system) (action action))
  (load-system-definition system))

(defmethod load-system-definition ((system system))
  (when-let (sdc (system-definition-component system))
    (execute sdc 'load-action)))
  
(defmethod load-system-definition ((system stub-system))
  (let ((sdc (or (system-definition-component system)
                 (create-component system :system-definition 'system-definition-file
                                   `((:pathname ,(system-definition-pathname system)))))))
    (setf (system-definition-component system)
          (execute sdc 'load-action))))

(defgeneric system-definition-pathname (system)
  (:method ((system system))
   (merge-pathnames (make-pathname :name (component-name (name-of system)) :type "mbd")
                    (component-pathname system))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun multiple-version-definitions-p (options)
  (get-from-options options :versions))

(defun expand-multiple-versions (name superclasses options)
  (let ((without-versions-and-md5sums (remove :md5sums (remove :versions options :key 'car) :key 'car))
        (versions (get-from-options options :versions))
        (md5sums (get-from-options options :md5sums)))
    (assert (get-from-options options :versions)
        (options) "Options does not contain a :versions form.")
    (apply 'mapcar #'(lambda (version md5sum)
                       `(define-system ,name ,superclasses
                          (:version ,@version)
                          ,@(when md5sum (list (list :md5sum md5sum)))
                          ,@without-versions-and-md5sums))
           (same-length versions md5sums))))

(defun same-length (&rest lists)
  "Returns the lists passed in as lists of the same length. Short lists are padded with NILs"
  (let ((max-size (reduce 'max lists :key 'length)))
    (mapcar #'(lambda (list)
                (map-into (make-list max-size) 'identity list))
            lists)))

(defun get-from-options (options key &optional default)
  (or (loop :for (keyword . value) :in options
            :thereis (and (eql key keyword) value))
      default))

) ;;eval-when


(deftype subsystem-definition ()
  'cons)

(defun fn-define-system (name superclasses parent options)
  (check-type name system-name)
  (with-simple-restart (skip "Skip the definition of ~S." name)
    (if (subsystem-definition-p name)
        (let ((class (module-class superclasses))
              (parent (find-system-for name
                                       (get-from-options options :version (first-version))))
              (name (first (last name))))
          (add-component-to (create-component parent name class options) parent))
        (register-system (create-component parent name (system-class superclasses) options)))))

(defun on-cpl (class classes)
  (some (lambda (super)
          (find class (class-precedence-list super)))
        classes))

(defun list-classes (superclasses &key with)
  (flet ((as-class (x)
           (let ((class (if (typep x 'standard-class)
                            x
                            (find-class x))))
             (finalize-inheritance class)
             class)))
    (let ((classes (mapcar #'as-class superclasses)))
      (nconc classes
             (when (and with (not (on-cpl (as-class with) classes)))
               (list (as-class with)))))))

;; from AMOP
(defun find-programattic-class (superclasses)
  (if (singlep superclasses)
      (first superclasses)
      (let ((class (find-if #'(lambda (class)
                                (equal superclasses (class-direct-superclasses class)))
                            (class-direct-subclasses (car superclasses)))))
        (if class
            class
            (make-programmatic-class superclasses)))))

(defun make-programmatic-class (superclasses)
  (make-instance 'standard-class
                 ;; CLISP and CMUCL only supports symbols as class names
                 :name #-(or clisp cmu) (mapcar 'class-name superclasses)
                       #+(or clisp cmu) (intern (format nil "ANON-~{~:@(~A~)~^-~}" (mapcar 'class-name superclasses))
                                                :keyword)
                 :direct-superclasses superclasses
                 :direct-slots ()))

(defun system-class (superclasses)
  "Creates a class which is used to instantiate a system. If superclasses only has one entry
then that entry is returned otherwise a new instance of standard class is created which superclasses
as the direct superclasses."
  (if superclasses
      (find-programattic-class (list-classes superclasses :with 'system))
      (find-class 'system)))
                    
(defun module-class (superclasses)
  (if superclasses
      (find-programattic-class (list-classes superclasses :with 'module))
      (find-class 'module)))

(defun subsystem-definition-p (name)
  (typep name 'subsystem-definition))

(defun find-system-for (name-list version)
  "Takes a list of (system-name modules* new-module) and finds the appropriate
module to be the parent of new-module with version VERSION."
  (let* ((system (find-component (list (first name-list) :version version)))
         (parent (apply #'find-component system (butlast (rest name-list)))))
    (check-type parent module)
    parent))

(defun system-key ()
  #'(lambda (sys) (list (name-of sys) (version-string (version-of sys)))))

(defun register-system (system)
  (setf *systems* (delete (funcall (system-key) system) *systems* :test #'equalp :key (system-key)))
  (pushnew system *systems* :test #'equalp :key (system-key))
  system)


;; CREATE-COMPONENT
(defun extract-option (name options)
  (cdr (find name options :key 'first)))

(defun default-initargs (instance)
  "Returns a list which can be used to reinitialize-instance to reset slots
to their class defaults."
  (flet ((compute-initarg (spec)
           (destructuring-bind (initarg value initfn) spec
             #+(or allegro cmu)
             (rotatef value initfn)
             (list initarg
                   #+lispworks
                   (if (integerp initfn)
                       (case initfn
                         (1 value)
                         (3 (eval value)))
                       (funcall initfn))
                   #-lispworks (funcall initfn)))))
    (loop :for initarg-spec :in (class-default-initargs (class-of instance))
          :append (compute-initarg initarg-spec))))

(defclass empty-class () ())
          
(defmethod reset-instance ((instance component))
  (let ((saved-slots (loop :for slot in *saved-slots*
                           :when (and (slot-exists-p instance slot)
                                      (slot-boundp instance slot))
                           :collect (cons slot (slot-value instance slot))))
        (class (class-of instance)))
    (change-class (change-class instance 'empty-class) class)
    (apply 'reinitialize-instance instance (default-initargs instance))
    (loop :for (slot . value) in saved-slots :do
          (setf (slot-value instance slot) value))
    instance))

(defun existing-component (parent name version)
  (or (find name *previous-components* :key 'name-of :test 'name=)
      (%find-component parent name :errorp nil :version version)))
  
(defgeneric create-component (parent name class &optional options &key existing-component)
  (:method (parent name class &optional options &key existing-component)
   (let* ((current (or existing-component (existing-component parent name (extract-option :version options)))))
     (when (and current (typep current 'system) (not (typep current 'stub-system)))
       (warn 'system-redefined :name (name-of current) :version (version-string current)))
     
     (when (and current (not (eql (class-of current) class)))
       (change-class current class))
     
     (let ((component (or current (make-instance class))))
       (when current
         (reset-instance component))
       
       (when parent
         (copy-slots parent component *inherited-slots*)
         (setf (parent-of component) parent))

       (process-option component :name name)
       
       (when (not parent)
         (fill-from-template component))
       
       (process-options component options)
       
       component))))

(defmethod fill-from-template ((object component))
  (loop for (option . values) in (get-template (name-of object)) :do
        (apply 'process-option object option values)))
     

;; This is only to be used by copy-slots and is not to be extended.
(defgeneric %copy-object (object)
  (:method ((obj t)) (error "Cannot inherit ~S." obj))
  (:method ((num number)) num)
  (:method ((sym symbol)) sym)
  (:method ((char character)) char)
  (:method ((sequence sequence)) (copy-seq sequence))
  ;; We also do hash-tables to allow us to propogate operation-times
  (:method ((hash hash-table))
   (let ((ret (make-hash-table :size (hash-table-size hash)
                               :test (hash-table-test hash)
                               :rehash-size (hash-table-rehash-size hash)
                               :rehash-threshold (hash-table-rehash-threshold hash))))
     (maphash #'(lambda (key value) (setf (gethash key ret) value)) hash)
     ret)))

(defun copy-slots  (from to slot-names &key (copy t))
  (dolist (name slot-names)
    (when (and (slot-exists-p from name)
               (slot-exists-p to name))
      (if (slot-boundp from name)
          (setf (slot-value to name)
                (funcall (if copy #'%copy-object #'identity)
                         (slot-value from name)))
          (slot-makunbound to name)))))





;; SINGLE FILE SYSTEMS
(defun single-file-system-hash ()
  (make-hash-table :test #'equal))

(defvar *single-file-systems*  (single-file-system-hash)
  "A hash table containing a mapping of name => component for single file systems.")

(defun single-file-system (name)
  (values (gethash (namestring name) *single-file-systems*)))

(defun (setf single-file-system) (newval name)
  (if newval
      (setf (gethash (namestring name) *single-file-systems*) newval)
      (remhash (namestring name) *single-file-systems*)))
              

(defparameter *search-paths* (list #+mswindows "/Program Files/Lisp/" #+linux "/usr/lib/lisp/" #+mac "/Library/Lisp/"
                                   (merge-pathnames (make-pathname :directory '(:relative "lisp"))
                                                    (user-homedir-pathname)))
  "A list containing strings or pathnames which are used as root directories to find single file systems.
New pathnames can be added to the list to add new search paths.")

(declaim (type (member :warn :error nil) *multiple-matching-files-behaviour*))
(defvar *multiple-matching-files-behaviour* :warn
  "Specifies the behaviour to take when more than one file is found when looking for single file systems.
The valid values are  :warn, :error or nil. When :warn a warning of type multiple-file-warning will be signalled.
When :error an error of type multiple-file-error will be signalled, and when nil the first file found will be used.
When conditions are signalled as restart named :USE will be made available to specify which file to use \(A list of
files can be found in the FILES slot in the condition.")

(define-condition multiple-file-condition (condition)
  ((name :reader name-of :initarg :name :initform (error "name is required"))
   (files :reader files-of :initarg :files :initform ()))
  (:documentation "The root condition signalled when multiple matching files are found.")
  (:report (lambda (c s)
             (print-unreadable-object (c s :type t :identity t)
               (format s "Multiple files were found (~{~A~^, ~}) matching ~S." (files-of c) (name-of c))))))

(define-condition multiple-file-warning (warning multiple-file-condition) ()
  (:documentation "Signalled to warn the user that multiple files are found and that the first one will be used."))

(define-condition multiple-file-error (error multiple-file-condition) ()
  (:documentation "The error to tell the user that multiple files are found and an appropriate one must be picked."))

(defun read-new-value ()
  (format *query-io* "Enter the file name to use: ")
  (let ((val (read-line *query-io*)))
    (list (if (stringp val) val (string val)))))
        
(defun all-single-files (name)
  (loop :for load-path :in *search-paths*
        :when (probe-file (proper-file-name load-path name))
        :collect :it))


(defmacro with-use-restart (&body body)
  `(restart-case (progn ,@body)
     (:specify (file)
       :report "Specify the file to use."
       :interactive read-new-value
       (return-from find-single-file file))))

(defun find-single-file (name)
  (let ((matches (all-single-files name)))
    (when (cdr matches)
      (ecase *multiple-matching-files-behaviour*
        ((nil) (first matches))
        (:warn (with-use-restart (warn 'multiple-file-warning :files matches :name name)))
        (:error (with-use-restart (error 'multiple-file-error :files matches :name name)))))
    (first matches)))

(defun proper-file-name (load-path name)
  (merge-pathnames (expand-single-path name)
                   load-path))

(defun expand-single-path (name)
  (let ((components (split name :ws '(#\;))))
    (make-pathname :directory (cons :relative (butlast components))
                   :name (car (last components))
                   :type "lisp")))


(defun single-file-specifier-p (name)
  (and (stringp name) (starts-with name #\;)))

(deftype single-file-specifier ()
  `(and string (satisfies single-file-specifier-p)))

(defun starts-with (string char)
  (and (plusp (length string))
       (eql char (char string 0))))

(defun minus-first (string char)
  (if (eql (char string 0) char)
      (make-array (- (length string ) 1)
                  :element-type (array-element-type string)
                  :displaced-to string
                  :displaced-index-offset 1)
      string))

(defclass single-file-system (lisp-source-file) ()
  (:default-initargs :type nil)
  (:documentation "Single-file-systems are the root class of all components which are looked up using the
single file system mechanism.

Single file systems are an attempt to provide a simple, yet useful mechanism for operating on files using
the normal mudballs commands without having an external system definition defined for them.

These components are specially named by preceding the filename with a semicolon \(#\;\). This indicates that
a single file system is requested and mudballs will search on *search-paths* to locate the file.
Files which are deeper in a directory heirarchy can be referenced using a directory-name;filename syntax
and can be arbitrarily deeply nested.

When a single file is requested \(eg. \";package\" \) and multiple files are matched  \(eg. with a *search-paths* of
\(#P\"/lisp/\" #P\"/workfiles\") and a file named package.lisp in both directories\) a warning will be signalled and
the file found will be used. This behaviour can be customized by changing the value of *multiple-matching-files-behaviour*.

These single file systems can contain a component form (which is also exported by the mudballs package) to the top of the
file to customize the component which will be created for the file. This form contains options, as per define-system,
which are used to customize the component.

eg.
<tt>
#+mudballs (mb:component (:needs :alexandria))
</tt>

This will specify that this file has a dependency on the system named alexandria. See define-system for more options.

"))

(defun find-single-file-system (system-name &key version)
  (declare (ignore version))
  (when (typep system-name 'single-file-specifier)
    (let* ((name (minus-first system-name #\;))
           (path (find-single-file name)))
      ;; TODO: If we find a file and that file is subsequently deleted
      ;; we will never delete the component that was created for it.
      (when path
        (list (find-or-create-system path))))))



(defvar *max-forms-to-read* 5 "The number of forms we will read while looking for an mb:component form.")

(defun extract-options (path) ;; this needs to pull out options from the file.
  (with-open-file (stream path)
    (rest (find-component-form stream))))

(defun find-component-form (stream)
  (loop :repeat *max-forms-to-read*
        :for form = (read stream nil nil nil)
        :when (eql (car form) 'component)
        :return form))

(defun find-or-create-system (path)
  (handler-bind ((system-redefined #'muffle-warning))
    (let* ((current (single-file-system path))
           (new (create-component nil (namestring path)
                                  'single-file-system
                                  (extract-options path)
                                  :existing-component current)))
      (setf (single-file-system path) new))))

(defmethod output-file ((component single-file-system))
  (compile-file-pathname (component-pathname component)))

(defmethod component-pathname ((component single-file-system))
  (pathname (name-of component)))

(defmacro component (&rest options)
  "Specifies that the file is a component.  Options are the same as provided to define-system
and are used to modify the component when it is created.
It's worth noting that this function does no work, but rather the files are read to determine
how to create a component for them."
  (declare (ignore options))
  nil)




;; Locating components
;; remember. If no version is requested then you must try to return an installed system.
(defun %find-system (name version &key (errorp t))
"%FIND-SYSTEM works as follows
It first looks for a currently loaded version of the system. If this exists and satisfies the version requested
then it is used. It then walks though the *finders* list funcalling each function on it with NAME and key VERSION.
Each function is expected to return 2 results. The first is a list of installed systems
matching the arguments and the second is a list of uninstalled arguments matching the arguments.
The first returned `installed-system` will be returned by %find-system. If no installed systems are found
then the largest version uninstalled version wil be used.
If still no version has been found and ERRORP is true then an error of type no-such-component will be signalled."
  (labels ((use (thing) (return-from %find-system thing)))
    (let* ((loaded-system (system-loaded-p name)))
      (when (and loaded-system (version-satisfies-p (version-of loaded-system) version))
        (use loaded-system))
      
      (let ((uninstalled ()))
        (dolist (finder *finders*)
          (multiple-value-bind (installed-systems other-systems)
              (funcall finder name :version version)
            ;; if a specific version is requested return the first one we find
            ;; whether it is installed or not. This effectively shortcuts running the rest of *finders*
            (cond ((and version installed-systems); other-systems))
                   (use (or (first installed-systems) (first other-systems))))

                  ;; if we find a supported system then use that 
                  (installed-systems (use (first installed-systems)))

                  ;; otherwise collect the uninstalled systems for use later
                  (t (setf uninstalled (sort (union uninstalled other-systems) #'version> :key 'version-of))))))

        ;; by this point we have no installed systems and a list of uninstalled systems.
        ;; so we use the first uninstalled version.
        (when uninstalled
          (use (first uninstalled)))
        
        ;; by this point we have no systems returned at all.
        (when errorp
          (restart-case (error 'no-such-component :name name :version version)
            (retry ()
              :report "Try Again."
              (%find-system name version :errorp errorp))))))))

(defvar *in-find-system* nil)

(defgeneric find-system (name &rest args &key errorp &allow-other-keys)
  (:method :around ((name t) &rest args)
   (declare (ignore args))
     (unless (builtin-system-p name)
       (unless *in-find-system*
         (let ((*in-find-system* t))
           (register-sysdefs))))
     (call-next-method))
  (:method ((system system) &rest args &key errorp)
   (declare (ignore args errorp))
   system)
  (:method (name &rest args &key (errorp t) (version nil version-supplied?) &allow-other-keys)
   (declare (ignorable args))
   (%find-system name (if version-supplied? version nil) :errorp errorp)))

   
(defun find-component (&rest args)
  (labels ((to-component (parent thing)
             (if (and (null parent) (typep thing 'component))
                 thing
                 (apply '%find-component parent (mklist thing))))
           (one-loop (parent rest)
             (if (endp rest)
                 parent
                 (one-loop (to-component parent (car rest))
                           (cdr rest)))))
    (one-loop nil (without-leading nil args))))


(defgeneric %find-component (parent name &key errorp version)
  (:method ((parent (eql nil)) name &key (errorp t) (version nil version-supp-p))
   (%find-system name (when version-supp-p version) :errorp errorp))
  (:method ((parent module) name &key (errorp t) version) ;; Version is unused here
   (declare (ignore version))
   (or (find name (slot-value parent 'components) :key #'name-of :test #'name=)
       (when errorp (error 'no-such-component :name name :parent parent)))))

(deftype system-designator ()
  "The type system-designator denotes the set of lisp objects which can be used to lookup a system.
It is either a string or symbol designating the system with that name, or a system designating itself."
  `(or string symbol system))

;; This could do with a better name
(defun normalize (name)
  (check-type name system-designator)
  (string name))

(defun name= (a b)
  "Returns true if the A and B are considered equal as system names."
  (equalp (normalize a) (normalize b)))

(defun available-systems (name)
  (remove name *systems* :key 'name-of :test-not 'name=))

(defun systems-for (name &key (version '(>= 0)))
  (sort (systems-matching (lambda (sys)
                            (and (name= (name-of sys) name)
                                 (version-satisfies-p (version-of sys) version))))
        'version>
        :key 'version-of))

;; name => system mapping for loaded systems

(defgeneric system-loaded-p (system-designator)
  (:documentation "Returns the current version of the system designated by SYSTEM-DESIGNATOR that
has been loaded into the the current Lisp image or nil.")
  (:method ((sys system))
   (system-loaded-p (name-of sys)))
  (:method ((name t))
   (values (gethash (normalize name) *loaded-versions*))))

(defgeneric (setf system-loaded-p) (newval designator)
  (:method ((newval system) designator) 
   (setf (gethash (normalize designator) *loaded-versions*) newval))
  (:method ((name null) designator)
   (let ((system (gethash (normalize designator) *loaded-versions*)))
     (do-components (component system)
       (setf (time-of component 'load-action) nil))
     (remhash (normalize designator) *loaded-versions*))))

(defmethod execute :after ((system system) (action load-action))
  (setf (system-loaded-p (name-of system)) system)
  (ensure-dependencies-up-to-date system)
  (load-patches system)
  (load-config system)
  (maybe-load-conduit-systems system))

(defvar *mudballs-loaded* nil)
(defgeneric ensure-dependencies-up-to-date (system)
  (:method ((system system))
   (when *mudballs-loaded*
     (maphash #'(lambda (name sys)
                  (declare (ignore name))
                  (when (on-macro-use-list sys system)
                    (execute sys 'load-action)))
              *loaded-versions*))))

(defmethod execute :before ((system system) (action source-file-action))
  (let ((loaded-system (system-loaded-p (name-of system))))
    (when (and loaded-system (not (version= (version-of system) (version-of loaded-system))))
      (cerror "Load system anyway." 'system-already-loaded :name (name-of system)
              :loaded-version (version-of loaded-system) :requested-version (version-of system))
      (setf (system-loaded-p (name-of system)) nil
            (time-of loaded-system (make-instance 'load-action)) nil))))

(defun default-system-finder (name &key version)
  (loop :for system :in (systems-for name :version version)
        :if  (component-exists-p system)
        :collect system :into installed
        :else :collect system :into not-installed
        :finally (return (values installed not-installed))))
                
;;; WILDCARD MODULES
;;; Modules which load all files in their directory.
;;; This is done by doing a wildcard directory search in the directory
;;; and adding components for all files found matching the modules suffix
(defmethod name-of ((thing pathname))
  (pathname-name thing))

(defclass wildcard-module (module)
  ((suffix :accessor suffix-of :initarg :suffix :initform "lisp")
   (ordered :accessor ordered-of :initarg :ordered :initform nil)
   (cached-components :accessor cached-components-of :initform nil)))


;; This could all do with some cleaning up..
(defvar *special-directory-keywords* '(:wild :wild-inferiors :back :up))


(defmethod component-pathname ((module wildcard-module))
  (let ((next (call-next-method)))
    (if (and (slot-boundp module 'directory) (member (directory-of module) *special-directory-keywords*))
        (merge-pathnames (make-pathname :directory (butlast (pathname-directory next)))
                         next)
        next)))

(defmethod output-file ((module wildcard-module))
  (let ((next (call-next-method)))
    (if (and (slot-boundp module 'directory) (member (directory-of module) *special-directory-keywords*))
        (merge-pathnames (make-pathname :directory (butlast (pathname-directory next)))
                         next)
        next)))

(defgeneric wildcard-pathname-of (module)
  (:method ((module module))
   (merge-pathnames (make-pathname :directory
                                   (when (and (slot-boundp module 'directory) (member (directory-of module) *special-directory-keywords*))
                                     (list :relative (directory-of module)))
                     :name :wild :type (suffix-of module))
                    (component-pathname module))))

(defun mismatchedp (filelist component-list)
  (or (set-difference filelist component-list :key 'name-of :test 'equalp)
      (set-difference component-list filelist :key 'name-of :test 'equalp)))

(defgeneric create-wildcard-components (module file-list)
  (:method ((module module) file-list)
   (loop :for file :in file-list
         :collect (create-component module (name-of file)
                                    (default-component-class-of module)
                                    `((:pathname ,file))))))

(defgeneric merge-differences (module on-disk-files cached-components)
  (:method  ((module module) on-disk-files cached-components)
   (flet ((to-keep ()
            (loop for cached in cached-components
                  if (find (name-of cached) on-disk-files :key 'name-of :test 'equalp)
                  collect cached)))
     (let ((new-files (set-difference on-disk-files cached-components :key 'name-of :test 'equalp)))
       (append (to-keep)
               (create-wildcard-components module new-files))))))

(defmethod components-of ((module wildcard-module))
  (let* ((on-disk-files (directory (wildcard-pathname-of module))))
    (setf (cached-components-of module)
          (if (mismatchedp on-disk-files (cached-components-of module))
              (merge-differences module on-disk-files (cached-components-of module))
              (cached-components-of module)))))

(defmethod components-of :around ((module wildcard-module))
  (let ((values (call-next-method)))
    (if (ordered-of module)
        (sort (copy-list values) (ordered-of module) :key 'name-of)
        values)))


;; CONDUIT-SYSTEMS
(defclass conduit-system (system)
  ()
  (:documentation "CONDUIT-SYSTEMS are systems which are automatically loaded once the systems they
depend on are loaded. These are heavily influenced by asdf-system-connections."))

(defun all-dependencies-loaded? (conduit)
  (loop :for (- . system) :in (component-dependencies conduit (load-time-value (make-instance 'load-action)) )
        :always (system-loaded-p system)))
             
(defmethod maybe-load-conduit-systems ((system system))
  (mapc #'(lambda (conduit)
            (let ((loaded? (all-dependencies-loaded? conduit)))
              (when loaded?
                (execute conduit 'load-action))))
        (conduit-systems-of system)))

(defmethod conduit-systems-of ((system system))
  (systems-matching #'(lambda (x) (and (typep x 'conduit-system)
                                       (find-if #'(lambda (dependency)
                                                    (eql (component-of dependency) (name-of system)))
                                                (needs-of x))))))


(defmacro define-conduit-system (name (&rest supers) &body options)
  `(define-system ,name (,@supers conduit-system) ,@options))


;; NAMED-MODULES
;; Named modules provide a module class whose directory name is specified by the :names option
;; provided to it.
;;eg
;; (:components (compat named-module (:names :allego :lispworks (:sbcl "pcl"))))
(defclass named-module (module)
  ((names :initarg :names :initform nil :accessor names-of)))

(full-data-option names-of :names "<strong>:names</strong> <i>mapping*</i> (only for named-module's)
specifies the directory name that the module is to use based on MAPPINGs.
Each mapping is of the for (<i>feature-test</i> &amp;optional<i>directory-name</i>) when only name is specified the
parenthesis can be dropped and name is taken to be the string-downcase of feature-test.                                                
When computing the directory-name of the named module, the directory-name of first mapping which has a feature
test that passes featurep is used.")


(defmethod appropriate-name ((module named-module))
  (flet ((use (thing) (return-from appropriate-name thing)))
    (loop :for name :in (names-of module) :do
          (destructuring-bind (name &optional (dir (string-downcase name))) (mklist name)
            (when (featurep name)
              (use dir))))
    (error "No appropriate directory name for module ~S." module)))

(defmethod module-directory ((module named-module))
  (let ((name (appropriate-name module)))
    (if name
        (list :relative name)
        (error "No appropriate directory found for ~S." module))))

;;; LOADING OF SYSTEM DEFINITION FILES
;;; sydef files are expected to execute define-system forms and are treated as a system
;;; whose components are computed.
(defclass wildcard-sysdef-searcher (wildcard-module)
  ((search-directory :accessor search-directory-of :initarg :search-directory)
   (development-mode :accessor development-mode-of :initarg :development-mode :initform nil))
  (:default-initargs :default-component-class 'sysdef-file))


(defmethod wildcard-pathname-of ((module wildcard-sysdef-searcher))
  (search-directory-of module))

(defmethod create-wildcard-components ((module wildcard-sysdef-searcher) file-list)
  (loop :for file :in file-list
        :collect (create-component module (name-of file)
                                   (default-component-class-of module)
                                   `((:pathname ,file)
                                     (:development-systems ,(development-mode-of module))))))

(defun wildcard-searcher (path &key (development-mode t))
  (create-component *sysdef-system*
                    (format nil "wildcard-search-~A" path)
                    'wildcard-sysdef-searcher
                    `((:search-directory ,path)
                      (:development-mode ,development-mode))))

(defclass sysdef-file (lisp-source-file)
  ((development-systems-p :initarg :development-systems :initform nil :accessor development-systems-p
                          :documentation "Controls whether systems defined by this sydesf file will be created in development mode")))

(defun sysdef-file-fasl-root-dir (file)
  (merge-pathnames (make-pathname :directory (append '(:relative "system-definitions")
                                                     (cdr (pathname-directory (enough-namestring (component-pathname file)
                                                                                                 *sysdef-path*)))))
                   *fasl-output-root*))

(defmethod fasl-path ((file sysdef-file))
  (if (development-systems-p file)
      #P""
      (call-next-method)))

(defmethod output-file ((file sysdef-file))
  (flet ((root-dir ()
           (if *fasl-output-root*
               (sysdef-file-fasl-root-dir file)
               *sysdef-path*)))
    (if (development-systems-p file)
        (compile-file-pathname (input-file file))
        (merge-pathnames (merge-pathnames (root-dir))
                         (call-next-method)))))

(defmethod execute ((file sysdef-file) (action load-action))
  (let ((*default-development-mode* (development-systems-p file)))
    (call-next-method)))

(setf *sysdef-system*
  (define-system :SYSDEF-DEFINITIONS ()
    (:pathname #.*sysdef-path*)
    (:default-component-class sysdef-file)
    (:uses-macros-from :mudballs)
    (:components  ("files" wildcard-module (:directory :wild-inferiors)))))


(defmethod components-of ((sys (eql *sysdef-system*)))
  (append (call-next-method) *custom-search-modules*))

(defvar *registering-sysdefs* nil)


;; I use (if *registering-sysdefs* :ignored) rather than (unless register-sysdefs) to help
;; debugging when using trace.
(defmethod execute around ((sys (eql *sysdef-system*)) (action load-action))
  (if *registering-sysdefs*
      :ignored
      (let ((*registering-sysdefs* t))
        (call-next-method))))

(defun register-sysdefs ()
  "Loads all system definition files that have been registered.
ie. All that are part of the standard mudballs distribution or custom files
loaded by functions on *custom-search-modules*. It is no longer necessary to
call this function by hand as the system definition files will be interrogated
before find-system is called."
  (declare (values))
  (perform *sysdef-system* 'load-action))

;;;;;;;
;;; PATCH SYSTEM
(defvar *system-being-patched* nil)

(defgeneric load-patches (system)
  (:method ((system system))
   (let ((module (or (patch-module-of system)
                     (setf (patch-module-of system)
                           (create-patch-module system))))
         (*system-being-patched* system))
     (execute module 'load-action))))

(defun patch-name (system)
  (intern (format nil "~:@(~A~)-PATCH-SYSTEM" system) :keyword))

(defgeneric create-patch-module (system)
  (:method ((system system))
   (create-component system (patch-name system) 'patch-module)))

(defun integer-string< (a b)
  (< (parse-integer a :junk-allowed t)
     (parse-integer b :junk-allowed t)))

(defclass patch-module (wildcard-module)
  ()
  (:default-initargs :ordered 'integer-string<))

(defmethod component-pathname ((module patch-module))
  (merge-pathnames (make-pathname :directory (list :relative "patches" (string-downcase (name-of (parent-of module)))
                                                   (version-string (version-of (parent-of module)))))
                   *root-pathname*))

(defmethod output-file ((module patch-module))
  (merge-pathnames (fasl-path module) (component-pathname module)))

(defmacro patch (version &key)
  `(setf (patch-version-of *system-being-patched*)
         ',version))


;; CONFIG FILES
(defmethod process-option ((component component) (option (eql :config-file)) &rest option-data)
  (let ((path (first option-data)))
    ;; clear out the config component if path is not the same as the pathname of the component)
    (when-let (component (config-component-of component))
      (unless (equal (translate-portable-path path) (input-file component))
        (setf (config-component-of component) nil)))
    (call-next-method)))
               
(defclass config-file (lisp-source-file) ())
(defmethod ensure-output-path-exists ((file config-file))
  t)

(defmethod config-file-of ((sys system))
  (when-let (value (slot-value sys 'config))
    (translate-portable-path value)))

(defun config-file-exists-p (sys)
  (and (config-file-of sys) (probe-file (config-file-of sys))))

(defgeneric load-config (system)
  (:documentation "Loads the configuration file for SYSTEM.")
  (:method :around ((sys system))
   (when (and *load-config* (config-file-exists-p sys))
     (call-next-method)))
  (:method ((sys system))
   "The default method ensures that a configuration component is present by invoking create-config-component,
saves the component on the system and loads it using execute and load-source-action"
   (let ((component (orf (config-component-of sys)
                         (create-config-component sys))))
     (execute component 'load-source-action))))

(defgeneric create-config-component (system)
  (:documentation "Creates config component for system.")
  (:method ((system system))
   "The default method creates a config component using the default-config-class (see options in define-system)
of the system and specifies the pathname of the component as the pathname specified by the :config-file option."
   (when-let (path (config-file-of system))
     (create-component system (pathname-name path)
                       (default-config-class-of system)
                       `((:pathname ,path))))))


(defun translate-portable-path (path)
  "Converts pathnames of the form ;dir-name;name to a proper pathname.
This will also expand ~'s to (user-homedir-pathname)"
  (when (or (pathnamep path)
            (and (not (find #\; path))
                 (absolute-pathname-p path)))
    (return-from translate-portable-path (merge-pathnames path)))
  (let* ((elements (remove "" (split path :ws '(#\;)) :test 'string=))
         (directories (butlast elements))
         (filename (car (last elements))))
    (merge-pathnames (make-pathname :directory (adjoin :absolute (replace-~ directories))
                                    :defaults (pathname filename)))))

(defun replace-~ (list)
  (if (string= (car list) "~")
      (append (pathname-directory (user-homedir-pathname)) (rest list))
      list))


;; PREFERENCE FILES
;; Preferences files are files which contain symbol => value mappings which are made available
;; at system load time.
(defclass preference-file (file) ())

(defmethod preference-file-of :around ((system system))
  (when-let (slot (slot-value system 'preference-file))
    (translate-portable-path slot)))

(defun preference-file-exists-p (sys)
  (and (preference-file-of sys) (probe-file (preference-file-of sys))))

(defun contents-of (stream)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (loop :for form = (read stream nil stream)
            :until (eq form stream)
            :collect form))))

(defgeneric preferences-of (system)
  (:method ((system system))
   (when (and *load-preferences* (preference-file-exists-p system))
     (with-open-file (input (preference-file-of system))
       (contents-of input)))))

(defun preference (name)
  "Returns the preference named NAME. This function only returns a meaningful value when
called in the context of a system load where the system has as :preference option set (see define-system).

Preference files are of the form
(KEYWORD VALUE)*

Example
A file (\"/tmp/config\") containing (:fasl-output-path \"/tmp/\")

A system (define-system :test () (:components \"test-file\") (:preferences \"/tmp/config\"))

and in test file a form (defvar *output-path* (or (sysdef:preference :fasl-output-path) \"/tmp/\"))


This will set the *output-path* variable to \"/tmp/\"
"
  (declare (symbol name))
  (cadr (assoc name *bound-preferences* :test 'string-equal)))

(defmethod execute around ((system system) (action source-file-action))
  (let ((*bound-preferences* (preferences-of system)))
    (call-next-method)))


;;; SYSTEM PROVIDERS AND DEFINITION FILES
(defvar *extracting-url* nil)
(defun definition-file-provider-url (file)
  (catch 'url
    (let ((*extracting-url* t))
      (load file))))


(defun initial-providers ()
  (make-hash-table :test #'equalp))

(defvar *registered-providers* (initial-providers))

(defun definition-file-provider (file)
  (gethash (definition-file-provider-url file)
           *registered-providers*))

    
(defclass provider ()
  ((url :initarg :url :reader url-of :initform nil)
   (contact :initarg :contact :reader contact-of :initform nil))
  (:documentation "The PROVIDER class abstracts the location
from which systems may be downloaded from. Systems with a bound provider slot
\(accessible using provider-of) typically indicates that a system has been
defined in the context of a with-provider macro and may be installed automatically."))

(defun register-provider (url &key contact)
  (orf (gethash url *registered-providers*)
       (make-instance 'provider :url url :contact contact)))

(defmacro with-provider ((&key url contact) &body body)
  "Executes BODY in a dynamic context where all systems defined,
typically using define-system, will have a provider with a url of URL."
  `(if *extracting-url*
       (throw 'url ,url)
       (let ((*default-provider* (register-provider ,url :contact ,contact)))
         ,@body)))



;;;; Extra operations.
;;; Here we predefine various files and system types which can be used to
;;; compose systems. We only provide the classes here and expect another system
;;; to define useful methods on them this is to keep startup times to a minimum
;;; (eg. sysdef-grovel requires an extra 3 systems and it isn't reasonable to
;;;   have them all loaded in order to define a system, which we do at startup)



(defclass grovel-file (lisp-source-file)
  ()
  (:default-initargs :type "cffi.lisp"))


;; A User package.
(defpackage :sysdef-user (:use :cl :sysdef)
  (:documentation "The :SYSDEF-USER package is provided as a workspace area to define your own
packages in, all mudballs definition files are expected to contain  (in-package :sysdef-user)
at the top of the file."))



;;; BOOTSTRAP
;; And now we create ourself as a system
(define-system :mudballs ()
  (:author "Sean Ross")
  (:supports (:implementation :lispworks :sbcl :cmucl :clisp :openmcl :scl :allegrocl))
  (:contact "sross@common-lisp.net")
  (:version 0 3 9)
  (:pathname #.(directory-namestring (or *compile-file-truename* "")))
  (:config-file #.(merge-pathnames ".mudballs" (user-homedir-pathname)))
  (:preferences #.(merge-pathnames ".mudballs.prefs" (user-homedir-pathname)))
  (:components "sysdef" "mudballs"))

;; we define our boot system to allow us to do a system-update with minimal fuss
;; It's not meant to be executed upon or to be available for public consumption.
(define-system :boot ()
  (:components "boot")
  (:provider t) ;; only boot is allowed to use T here!
  (:pathname #.*root-pathname*))

;; and register ourselves as loaded
(let ((first-file (find-component :mudballs "sysdef")))
  (setf (time-of first-file (make-instance 'load-action))
        (get-universal-time)))

(perform :mudballs 'load-action)

(setf *mudballs-loaded* t)
(pushnew :mudballs *features*)

;; EOF
