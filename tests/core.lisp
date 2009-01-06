(in-package :mb.sysdef)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mb:load :tryil)
  (use-package :tryil))

;; Allows rebinding of function in a dynamic extent.
(defmacro dflet ((&rest function-bindings) &body body)
  "Executes body in the dynamic extent of function-bindings. This is NOT thread safe and redefines the functions in question.
Bindings are the same as in flet/labels"
  (let* ((function-definitions (create-function-definition-forms function-bindings))
         (names (get-function-names function-bindings))
         (saved-vars (saved-names-for names)))
    `(let ,(mapcar #'(lambda (saved fn-name) `(,saved #',fn-name)) saved-vars names)
       ,@function-definitions
       (unwind-protect (progn ,@body)
         ,@(mapcar #'(lambda (saved fn-name)
                       `(setf (fdefinition ',fn-name) ,saved))
                   saved-vars names)))))

(defun create-function-definition-forms (fn-bindings)
  (loop :for (name arglist . body) :in fn-bindings :collect `(setf (fdefinition ',name)
                                                                   (lambda ,arglist ,@body))))

(defun get-function-names (fn-bindings)
  (mapcar 'first fn-bindings))

(defun saved-names-for (fn-names)
  (mapcar (lambda (name) (make-symbol (format nil "SAVED-FN-~A" name)) ) fn-names))


;;; Utils Macros and Functions
(defmacro define-test-system (name super &body body)
  `(progn (push ,name *builtin-systems*)
     (define-system ,name ,(or super '(testing-system)) ,@body)))

(defclass testing-system (system) () (:default-initargs :default-component-class 'testing-file))
(defmethod component-exists-p ((system testing-system))
  t)

(defclass uninstalled-testing-system (testing-system) ())
(defmethod component-exists-p ((system uninstalled-testing-system))
  nil)

(defclass testing-file (lisp-source-file)
  ((compile-time :accessor last-compile-time :initform 0))
  (:documentation "A Component which does nothing when an action is applied to it."))



(defmethod ensure-output-path-exists ((file testing-file))
  t)

(defmethod component-output-exists-p ((file testing-file))
  t)

(defmethod input-write-date ((file testing-file))
  (get-universal-time))

(defmethod output-write-date ((file testing-file))
  0)

(defclass static-test-file (static-file) ())
(defmethod component-exists-p ((file static-test-file))
  t)

;; Since we don't have an actual output fle for testing files we need to track our compile time manually
;; This acts a a fake file-write-date
(defmethod execute :around ((component testing-file) (action compile-action))
  (multiple-value-prog1 (call-next-method)
    (setf (last-compile-time component) (get-universal-time))))


(defmethod execute ((component testing-file) (action source-file-action))
  nil)

(defmethod component-exists-p ((component testing-file))
  t)

(defclass error-file (testing-file)  ()
  (:documentation "A component which throws an error when it is loaded."))

(define-condition deliberate-error (error) ())
(defmethod execute ((component error-file) (action source-file-action))
  (error 'deliberate-error))

(defmacro with-slot-assertions ((&rest slot-names) &body body)
  "Runs body in an environment which as the macro
\(assert-slot-values list-of-values form) present.
LIST-OF-VALUES, which is evaluated, is tested using equal against
a list created by extracting SLOT-NAMES from form."
  `(macrolet ((assert-slot-values (expected form)
                `(assert-equal ,expected (extract-slots ,form))))
     (flet ((extract-slots (instance)
              (mapcar #'(lambda (slot) (slot-value instance slot))
                      ',slot-names)))
       ,@body)))

(defmacro capture-condition (condition &body body)
  `(handler-case (progn ,@body)
     (,condition (c) c)))



(defmacro with-test-systems ((&rest creators) &body body)
  `(let* ((*systems* ,(if creators `(mapcan 'funcall ',creators) `(default-test-systems)))
          (*builtin-systems* (remove-duplicates (mapcar 'name-of *systems*)))
          (*loaded-versions* (make-hash-table)))
     ,@body))

(defun deprecated-systems ()
  (list (create-component nil :test-deprecated 'testing-system
                          '((:deprecated :replacement-system)))
        (create-component nil :test-deprecated2 'testing-system '((:deprecated t)))))

(defun default-test-systems ()
  (let ((sys1 (create-component nil :test 'testing-system '((:documentation "Test System"))))
        (sys2 (create-component nil :test2 'testing-system))
        )
    ;;sys1
    (add-component-to (create-component sys1 "foo" 'testing-file) sys1)
    (add-component-to (create-component sys1 :module1 'module) sys1)
    ;;sys2
    (add-component-to (create-component sys2 "bar" 'testing-file) sys2)

    (list sys1 sys2 )))

(defun extra-test-systems ()
  (let ((sys (create-component nil :test 'testing-system '((:version 0 8 0))))
        (sys2 (create-component nil :test 'testing-system '((:version 0 9 0))))
        (dep (create-component nil :dep 'testing-system '((:needs (:test :version (0 8 0)))))))
    (add-component-to (create-component sys "foo" 'testing-file) sys)
    (add-component-to (create-component sys :module1 'module) sys)

    (add-component-to (create-component sys2 "foo" 'testing-file) sys2)
    (add-component-to (create-component sys2 :module1 'module) sys2)
    (list sys sys2 dep)))

(defun dependent-test-systems ()
  (let ((sys (create-component nil :needs-test 'testing-system '((:needs :test))))
        (sys2 (create-component nil :needs-test2 'testing-system '((:uses-macros-from :test)))))

    (add-component-to (create-component sys "foo" 'testing-file) sys)
    (add-component-to (create-component sys2 "foo" 'testing-file) sys2)

    (list sys sys2)))


;;; Start of Actual tests.

(define-test deprecated
  (with-test-systems (deprecated-systems)
    (let ((system (find-component :test-deprecated)))
      (assert-error 'deprecated-system (perform system 'load-action))
      (assert-prints "System :TEST-DEPRECATED 0.0.1 is deprecated, Please use REPLACEMENT-SYSTEM instead."
                     (princ (capture-condition deprecated-system (perform system 'load-action)))))
    (let ((system (find-component :test-deprecated2)))
      (assert-error 'deprecated-system (perform system 'load-action))
      (assert-prints "System :TEST-DEPRECATED2 0.0.1 is deprecated."
                     (princ (capture-condition deprecated-system (perform system 'load-action)))))))


(define-test get-from-options
  (let ((options '((:version 0 8) (:name "foo"))))
    (assert-equal '(0 8) (get-from-options options :version))
    (assert-equal '("foo") (get-from-options options :name))
    (assert-equal :not-present (get-from-options options :not-present :not-present))))

(define-test subsystem-definition
  (assert-true (subsystem-definition-p '(foo bar)))
  (assert-true (subsystem-definition-p '(foo)))
  (assert-false (subsystem-definition-p 'foo)))



(define-test find-system-for
  (with-test-systems ()
    (assert-eql (find-component :test :module1)
                (find-system-for '(:test :module1 "bar") (first-version)))
    (assert-error 'type-error (find-system-for '(:test "foo" "bar") nil))
    (assert-error 'no-such-component (find-system-for '(:test3 "foo") nil))
    (assert-error 'no-such-component (find-system-for '(:test "foo") '(0 0 2)))))


(define-test define-system
  (assert-expands '(fn-define-system ':foo 'system nil '((:version 0 8)))
                  (define-system  :foo () (:version 0 8))))


;; some basic system definition tests
(define-test fn-define-system
  (with-test-systems ()
    (fn-define-system :new 'testing-system nil nil)
    (assert= 3 (length *systems*))
    (assert-error 'no-such-component (fn-define-system '(:new2 "foo") 'testing-system (find-component :test) nil))
    (fn-define-system '(:new "foo") 'testing-system (find-component :test) nil)
    (assert= 3 (length *systems*))
    (assert= 1 (length (components-of (find-system :new))))))


(define-test versioned-system-name-p
  (assert-true (versioned-system-name-p '(:foo :version (0 2))))
  (assert-false (versioned-system-name-p '(:foo :version)))
  (assert-false (versioned-system-name-p 'fo))
  (assert-false (versioned-system-name-p nil)))


(define-test make-dependency-spec
  (with-slot-assertions (component match-action consequent-action)
    (assert-slot-values '((:foo :version (0 2)) action nil)
                        (make-dependency-spec '(:foo :version (0 2))))
    (assert-slot-values '(:foo action nil) (make-dependency-spec :foo))
    (assert-slot-values '(:foo action nil) (make-dependency-spec '(:foo)))
    (assert-slot-values '(:foo load-action nil) (make-dependency-spec '(load-action :foo )))
    (assert-slot-values '(:foo load-action compile-action)
                        (make-dependency-spec '(load-action :foo compile-action)))
    (assert-error 'error (make-dependency-spec '(load-action :foo compile-action fail)))))

(define-test find-component-versioned
  (with-test-systems (default-test-systems extra-test-systems)
    (with-slot-assertions (name version)
      (assert-slot-values '(:test (0 8 0)) (find-component '(:test :version (0 8 0))))
      (assert-slot-values '(:test (0 9 0)) (find-component :test))
      (assert-true (find-component :test '("foo" :version (0 8))))
      (assert-error 'no-such-component (find-component '(:test  :version (0 9 1)))))))

(define-test versioned-dependency
  (with-test-systems (default-test-systems extra-test-systems)
    (assert-true (perform (find-component :dep) 'load-action))
    (assert-equal (find-system :test)
                  (system-loaded-p :test))))

(define-test lazy-module
  (with-test-systems ()
    ;;add lazy module
    (let* ((sys (find-system :test))
           (module (create-component sys 'lazy 'lazy-module))
           (file (create-component module 'file 'error-file)))
      (add-component-to file module)
      (add-component-to module sys))
    ;; loading test1 should not fail
    (assert-true (perform :test 'load-action))
    ;; but loading the component itself should
    (assert-error 'deliberate-error  (perform (find-component :test 'lazy)
                                              'load-action))))


(define-test load-needs-compile
  (with-test-systems ()
    (let ((sys (define-test-system :load-needs-compile ()
                 (:components ("module1" module) "file1"))))
      ;; ensure that loading file1 requires file1 to be compiled.
      (destructuring-bind ((action . file))
          (dependencies-of 'load-action (find-component sys "file1"))
        (assert-eq file (find-component sys "file1"))
        (assert-true (typep action 'compile-action)))

      ;; but this does not apply to systems.
      (assert-true (endp (dependencies-of 'load-action sys)))
      ;; or modules
      (assert-true (endp (dependencies-of 'load-action (find-component sys "module1")))))))

(define-test on-macro-use-list
  (with-test-systems (default-test-systems dependent-test-systems)

    (assert-false (on-macro-use-list (find-system :needs-test) (find-system :test)))
    (assert-true (on-macro-use-list (find-system :needs-test2) (find-system :test)))

    ;; ensure that :uses-macros-from adds a dependency
    (destructuring-bind ((action . system))
            (component-dependencies (find-system :needs-test2) (make-instance 'load-action))
      (assert-true (typep action 'load-action))
      (assert-eql system (find-system :test)))

    ;; load needs-test & needs-test2
    (perform :needs-test 'load-action)
    (perform :needs-test2 'load-action)

    ;; set compilation times for :test to after the compile time for (:needs-test2 :foo)
    (setf (last-compile-time (find-component :test "foo"))
          (+ 10 (time-of (find-component :needs-test2 "foo") (make-instance 'load-action))))

    (out-of-date-p (find-component :needs-test2 "foo") (make-instance 'compile-action))

    (assert-true (parent-deps-out-of-date-p (make-instance 'compile-action)
                                            (find-component :needs-test2 "foo")))
    (assert-true  (out-of-date-p (find-component :needs-test2 "foo")
                                 (make-instance 'compile-action)))
    (assert-false (parent-deps-out-of-date-p (make-instance 'compile-action)
                                             (find-component :needs-test "foo")))))



(define-test multiple-versions-tests
  (assert-equal '((define-system :foo (system) (:version 0 8 1)) (define-system :foo (system) (:version 0 8 0)))
                (expand-multiple-versions :foo 'system '((:versions (0 8 1) (0 8 0)))))
  (assert-error 'error (capture-condition error (expand-multiple-versions :foo 'system ())))

  (with-test-systems ()
    (assert-false (find-system :multiple :errorp nil))
    (define-system :multiple ()
      (:versions (0 0 2) (0 0 3)))
    (assert= 2 (length (systems-for :multiple)))
    (assert-equal '((0 0 3) (0 0 2)) (mapcar 'version-of (systems-for :multiple)))
    (define-test-system :multiple ()
      (:version 0 0 2 5))
    (assert-equal '((0 0 3) (0 0 2 5) (0 0 2)) (mapcar 'version-of (systems-for :multiple)))))



(define-test static-file-test
  (with-test-systems ()
    (define-test-system :static-test ()
      (:components ("foo" static-test-file) ("bar" testing-file)))
    (assert-true (perform :static-test 'load-action))))


(define-test development-mode-test
  (let ((*load-truename* #P"/tmp/"))
    (with-test-systems ()
      (define-system :test-dev-mode ()
        (:development t))
      (assert-true (development-mode (find-component :test-dev-mode)))
      (assert-equal #P"/tmp/" (pathname-of (find-component :test-dev-mode)))

      (let ((*default-development-mode* t))
        (define-system :test-dev-mode2 ()())
        (assert-true (development-mode (find-component :test-dev-mode2)))
        (assert-equal #P"/tmp/" (pathname-of (find-component :test-dev-mode2)))))))


(define-test custom-sysdefs
  (let* ((*custom-search-modules* ())
         (system (find-system :sysdef-definitions))
         (current-size (length (components-of system)))
         (*custom-search-modules* (list (create-component system "sydef" 'testing-file))))
    (assert= (+ 1 current-size) (length (components-of system)))))


;; If this test is failing make that the number of files it expects to find in this directory is correct
(define-test wildcard-sysdef-searcher
  (flet ((test-once (development-mode)
           (let ((component (wildcard-searcher (merge-pathnames (make-pathname :name :wild :type "lisp" :directory '(:relative "tests"))
                                                                (component-pathname *tested-system*))
                                               :development-mode development-mode))
                 (expected-size 1))
             (assert-eql 'sysdef-file (default-component-class-of component))
             (assert-eql development-mode (development-mode-of component))
             (assert-true (search-directory-of component))
             (assert= expected-size (length (directory (search-directory-of component))))
             (assert-equal (wildcard-pathname-of component) (search-directory-of component))

             (let ((components (create-wildcard-components component (directory (wildcard-pathname-of component)))))
               (mapcar #'(lambda (comp)
                           (assert-equal nil
                                         (output-pathname-of comp))
                           (assert-eql (development-mode-of component) (development-systems-p comp)))
                       components)))))
    (test-once nil)
    (test-once t)))


;; we add this function because (compile-file-pathname #P"foo.lisp") fails in CLISP.
(defun fasl-path-type ()
  (pathname-type (compile-file-pathname (make-pathname :name "foo" :type "lisp" :directory '(:absolute)))))

(define-test output-pathname-test
  (with-test-systems ()
    (let ((sys (define-test-system :output-test ()
                 (:components "foo" ("bar" (:output-pathname "/baz/bar.fas")))
                 (:output-pathname #P"/tmp/"))))
      (assert-equal (merge-pathnames (fasl-path (find-component sys "foo"))
                                     (compile-file-pathname #P"/tmp/foo.lisp"))
                    (output-file (find-component sys "foo")))
      (assert-equal "/baz/bar.fas" (output-file (find-component sys "bar"))))

    (let ((sys (define-test-system :output-test2 ()
                 (:components "foo" ("bar" (:output-pathname "/baz/bar.fas")))
                 (:pathname #P"/tmp/"))))
      (assert-equal (merge-pathnames (fasl-path (find-component sys "foo"))
                                     (compile-file-pathname #P"/tmp/foo.lisp"))
                    (output-file (find-component sys "foo")))
      (assert-equal "/baz/bar.fas" (output-file (find-component sys "bar"))))

    ;; test with *fasl-output-root*
    (let ((*fasl-output-root* #P"/tmp/"))
      (let ((sys (define-test-system :for-test-system ()
                   (:components "foo" ("bar" (:output-pathname "/baz/bar.fas"))))))

        (assert-equal (merge-pathnames (fasl-path (find-component sys "foo"))
                                       (make-pathname :version :newest :name "foo" :type (fasl-path-type)
                                                      :directory '(:absolute "tmp" "for-test-system" "0.0.1")))
                      (output-file (find-component sys "foo")))
        (assert-equal "/baz/bar.fas" (output-file (find-component sys "bar")))))))



;(find-system :installer)
(defclass ordering-file (testing-file)
  ((completed-actions :accessor completed-actions-of :initform ())))

(defmethod out-of-date-p ((file ordering-file) (action action))
  (not (find (class-name (class-of action)) (completed-actions-of file))))

(defvar *loaded* ())
(defmethod execute ((file ordering-file) (action action))
  (push (class-name (class-of action)) (completed-actions-of file)))

(defmethod execute ((file ordering-file) (action load-action))
  (call-next-method)
  (push (name-of file) *loaded*))

(define-test serial
  (with-test-systems ()
    (let ((*loaded* ())
          (sys (define-test-system :serial-system ()
                 (:default-component-class ordering-file)
                 (:serial t)
                 (:components "foo" "bar"))))
      (perform sys 'load-action)
      (assert-true (serialp sys))
      (assert-equal '(ordering-file ordering-file)
                    (mapcar (lambda (c) (class-name (class-of c))) (components-of sys)))
      (assert-equal '("bar" "foo") *loaded*))))


;; :supports
(define-test supports
  (dflet ((os () :mac)
          (implementation () :sbcl)
          (platform () :x86))
    (assert-true (check-supported-p :implementation :sbcl))
    (assert-false (check-supported-p :implementation :clisp))
    (assert-true (check-supported-p :os :mac))
    (assert-false (check-supported-p :os :mswindows))
    (assert-true (check-supported-p :platform :x86))
    (assert-false (check-supported-p :platform :darwin))
    (assert-true (check-supported-p :and '(:implementation :sbcl) ))
    (assert-true (check-supported-p :and '(:implementation :sbcl) '(:os :mac) '(:platform :x86)))
    (assert-false (check-supported-p :and '(:implementation :sbcl) '(:os :mswindows)))
    (assert-true (check-supported-p :or '(:implementation :sbcl) '(:os :mswindows)))
    (assert-false (check-supported-p :or))
    (assert-true (check-supported-p :and))
    (assert-true (check-supported-p :not '(:os :mswindows)))
    (assert-false (check-supported-p :not '(:os :mac)))
    (let ((*features* '(:sbcl :x86 :mswindows)))
      (assert-true (check-supported-p :feature :sbcl))
      (assert-true (check-supported-p :feature '(:not :lispworks)))
      (assert-false (check-supported-p :feature '(:not :sbcl)))
      (assert-true (check-supported-p :feature '(:and :sbcl :x86)))
      (assert-true (check-supported-p :feature '(:or :mswindows :macosx) :sbcl))
      (assert-true (check-supported-p :feature '(:or :lispworks (:and :sbcl :mswindows (:not :ppc))))))))


(defmacro with-gensyms ((&rest vars) &body body)
  `(let (,@(loop for var in vars collect `(,var (gensym))))
     ,@body))

(defmacro has-restart (condition name &body body)
  "Returns true if body signals CONDITION and has a restart named NAME (evaluated)."
  (with-gensyms (gblock gvar)
    `(block ,gblock
       (handler-bind ((,condition (lambda (,gvar)
                                    (when (find-restart ,name ,gvar)
                                      (return-from ,gblock t)))))
         (progn ,@body
           nil)))))
                                  

(defun always-nil (&rest args)
  (declare (ignore args))
  nil)

(define-test supportedp
  (dflet ((os () :mac)
          (implementation () :sbcl)
          (platform () :x86))
    (assert-error 'component-not-supported (supportedp (create-component nil :test 'testing-system
                                                                         '((:supports (:os :mswindows))))))
    (assert-true (has-restart component-not-supported 'continue (supportedp (create-component nil :test 'testing-system
                                                                                              '((:supports (:os :mswindows)))))))
    (assert-true (null (supportedp (create-component nil :test 'testing-system
                                                     '((:supports (:os :mswindows))
                                                       (:if-supports-fails always-nil))))))
    (assert-true (supportedp  (create-component nil :test 'testing-system)))
    (assert-error 'simple-error (supportedp (create-component nil :test 'testing-system
                                                              '((:supports (:os :mswindows))
                                                                (:if-supports-fails nil)))))))

(defclass ordered-test-module (wildcard-module)
  ()
  (:default-initargs :ordered 'integer-string<))

(defmethod components-of ((module ordered-test-module))
  (mapcar #'(lambda (name)
              (create-component module name 'testing-file))
          '("002" "003" "001" "004")))


(define-test ordered-test ()
  (let ((module (make-instance 'ordered-test-module)))
    (loop for (a b . rest) on (components-of module)
          :do (when (and a b)
                (assert-true (integer-string< (name-of a) (name-of b)))))))

(defclass ordered-patch-test (patch-module)
  ())

(defmethod components-of ((module ordered-patch-test))
  (mapcar #'(lambda (name)
              (create-component module name 'testing-file))
          '("002" "003" "001" "004")))

(define-test patch-tests
             (let* ((sys (create-component nil 'test 'system '((:version 0 1 1))))
         (*system-being-patched* sys))
    (patch (0 1 2))
    (assert-equal '(0 1 1) (version-of sys))
    (assert-equal '(0 1 2) (patch-version-of sys))
    (assert-eql :mb.sysdef-patch-system (patch-name :mb.sysdef))
    (assert-equal (namestring (make-pathname :directory '(:relative "patches" "test" "0.1.1")))
                  (enough-namestring (component-pathname (create-patch-module sys))
                                     *root-pathname*)))
  (let ((module (make-instance 'ordered-patch-test)))
    (loop for (a b . rest) on (components-of module)
          :do (when (and a b)
                (assert-true (integer-string< (name-of a) (name-of b)))))))

(define-condition test-condition (error) ())
(defun signal-test-condition (comp)
  (declare (ignore comp))
  (error 'test-condition))

(define-test not-supported-tests
  (with-test-systems ()
    (dflet ((os () :mswindows))
      (let ((sys1 (define-test-system :test-a () (:supports (:os :linux))))
            (sys2 (define-test-system :test-b () (:supports (:os :linux)) (:if-supports-fails nil)))
            (sys3 (define-test-system :test-c () (:supports (:os :linux)) (:if-supports-fails signal-test-condition))))
        (assert-error 'component-not-supported (perform sys1 'load-action))
        (assert-error 'error (assert-false (perform sys2 'load-action)))
        (block nil
          (handler-bind ((component-not-supported (lambda (c)
                                                    (assert-true (find-restart 'continue c))
                                                    (return nil))))
            (perform sys1 'load-action)))
        (assert-error 'test-condition (perform sys3 'load-action))))))


(defmacro test-function-calls ((function test) &body body)
  (let ((assertion-name (intern (format nil "ASSERT-~A" test) :tryil)))
    `(progn ,@(loop for (arg result) in body
                    collect `(,assertion-name ',result (,function ',arg ))))))

(define-test coerce-to-version
  (test-function-calls (coerce-to-version equal)
    ("0.8" (0 8))
    ("0.*" (and (>= 0) (< 1)))
    (1 (1))
    ((1 3 *) (and (>= 1 3) (< 1 4)))
    ((and "0.15.*" (not "0.15.5") (>= 0 15 4)) (AND "0.15.*" (NOT "0.15.5") (>= 0 15 4)))
    ((1 3) (1 3)))
  (assert-error 'error (coerce-to-version "not-a-version"))
  (assert-error 'error (coerce-to-version 'not-a-version)))

(defclass test-config-file (config-file) ())

(defmethod component-exists-p ((component test-config-file))
  t)

(defmethod execute ((file test-config-file) (action load-source-action))
  (princ "exec"))

(defmethod input-write-date ((file test-config-file))
  (get-universal-time))

(define-test config
  (with-test-systems ()
    (dflet ((config-file-exists-p (x) (declare (ignore x)) t))
      (let ((sys (define-test-system :pref-sys ()
                   (:default-config-class test-config-file)
                   (:config-file "core.lisp"))))
        (assert-prints "exec" (load-config sys))
        (assert-prints ""  (load-config sys)))
      (let ((sys (define-test-system :pref-sys2 ()
                   (:default-config-class test-config-file)
                   (:config-file "core.lisp")))
            (*load-config* nil))
        (assert-prints "" (load-config sys))
        (assert-prints "" (load-config sys))))))

(define-test contents-of
  (with-test-systems ()
    (let ((fake-file "(:featuritis t)"))
      (with-open-stream (stream (make-string-input-stream fake-file))
        (assert-equal '((:featuritis t)) (contents-of stream))))
    (let ((fake-file "(:featuritis #.t)"))
      (with-open-stream (stream (make-string-input-stream fake-file))
        (assert-error 'reader-error (contents-of stream))))))

(defclass preferences-system (testing-system) ())

(defmethod preferences-of ((system preferences-system))
  '((:test-pref "my-preferences")))

(defmethod execute ((sys preferences-system) (action action))
  (throw 'pref (preference :test-pref)))

(define-test preferences
  (assert-true (subtypep 'preference-file 'file))
  (let ((*bound-preferences* '((:test-pref "my-preference"))))
    (assert-equal "my-preference" (preference :test-pref)))
  (with-test-systems ()
    (define-test-system :pref-sys (preferences-system) ())
    (assert-equal "my-preferences" (catch 'pref (execute :pref-sys 'load-action)))))
      
                      
(define-test duplicate-component
  (with-test-systems ()
    (assert-error 'duplicate-component (define-system :test-dups () (:components "foo" "foo")))
    (assert-error 'duplicate-component (define-system :test-dups () (:components "foo" "Foo")))))

(define-test clean-action-test
  (with-test-systems (default-test-systems dependent-test-systems)
    (assert-true (endp (dependencies-of (make-instance 'clean-action) (find-system :needs-test))))
    (assert-false (endp (dependencies-of (make-instance 'load-action) (find-system :needs-test))))))

(define-test redefine-system-test
  (with-test-systems ()
    (define-test-system :foo () (:components "foo"))
    (assert= 1 (length (components-of (find-system :foo))))
    (handler-bind ((system-redefined #'muffle-warning)) 
      (define-test-system :foo () (:components "foo" "bar")))
    (assert= 2 (length (components-of (find-system :foo))))))

(define-test name-test
  (with-test-systems ()
    (assert-eql (find-system :test) (find-system "test"))
    (assert-eql  (find-system "test") (find-system "TEST"))
    (assert-error 'error (eval '(define-test-system "Foo" () ())))
    (assert-true (typep "foo" 'system-designator))
    (assert-true (typep 'foo 'system-designator))
    (assert-true (typep :foo 'system-designator))
    (assert-false (typep '(:foo :bar) 'system-designator))
    (assert-true (typep 'foo 'system-name))
    (assert-true (typep :foo 'system-name))
    (assert-false (typep "foo" 'system-name))))

;; Clisp fails the doc tests for (documentation (or string symbol) 'system),
;; there seems to be a method in place which is more specific than the custom one
(define-test doc-test ()
  (with-test-systems ()
    (let ((sys (find-system "test")))
      (assert-equal (documentation sys 'system)
                    (doc sys))
      #-clisp
      (assert-equal (documentation :test 'system) (doc sys))
      #-clisp
      (assert-equal (documentation "test" 'system) (doc sys)))))


(defclass out-of-date-file (testing-file) ())
(defmethod out-of-date-p ((file out-of-date-file) (action action))
  nil)

(define-test force-test 
  (let ((comp (create-component nil "test" 'out-of-date-file)))
    (assert-false (out-of-date-p comp (make-instance 'load-action)))
    (assert-true (out-of-date-p comp (make-instance 'load-action :force t)))))


(define-test multiple-directory-module
  (with-test-systems ()
    (let ((sys (define-test-system :multi-dir-test ()
                 (:components (:foo module
                               (:directory ("dir1" "dir2")))))))
      (assert-equal '(:relative "multi-dir-test") (module-directory sys))
      (assert-equal '(:relative "dir1" "dir2")
                    (module-directory (find-component sys :foo)))
      (assert-equal (namestring (make-pathname :directory '(:relative "dir1" "dir2")))
                    (enough-namestring (component-pathname (find-component sys :foo))
                                       (component-pathname sys))))))

(define-test system-redefined-test
  (with-test-systems ()
    (assert-error 'system-redefined (define-test-system :test () ()))
    (assert-true (handler-case (define-test-system :new-system () ())
                   (condition (c) (declare (ignore c)) nil)
                   (:no-error (val) (declare (ignore val)) t)))))


(define-test custom-method-combination-test
  (with-test-systems ()
    (let ((sys (define-system :around-test () ())))
      (defmethod execute :around ((sys (eql sys)) (action load-action))
        (throw 'lisp-around t))
      (assert-true (catch 'lisp-around (execute sys 'load-action) nil))
      
      (defmethod execute around ((sys (eql sys)) (action load-action))
        (throw 'sysdef-around t))
      (assert-true (catch 'sysdef-around (execute sys 'load-action) nil)))))


(define-test system-traversal-tests ()
  (with-test-systems ()
    (let ((count 0))
      (do-systems (sys) (incf count))
      (assert= 2 count))
    (let ((systems (systems-matching (constantly t))))
      (map-systems (lambda (x) (setf systems (delete x systems))))
      (assert-true (endp systems)))
    (let ((first (first (systems-matching (constantly t)))))
      (undefine-system first)
      (assert= 1 (length (systems-matching (constantly t)))))))

(define-test case-tests ()
  (with-test-systems ()
    (let ((sys (define-test-system :case-test ()
                 (:components "Foo" "bar" :baz))))
      (assert-equal '("Foo" "bar" "baz")
                    (mapcar (lambda (x) (pathname-name (component-pathname x)))
                            (all-files sys))))))

(defun lookup-test-systems ()
  (let ((sys1 (create-component nil :lookup-test 'testing-system '((:version 0 0 1))))
        (sys2 (create-component nil :lookup-test 'testing-system '((:version 0 1 0))))
        (sys3 (create-component nil :lookup-test 'testing-system '((:version 1 0 0))))
        (sys4 (create-component nil :lookup-test 'uninstalled-testing-system '((:version 2 0 0)))))
    (list sys1 sys2 sys3 sys4)))

(define-test find-system-tests ()
  (with-test-systems (lookup-test-systems)
    (flet ((lookup (&optional version &key (errorp t))
             (find-system :lookup-test :version version :errorp errorp)))
      (destructuring-bind (min-system mid-system max-system uninstalled-system) *systems*
        (assert-eq max-system (lookup))
        (assert-eq max-system (lookup "1.00"))
        (assert-eq min-system (lookup "0.0.1"))
        (assert-error 'no-such-component (lookup "10"))
        (assert-false (lookup "10" :errorp nil))
        (assert-eq uninstalled-system (lookup "2"))
    
        (perform mid-system 'load-action)
        (assert-eq mid-system (lookup))
        (assert-eq mid-system (lookup '(> 0 0 1)))
        (assert-eq mid-system (lookup '(0 1 0)))
        (assert-eq min-system (lookup "0.0.1"))
        (assert-eq max-system (lookup "1"))
        (assert-eq uninstalled-system (lookup "2"))))))

;(mb:test :mb.sysdef)
                     
(princ (run-tests))
 
