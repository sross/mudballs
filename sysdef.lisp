(in-package :sysdef-user)
 
(define-system :mb.sysdef ()
  (:author "Sean Ross")
  (:supports (:implementation :lispworks :sbcl :cmucl :clisp :allegrocl :abcl :ecl :openmcl))
  (:contact "sross@common-lisp.net")
  (:version 1)
  (:preferences #.(merge-pathnames ".mudballs" (user-homedir-pathname)))
  (:components "mb"))
