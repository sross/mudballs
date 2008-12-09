(:special-var *compile-fails-behaviour*
 (:see-also (*COMPILE-WARNS-BEHAVIOUR*)))

(:special-var *COMPILE-WARNS-BEHAVIOUR*
 (:see-also (*compile-fails-behaviour*)))

(:special-var *custom-search-modules*
 (:see-also (wildcard-searcher)))

(:function run-shell-command
 )

(:system :mb.sysdef
 (:notes "These are the <strong>notes</strong>"))

(:macro with-provider
 (:see-also (provider-of provider)))


(:type system-name
 (:notes "Defining systems using the (system module new-module) syntax is intended to be<br/>
used to define lazy subsystems. This is done by using this syntax and specifying the<br/>
type of the new module to be a lazy-module. Systems defined in this manner can<br/>
then be used to define modules which are only loaded when needed (eg. clsql's various<br/>
database backends) without cluttering up the global system namespace."))


(:macro define-system
 (:exceptional-situations "If multiple components with the same name are specified in a components
list an error of type duplicate-component will be signalled.")
 (:examples
  "<pre>
(define-system :test ()
  (:version 0 0 1)
  (:components \"foo\")))

(define-system :my-system (serial-system)
  (:version 1 2)
  (:components (\"packages\"
                (boot MODULE
                  (:directory (\"system\" \"boot\"))
                  (:serial t)
                  (:components \"first\" \"second\")
                  (:needs \"packages\")))))

</pre>

")
 )
