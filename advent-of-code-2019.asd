(defsystem "advent-of-code-2019"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("uiop")
  :components ((:module "src"
                :components
                ((:file "day-1"))))
  :description ""
  :in-order-to ((test-op (test-op "advent-of-code-2019/tests"))))

(defsystem "advent-of-code-2019/tests"
  :author ""
  :license ""
  :depends-on ("advent-of-code-2019"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "day-1"))))
  :description "Test system for advent-of-code-2019"
  :perform (test-op (op c) (symbol-call :rove :run c)))
