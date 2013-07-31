;;;; This contains some of the core Hy macros used
;;;; to make functional programming slightly easier.
;;;;

(defmacro kwapply [call kwargs &rest rest &kwargs morekw] 
  (let [[fun (first call)]
        [args (rest call)]] 
    (quasiquote 
     (apply (unquote fun)
            [(unquote-splice args)]
            (unquote kwargs)))))

;;(def *exports* ["kwapply"])

