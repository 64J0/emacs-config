;;; package: noob.el
;;; commentary: emacs-lisp-mode
(+ 2 2)

(- 2 80)

(+ 2 3)

(set 'a 2)
(setq a 2)

(set 'b 1)
(setq b 1)

(+ a b)

(concat "abc" "ddd")
(substring "The quick brown fox jumped." 16 19) ; fox
(concat "The "
        (number-to-string
         (+ fill-column 10)) " red foxes")
(zerop 0) ; p stands for predicate
(zerop 1) ; nil
(listp ()) ; t
(listp 1) ; nil
(message "This message appears in the echo area!")
(message "The name of this buffer is: %s." (buffer-name))

; BIND A VARIABLE TO A VALUE
(set 'flowers '(rose violet daisy buttercup))
(setq flowers '(rose violet daisy buttercup))

(setq counter 0) ; this is the initializer
(setq counter (+ counter 1)) ; this is the incrementer
counter ; this is the counter value

(setq counter-fn (setq counter (+ counter 1)))
counter-fn
