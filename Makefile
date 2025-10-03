.PHONY: tangle-program

tangle-program:
	emacs -Q --batch --eval "(require 'org)" --eval "(find-file \"pws-org-program.org\")" --eval "(org-babel-tangle)"
