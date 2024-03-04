files := $(shell find . -name '*.rkt' | grep -v 'flycheck')
.PHONY: test
test:
	raco test --drdr $(files)
