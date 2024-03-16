files := $(shell find . -name '*.rkt' | grep -v 'flycheck')
.PHONY: test
test:
	raco test --drdr $(files)

.PHONY: fmt
fmt:
	raco fmt -i $(files)

