NAME=laap
TEST_PACKAGE=laap-test
SOURCES := $(wildcard src/*.lisp) $(wildcard *.asd) $(wildcard t/*.lisp)
QL_LOCAL=$(PWD)/.quicklocal
LOCAL_OPTS=--noinform --noprint --no-sysinit --no-userinit --disable-debugger
QL_OPTS=--load $(QL_LOCAL)/setup.lisp

$(QL_LOCAL)/setup.lisp: $(QL_LOCAL)/quicklisp.lisp
	@sbcl --noinform --noprint --disable-debugger --no-sysinit --no-userinit \
		--load $(QL_LOCAL)/quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "$(QL_LOCAL)")' \
		--eval '(quit)'

$(QL_LOCAL)/quicklisp.lisp:
	@mkdir -p $(QL_LOCAL)
	@wget --no-check-certificate https://beta.quicklisp.org/quicklisp.lisp && mv quicklisp.lisp $(QL_LOCAL)
	@echo '4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17 *$(QL_LOCAL)/quicklisp.lisp' | sha256sum -c -

.PHONY: clean

clean:
	@rm -rf .quicklocal
