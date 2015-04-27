IVORY_REPO ?= ../../ivory
TOWER_REPO ?= ../../tower
BSP_REPO ?= ..
include ../Makefile.sandbox

test: freertos-test
test: tower-example-simple

.PHONY: freertos-test
freertos-test:
	cabal run freertos-bindings-test -- --src-dir=freertos-test
	make -C freertos-test

.PHONY: tower-example-simple
tower-example-simple:
	cabal run tower-example-simple -- --src-dir=tower-example-simple
	make -C tower-example-simple

freertos-test-clean:
	-rm -rf freertos-test

tower-example-simple-clean:
	-rm -rf tower-exmple-simple

clean: freertos-test-clean
clean: tower-example-simple-clean
