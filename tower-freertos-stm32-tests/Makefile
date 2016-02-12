include ../stack.mk

test: freertos-test
test: tower-example-simple

.PHONY: freertos-test
freertos-test:
	stack build . --exec 'freertos-bindings-test --src-dir=freertos-test'
	make -C freertos-test

.PHONY: tower-example-simple
tower-example-simple:
	stack build . --exec 'tower-example-simple --src-dir=tower-example-simple'
	make -C tower-example-simple

.PHONY: freertos-test-clean
freertos-test-clean:
	-rm -rf freertos-test

.PHONY: tower-example-simple-clean
tower-example-simple-clean:
	-rm -rf tower-exmple-simple

.PHONY: clean
clean: freertos-test-clean
clean: tower-example-simple-clean
