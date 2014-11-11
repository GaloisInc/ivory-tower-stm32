CREATE_SANDBOX_SRCS=																		 \
  ../../ivory/ivory/																		 \
	../../ivory/ivory-artifact/														 \
	../../ivory/ivory-hw/																	 \
	../../ivory/ivory-opts/																 \
	../../ivory/ivory-stdlib/															 \
	../../ivory/ivory-backend-c/													 \
	../../tower/tower/																		 \
	../../tower/tower-config                               \
	../ivory-freertos-bindings/														 \
	../ivory-bsp-stm32/																		 \
	../tower-freertos-stm32/

default:
	cabal build

.PHONY: freertos-test
freertos-test:
	cabal run freertos-bindings-test -- --src-dir=freertos-test
	make -C freertos-test

.PHONY: simple-test
simple-test:
	cabal run tower-example-simple -- --src-dir=tower-example-simple
	make -C tower-example-simple


create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(CREATE_SANDBOX_SRCS)
	cabal install $(CREATE_SANDBOX_SRCS)

dist-clean:
	-rm -rf dist

simple-clean: dist-clean
	-rm -rf tower-exmple-simple

freertos-test-clean: dist-clean
	-rm -rf freertos-test

clean: dist-clean simple-clean freertos-test-clean

clean-sandbox: clean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
