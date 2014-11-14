
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
	cabal sandbox add-source ../../ivory/ivory
	cabal sandbox add-source ../../ivory/ivory-artifact
	cabal sandbox add-source ../../ivory/ivory-hw
	cabal sandbox add-source ../../ivory/ivory-opts
	cabal sandbox add-source ../../ivory/ivory-stdlib
	cabal sandbox add-source ../../ivory/ivory-backend-c
	cabal sandbox add-source ../../tower/tower
	cabal sandbox add-source ../../tower/tower-config
	cabal sandbox add-source ../ivory-freertos-bindings
	cabal sandbox add-source ../ivory-bsp-stm32
	cabal sandbox add-source ../tower-freertos-stm32
	cabal install --dependencies-only

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
