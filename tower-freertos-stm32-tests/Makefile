
IVORY_REPO ?= ../../ivory
TOWER_REPO ?= ../../tower

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
	cabal sandbox add-source $(IVORY_REPO)/ivory
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(IVORY_REPO)/ivory-hw
	cabal sandbox add-source $(IVORY_REPO)/ivory-opts
	cabal sandbox add-source $(IVORY_REPO)/ivory-stdlib
	cabal sandbox add-source $(IVORY_REPO)/ivory-backend-c
	cabal sandbox add-source $(TOWER_REPO)/tower
	cabal sandbox add-source $(TOWER_REPO)/tower-config
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
