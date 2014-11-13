
default:
	cabal build

led-test:
	cabal run bsp-led-test-gen -- --src-dir=bsp-led-test --platform=px4fmuXXX
	make -C bsp-led-test

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
	cabal sandbox add-source ../../tower/tower-statemachine
	cabal sandbox add-source ../ivory-freertos-bindings
	cabal sandbox add-source ../ivory-bsp-stm32
	cabal sandbox add-source ../tower-freertos-stm32
	cabal install --dependencies-only

dist-clean:
	-rm -rf dist

clean: dist-clean

clean-sandbox: clean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config