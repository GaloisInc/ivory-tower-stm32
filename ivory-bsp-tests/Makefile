
IVORY_REPO ?= ../../ivory
TOWER_REPO ?= ../../tower

default:
	cabal build

led-test:
	cabal run bsp-led-test-gen -- --src-dir=bsp-led-test
	make -C bsp-led-test

led-test-clean:
	rm -rf bsp-led-test

spi-test:
	cabal run bsp-spi-test-gen -- --src-dir=bsp-spi-test
	make -C bsp-spi-test

spi-test-clean:
	rm -rf bsp-spi-test

i2c-test:
	cabal run bsp-i2c-test-gen -- --src-dir=bsp-i2c-test
	make -C bsp-i2c-test

i2c-test-clean:
	rm -rf bsp-i2c-test

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
	cabal sandbox add-source $(TOWER_REPO)/tower-statemachine
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
