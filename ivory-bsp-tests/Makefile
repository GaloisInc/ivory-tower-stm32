
IVORY_REPO ?= ../../ivory
TOWER_REPO ?= ../../tower

IVORYFLAGS = --const-fold

default:
	cabal build

test: bsp-led-test
test: bsp-spi-test
test: bsp-i2c-test
test: bsp-uart-test
test: bsp-can-test
test: bsp-rng-test

.PHONY: bsp-led-test
bsp-led-test:
	cabal run bsp-led-test-gen -- --src-dir=bsp-led-test $(IVORYFLAGS)
	make -C bsp-led-test

bsp-led-test-clean:
	rm -rf bsp-led-test

.PHONY: bsp-spi-test
bsp-spi-test:
	cabal run bsp-spi-test-gen -- --src-dir=bsp-spi-test $(IVORYFLAGS)
	make -C bsp-spi-test

bsp-spi-test-clean:
	rm -rf bsp-spi-test

.PHONY: bsp-i2c-test
bsp-i2c-test:
	cabal run bsp-i2c-test-gen -- --src-dir=bsp-i2c-test $(IVORYFLAGS)
	make -C bsp-i2c-test

bsp-i2c-test-clean:
	rm -rf bsp-i2c-test

.PHONY: bsp-uart-test
bsp-uart-test:
	cabal run bsp-uart-test-gen -- --src-dir=bsp-uart-test $(IVORYFLAGS)
	make -C bsp-uart-test

bsp-uart-test-clean:
	rm -rf bsp-uart-test

.PHONY: bsp-can-test
bsp-can-test:
	cabal run bsp-can-test-gen -- --src-dir=bsp-can-test $(IVORYFLAGS)
	make -C bsp-can-test

bsp-can-test-clean:
	rm -rf bsp-can-test

.PHONY: bsp-rng-test
bsp-rng-test:
	cabal run bsp-rng-test-gen -- --src-dir=bsp-rng-test $(IVORYFLAGS)
	make -C bsp-rng-test

bsp-rng-test-clean:
	rm -rf bsp-rng-test

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(IVORY_REPO)/ivory-hw
	cabal sandbox add-source $(IVORY_REPO)/ivory-opts
	cabal sandbox add-source $(IVORY_REPO)/ivory-serialize
	cabal sandbox add-source $(IVORY_REPO)/ivory-stdlib
	cabal sandbox add-source $(IVORY_REPO)/ivory-backend-c
	cabal sandbox add-source $(TOWER_REPO)/tower
	cabal sandbox add-source $(TOWER_REPO)/tower-config
	cabal sandbox add-source $(TOWER_REPO)/tower-hal
	cabal sandbox add-source ../ivory-freertos-bindings
	cabal sandbox add-source ../ivory-bsp-stm32
	cabal sandbox add-source ../tower-freertos-stm32
	cabal install --dependencies-only

distclean:
	-rm -rf dist

clean: bsp-led-test-clean
clean: bsp-spi-test-clean
clean: bsp-i2c-test-clean
clean: bsp-uart-test-clean
clean: bsp-can-test-clean
clean: bsp-rng-test-clean

clean-sandbox: clean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
