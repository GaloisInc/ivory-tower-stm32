IVORY_REPO ?= ../../ivory
TOWER_REPO ?= ../../tower
BSP_REPO ?= ..
include ../Makefile.sandbox

IVORYFLAGS = --const-fold

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

.PHONY: bsp-dma-uart-test
bsp-dma-uart-test:
	cabal run bsp-dma-uart-test-gen -- --src-dir=bsp-dma-uart-test $(IVORYFLAGS)
	make -C bsp-dma-uart-test

bsp-dma-uart-test-clean:
	rm -rf bsp-dma-uart-test

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

clean: bsp-led-test-clean
clean: bsp-spi-test-clean
clean: bsp-i2c-test-clean
clean: bsp-uart-test-clean
clean: bsp-dma-uart-test-clean
clean: bsp-can-test-clean
clean: bsp-rng-test-clean
