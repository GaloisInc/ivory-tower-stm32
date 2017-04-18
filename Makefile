include stack.mk

SUBDIRS = tower-freertos-stm32-tests ivory-bsp-tests ivory-freertos-bindings ivory-bsp-stm32 \
					tower-freertos-stm32 tower-echronos-stm32

default: $(SUBDIRS)

$(SUBDIRS):
	make -C $@ test

.PHONY: $(SUBDIRS)

TRAVIS_STACK ?= stack --no-terminal

travis-test:
	$(TRAVIS_STACK) setup
	$(TRAVIS_STACK) build --test --no-run-tests --haddock --no-haddock-deps --pedantic
	make $(SUBDIRS)
