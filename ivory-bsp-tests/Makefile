include ../stack.mk

IVORYFLAGS ?= --const-fold
TESTS  := bsp-led-test bsp-spi-test bsp-i2c-test bsp-uart-test bsp-can-test bsp-rng-test
CLEANS := $(foreach test,$(TESTS),$(test)-clean)

.PHONY: test clean $(TESTS) $(CLEANS)
test: $(TESTS)
clean: $(CLEANS)

define MKTEST
$(1):
	stack build . --exec '$(1)-gen --src-dir=$(1) $(IVORYFLAGS)'
	make -C $(1)
$(1)-clean:
	rm -rf $(1)
endef

$(foreach test,$(TESTS),$(eval $(call MKTEST,$(test))))
