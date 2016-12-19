include ../stack.mk

IVORYFLAGS ?= --const-fold
TESTS      := bsp-led-test bsp-spi-test bsp-i2c-test bsp-uart-test bsp-can-test bsp-rng-test bsp-i2c-fuzzer
AADL_TESTS := bsp_led_aadl_test bsp_i2c_aadl_test bsp_uart_aadl_test bsp_can_aadl_test bsp_dma_uart_aadl_test
CLEANS     := $(foreach test,$(TESTS),$(test)-clean) \
	            $(foreach test,$(AADL_TESTS),$(test)_clean)

.PHONY: test clean $(TESTS) $(AADL_TESTS) $(CLEANS)
test: $(TESTS) $(AADL_TESTS)
clean: $(CLEANS)

define MKTEST
$(1):
	stack build . --exec '$(1)-gen --src-dir=$(1) $(IVORYFLAGS)'
	make -C $(1)
$(1)-clean:
	rm -rf $(1)
endef

define MK_AADL_TEST
$(1):
	stack build . --exec '$(1)_gen --src-dir=$(1) $(IVORYFLAGS)'
$(1)_clean:
	rm -rf $(1)
endef

$(foreach test,$(TESTS),$(eval $(call MKTEST,$(test))))
$(foreach test,$(AADL_TESTS),$(eval $(call MK_AADL_TEST,$(test))))
