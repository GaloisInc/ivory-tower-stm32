include ../stack.mk

IVORYFLAGS ?= --const-fold
# TODO: this needs to be refactored
TESTS  := bsp-led-test bsp-spi-test bsp-i2c-test bsp-uart-test bsp-can-test bsp-rng-test \
          bsp_led_aadl_test bsp_i2c_aadl_test bsp_uart_aadl_test bsp_can_aadl_test
	        
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

.PHONY: bsp_led_aadl_test
bsp_led_aadl_test:
	cabal run bsp-led-aadl-test-gen -- --src-dir=$@ --lib-dir="" $(IVORYFLAGS)

bsp_led_aadl_test_clean:
	rm -rf bsp_led_aadl_test

.PHONY: bsp_i2c_aadl_test
bsp_i2c_aadl_test:
	cabal run bsp_i2c_aadl_test_gen -- --src-dir=bsp_i2c_aadl_test --lib-dir="" $(IVORYFLAGS)

bsp_i2c_aadl_test_clean:
	rm -rf bsp_i2c_aadl_test

.PHONY: bsp_uart_aadl_test
bsp_uart_aadl_test:
	cabal run bsp-uart-aadl-test-gen -- --src-dir=$@ --lib-dir="" $(IVORYFLAGS)

.PHONY: bsp_can_aadl_test
bsp_can_aadl_test:
	cabal run bsp_can_aadl_test_gen -- --src-dir=bsp_can_aadl_test --lib-dir="" $(IVORYFLAGS)

bsp_can_aadl_test_clean:
	rm -rf bsp_can_aadl_test

$(foreach test,$(TESTS),$(eval $(call MKTEST,$(test))))
