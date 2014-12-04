
default: tower-freertos-stm32-tests ivory-bsp-tests
default: ivory-freertos-bindings ivory-bsp-stm32 tower-freertos-stm32

.PHONY: tower-freertos-stm32-tests
tower-freertos-stm32-tests:
	make -C tower-freertos-stm32-tests create-sandbox
	make -C tower-freertos-stm32-tests
	make -C tower-freertos-stm32-tests freertos-test
	make -C tower-freertos-stm32-tests simple-test

.PHONY: ivory-bsp-tests
ivory-bsp-tests:
	make -C ivory-bsp-tests create-sandbox
	make -C ivory-bsp-tests
	make -C ivory-bsp-tests led-test
	make -C ivory-bsp-tests spi-test
	make -C ivory-bsp-tests i2c-test
	make -C ivory-bsp-tests uart-test
	make -C ivory-bsp-tests uart-sm-test

.PHONY: ivory-freertos-bindings
ivory-freertos-bindings:
	make -C ivory-freertos-bindings create-sandbox
	make -C ivory-freertos-bindings

.PHONY: ivory-bsp-stm32
ivory-bsp-stm32:
	make -C ivory-bsp-stm32 create-sandbox
	make -C ivory-bsp-stm32

.PHONY: tower-freertos-stm32
tower-freertos-stm32:
	make -C tower-freertos-stm32 create-sandbox
	make -C tower-freertos-stm32
