
default:
	make -C ivory-freertos-bindings create-sandbox
	make -C ivory-freertos-bindings
	make -C ivory-bsp-stm32 create-sandbox
	make -C ivory-bsp-stm32
	make -C tower-freertos-stm32 create-sandbox
	make -C tower-freertos-stm32
	make -C tower-freertos-stm32-tests create-sandbox
	make -C tower-freertos-stm32-tests
	make -C tower-freertos-stm32-tests freertos-test
	make -C tower-freertos-stm32-tests simple-test
	make -C ivory-bsp-tests create-sandbox
	make -C ivory-bsp-tests
	make -C ivory-bsp-tests led-test
	make -C ivory-bsp-tests spi-test
	make -C ivory-bsp-tests i2c-test

