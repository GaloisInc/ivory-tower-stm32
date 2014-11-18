
default:
	make -C ivory-freertos-bindings
	make -C ivory-bsp-stm32
	make -C tower-freertos-stm32
	make -C tower-freertos-stm32-tests
	make -C ivory-bsp-tests

