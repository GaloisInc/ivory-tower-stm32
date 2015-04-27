SUBDIRS = tower-freertos-stm32-tests ivory-bsp-tests ivory-freertos-bindings ivory-bsp-stm32 tower-freertos-stm32

default: $(SUBDIRS)

$(SUBDIRS):
	make -C $@ create-sandbox
	make -C $@
	make -C $@ test

.PHONY: $(SUBDIRS)
