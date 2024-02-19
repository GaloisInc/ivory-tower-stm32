
#include "stm32_freertos_init.h"
#include "ivory.h"

extern uint32_t _sidata;
extern uint32_t _sdata;
extern uint32_t _edata;
extern uint32_t _sbss;
extern uint32_t _ebss;

void init_relocate(void){
	uint32_t *src, *dst;
	/* relocate sidata to sdata */
	src = &_sidata;
	dst = &_sdata;
	if (src != dst) {
		while (dst < &_edata) {
			*dst++ = *src++;
		}
	}
	/* zero bss */
	dst = &_sbss;
	while (dst < &_ebss) {
		*dst++ = 0;
	}
}

extern void __libc_init_array(void);

void init_libc(void) {
	__libc_init_array();
}

// XXX we can probably move this to be an Ivory module that calls task_wrapper

#include "FreeRTOS.h"
#include "task.h"
extern void tower_entry(void);

static void tower_launch_task(void *machtnichts) {
	tower_entry();
	vTaskSuspend(NULL);
}

int main (void) {
	xTaskCreate(tower_launch_task, "twrlaunch",
		256, NULL, 0, NULL);
	vTaskStartScheduler();
	for(;;);
	return 0; // unreachable
}

extern void vApplicationStackOverflowHook ( TaskHandle_t xTask,
                                            char * pcTaskName )
{
  /* Check pcTaskName for the name of the offending task,
   * or pxCurrentTCB if pcTaskName has itself been corrupted. */
  ( void ) xTask;
  ( void ) pcTaskName;
  ASSERTS(0);
  for(;;);
}
