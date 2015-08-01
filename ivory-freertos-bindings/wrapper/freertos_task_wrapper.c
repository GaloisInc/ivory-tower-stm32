
#include "freertos_task_wrapper.h"

#include "FreeRTOS.h"
#include "task.h"
#include "ivory.h"

void ivory_freertos_task_create(void (*tsk)(struct taskarg*),
        uint32_t stacksize, uint8_t priority, const char* const name)
{
    unsigned short usStackDepth = stacksize / sizeof (portSTACK_TYPE);
    TaskHandle_t xHandle = NULL;
    xTaskCreate(
            (TaskFunction_t) tsk, /* pvTaskCode */
            name,                 /* pcName */
            usStackDepth,          /* usStackDepth */
            NULL,                  /* pvParameters */
            priority,              /* uxPriority */
            &xHandle);             /* pxCreatedTask */
    if ( xHandle == NULL) {
        // FAILURE! possible causes:
        // - FreeRTOS heap is out of memory for allocating stack or tcb
        // - priority level is invalid
        ASSERTS(0);
        for(;;);
    }
}
