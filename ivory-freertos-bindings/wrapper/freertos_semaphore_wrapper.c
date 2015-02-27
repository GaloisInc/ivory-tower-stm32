
#include "ivory_asserts.h"
#include "freertos_semaphore_wrapper.h"

#include "FreeRTOS.h"
#include "semphr.h"

void ivory_freertos_binary_semaphore_create(struct binary_semaphore* bs_handle) {

	bs_handle->v = xSemaphoreCreateBinary();
	if ( bs_handle->v == NULL ) {
		/* Die: semaphore not created successfully */
		ASSERTS(0);
	}

}

void ivory_freertos_binary_semaphore_takeblocking(struct binary_semaphore* bs_handle) {

	if ( xSemaphoreTake( bs_handle->v, portMAX_DELAY) ) {
	} else {
		/* Die: a take with portMAX_DELAY should always succeed */
		ASSERTS(0);
	}

}

void ivory_freertos_binary_semaphore_give(struct binary_semaphore* bs_handle) {

	if ( xSemaphoreGive( bs_handle->v ) ) {
	} else {
		/* Die: give should always succeed */
		ASSERTS(0);
	}

}

void ivory_freertos_binary_semaphore_give_from_isr(struct binary_semaphore* bs_handle) {

	signed long xHigherPriorityTaskWoken;

	if ( xSemaphoreGiveFromISR( bs_handle->v, &xHigherPriorityTaskWoken ) ) {
	} else {
		/* Die: give should always succeed */
		ASSERTS(0);
	}

	portYIELD_FROM_ISR( xHigherPriorityTaskWoken );
}

