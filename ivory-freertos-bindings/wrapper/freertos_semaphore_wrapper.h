
#ifndef __IVORY_FREERTOS_SEMAPHORE_WRAPPER_H__
#define __IVORY_FREERTOS_SEMAPHORE_WRAPPER_H__

#include "FreeRTOS.h"
#include "semphr.h"

struct binary_semaphore {
	SemaphoreHandle_t v;
};

void ivory_freertos_binary_semaphore_create(struct binary_semaphore* bs_handle);

void ivory_freertos_binary_semaphore_takeblocking(struct binary_semaphore* bs_handle);

void ivory_freertos_binary_semaphore_give(struct binary_semaphore* bs_handle);

void ivory_freertos_binary_semaphore_give_from_isr(struct binary_semaphore* bs_handle);

#endif // __IVORY_FREERTOS_SEMAPHORE_WRAPPER_H__
