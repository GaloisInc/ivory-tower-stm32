
#ifndef __IVORY_FREERTOS_MUTEX_WRAPPER_H__
#define __IVORY_FREERTOS_MUTEX_WRAPPER_H__

#include "FreeRTOS.h"
#include "semphr.h"

struct mutex {
	SemaphoreHandle_t v;
};

void ivory_freertos_mutex_create(struct mutex *m_handle);

void ivory_freertos_mutex_takeblocking(struct mutex *m_handle);

void ivory_freertos_mutex_give(struct mutex *m_handle);

#endif // __IVORY_FREERTOS_MUTEX_WRAPPER_H__
