
#include "ivory_asserts.h"
#include "freertos_mutex_wrapper.h"

#include "FreeRTOS.h"
#include "semphr.h"

void ivory_freertos_mutex_create(struct mutex *m_handle) {

	m_handle->v = xSemaphoreCreateMutex();
	if ( m_handle->v == NULL ) {
		/* Die: mutex not created successfully */
		ASSERTS(0);
	}

}

void ivory_freertos_mutex_takeblocking(struct mutex *m_handle) {

	if ( xSemaphoreTake( m_handle->v, portMAX_DELAY ) ) {
	} else {
		/* Die: a take with portMAX_DELAY should always succeed */
		ASSERTS(0);
	}

}

void ivory_freertos_mutex_give(struct mutex *m_handle) {

	if ( xSemaphoreGive( m_handle->v ) ) {
	} else {
		/* Die: give should always succeed */
		ASSERTS(0);
	}

}

