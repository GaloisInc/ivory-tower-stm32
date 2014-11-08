
#ifndef STM32_FREERTOS_INIT_H__
#define STM32_FREERTOS_INIT_H__

#include <stdint.h>

void init_relocate(void);
void init_libc(void);
int  main(void);

#endif // STM32_FREERTOS_INIT_H__

