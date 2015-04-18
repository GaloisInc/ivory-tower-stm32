
#ifndef ASSERT_REDIRECT_H
#define ASSERT_REDIRECT_H

#if defined(IVORY_TEST) || defined(IVORY_DEPLOY)
#include "ivory_asserts.h"
#define assert(arg) ASSERTS(arg)
#else
#error "when compiling with ivory-tower-stm32, must use IVORY_TEST or IVORY_DEPLOY cflag"
#endif

#endif /* ASSERT_REDIRECT_H */

