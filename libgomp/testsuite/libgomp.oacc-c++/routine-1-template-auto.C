// Templated routine with "auto" return type.

// { dg-additional-options "-fno-exceptions" }

#define TEMPLATE template<typename TYPE>
#define RETURN_1 auto
#define RETURN_2
#include "../libgomp.oacc-c-c++-common/routine-1.c"
