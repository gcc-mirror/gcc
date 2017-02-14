// Routine using trailing return type syntax.

// { dg-additional-options "-fno-exceptions" }

#define TEMPLATE
#define TYPE int
#define RETURN_1 auto
#define RETURN_2 -> TYPE
#include "../libgomp.oacc-c-c++-common/routine-1.c"
