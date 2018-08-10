// Templated routine using trailing return type syntax.

#define TEMPLATE template<typename TYPE>
#define RETURN_1 auto
#define RETURN_2 -> TYPE
#include "../libgomp.oacc-c-c++-common/routine-1.c"
