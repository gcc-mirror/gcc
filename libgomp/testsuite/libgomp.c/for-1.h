#define S
#define N(x) M(x, G, static)
#include "for-2.h"
#undef S
#undef N
#define S schedule(static, 32)
#define N(x) M(x, G, static32)
#include "for-2.h"
#undef S
#undef N
#define S schedule(auto)
#define N(x) M(x, G, auto)
#include "for-2.h"
#undef S
#undef N
#define S schedule(guided, 32)
#define N(x) M(x, G, guided32)
#include "for-2.h"
#undef S
#undef N
#define S schedule(runtime)
#define N(x) M(x, G, runtime)
#include "for-2.h"
#undef S
#undef N
