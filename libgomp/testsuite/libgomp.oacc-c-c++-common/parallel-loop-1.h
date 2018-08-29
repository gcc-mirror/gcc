#define S
#define N(x) M(x, G, none)
#include "parallel-loop-2.h"
#undef S
#undef N
#define S auto
#define N(x) M(x, G, auto)
#include "parallel-loop-2.h"
#undef S
#undef N
#define S independent
#define N(x) M(x, G, independent)
#include "parallel-loop-2.h"
#undef S
#undef N
#define S seq
#define N(x) M(x, G, seq)
#include "parallel-loop-2.h"
#undef S
#undef N
