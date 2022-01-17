// { dg-do preprocess }

// PR libstdc++/103650 libstdc++ headers defined LT_OBJDIR and STDC_HEADERS

#define LT_OBJDIR 99
#define STDC_HEADERS 99
#include <utility>
#if LT_OBJDIR != 99
# error LT_OBJDIR redefined
#endif
#if STDC_HEADERS != 99
# error STDC_HEADERS redefined
#endif
