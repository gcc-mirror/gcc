#include <stdint.h>

#if __LITTLE_ENDIAN__
# define IEEE_8087
#elif __BIG_ENDIAN__
# define IEEE_MC68k
#else
// Because quad math is typically performed on little-endian hardware
//# error "Unknown endianness"
# define IEEE_8087
#endif

// This should be 32 bit integer type
#define Long int
