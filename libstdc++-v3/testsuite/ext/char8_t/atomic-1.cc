// Test that char8_t related atomic types and macros are not present when
// -fchar8_t is not enabled.
// { dg-do compile }
// { dg-options "-fno-char8_t" }

#include <atomic>

#if defined(ATOMIC_CHAR8_T_LOCK_FREE)
#error ATOMIC_CHAR8_T_LOCK_FREE is defined!
#endif

std::atomic_char8_t x1; // { dg-error "error: .atomic_char8_t. in namespace .std. does not name a type" "char8_t" }
