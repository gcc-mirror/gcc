#include "config.h"
#include "kinds.inc"

/* Note that -1 is used by CFI_type_other, hence, -2 is used for unavailable kinds.  */

#if GFC_C_INT128_T_KIND == 16
#define CFI_type_int128_t (CFI_type_Integer + (16 << CFI_type_kind_shift))
#define CFI_type_int_least128_t (CFI_type_Integer + (16 << CFI_type_kind_shift))
#define CFI_type_int_fast128_t (CFI_type_Integer + (16 << CFI_type_kind_shift))
#elif GFC_C_INT128_T_KIND < 0
#define CFI_type_int128_t -2
#define CFI_type_int_least128_t -2
#define CFI_type_int_fast128_t -2
#else
#error "Unexpected value for GFC_C_INT128_T_KIND"
#endif 

#if GFC_C_LONG_DOUBLE_KIND == 16
#define CFI_type_long_double (CFI_type_Real + (16 << CFI_type_kind_shift))
#define CFI_type_long_double_Complex (CFI_type_Complex + (16 << CFI_type_kind_shift))
#elif GFC_C_LONG_DOUBLE_KIND == 10 
#define CFI_type_long_double (CFI_type_Real + (10 << CFI_type_kind_shift))
#define CFI_type_long_double_Complex (CFI_type_Complex + (10 << CFI_type_kind_shift))
#elif GFC_C_LONG_DOUBLE_KIND == 8 
#define CFI_type_long_double (CFI_type_Real + (8 << CFI_type_kind_shift))
#define CFI_type_long_double_Complex (CFI_type_Complex + (8 << CFI_type_kind_shift))
#elif GFC_C_LONG_DOUBLE_KIND < 0 
#define CFI_type_long_double -2
#define CFI_type_long_double_Complex -2
#else
#error "Unexpected value for GFC_C_LONG_DOUBLE_KIND"
#endif 

#if GFC_C_FLOAT128_KIND == 16
#define CFI_type_float128 (CFI_type_Real + (16 << CFI_type_kind_shift))
#define CFI_type_float128_Complex (CFI_type_Complex + (16 << CFI_type_kind_shift))
#elif GFC_C_FLOAT128_KIND < 0
#define CFI_type_float128 -2
#define CFI_type_float128_Complex -2
#else
#error "Unexpected value for GFC_C_FLOAT128_KIND"
#endif 
