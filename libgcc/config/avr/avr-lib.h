#ifdef FLOAT
#define CMPtype QItype
#define DF SF
#define DI SI
typedef int QItype __attribute__ ((mode (QI)));
#endif

/* fixed-bit.h does not define functions for TA and UTA because
   that part is wrapped in #if MIN_UNITS_PER_WORD > 4.
   This would lead to empty functions for TA and UTA.
   Thus, supply appropriate defines as if HAVE_[U]TA == 1.
   #define HAVE_[U]TA 1 won't work because avr-modes.def
   uses ADJUST_BYTESIZE(TA,8) and fixed-bit.h is not generic enough
   to arrange for such changes of the mode size.  */

typedef unsigned _Fract UTAtype __attribute__ ((mode (UTA)));

#if defined (UTA_MODE)
#define FIXED_SIZE      8       /* in bytes */
#define INT_C_TYPE      UDItype
#define UINT_C_TYPE     UDItype
#define HINT_C_TYPE     USItype
#define HUINT_C_TYPE    USItype
#define MODE_NAME       UTA
#define MODE_NAME_S     uta
#define MODE_UNSIGNED   1
#endif

#if defined (FROM_UTA)
#define FROM_TYPE               4       /* ID for fixed-point */
#define FROM_MODE_NAME          UTA
#define FROM_MODE_NAME_S        uta
#define FROM_INT_C_TYPE         UDItype
#define FROM_SINT_C_TYPE        DItype
#define FROM_UINT_C_TYPE        UDItype
#define FROM_MODE_UNSIGNED      1
#define FROM_FIXED_SIZE         8       /* in bytes */
#elif defined (TO_UTA)
#define TO_TYPE                 4       /* ID for fixed-point */
#define TO_MODE_NAME            UTA
#define TO_MODE_NAME_S          uta
#define TO_INT_C_TYPE           UDItype
#define TO_SINT_C_TYPE          DItype
#define TO_UINT_C_TYPE          UDItype
#define TO_MODE_UNSIGNED        1
#define TO_FIXED_SIZE           8       /* in bytes */
#endif

/* Same for TAmode */

typedef _Fract TAtype  __attribute__ ((mode (TA)));

#if defined (TA_MODE)
#define FIXED_SIZE      8       /* in bytes */
#define INT_C_TYPE      DItype
#define UINT_C_TYPE     UDItype
#define HINT_C_TYPE     SItype
#define HUINT_C_TYPE    USItype
#define MODE_NAME       TA
#define MODE_NAME_S     ta
#define MODE_UNSIGNED   0
#endif

#if defined (FROM_TA)
#define FROM_TYPE               4       /* ID for fixed-point */
#define FROM_MODE_NAME          TA
#define FROM_MODE_NAME_S        ta
#define FROM_INT_C_TYPE         DItype
#define FROM_SINT_C_TYPE        DItype
#define FROM_UINT_C_TYPE        UDItype
#define FROM_MODE_UNSIGNED      0
#define FROM_FIXED_SIZE         8       /* in bytes */
#elif defined (TO_TA)
#define TO_TYPE                 4       /* ID for fixed-point */
#define TO_MODE_NAME            TA
#define TO_MODE_NAME_S          ta
#define TO_INT_C_TYPE           DItype
#define TO_SINT_C_TYPE          DItype
#define TO_UINT_C_TYPE          UDItype
#define TO_MODE_UNSIGNED        0
#define TO_FIXED_SIZE           8       /* in bytes */
#endif
