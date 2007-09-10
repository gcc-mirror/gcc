#if !defined(DECIMAL128SYMBOLS)
#define DECIMAL128SYMBOLS

#ifdef IN_LIBGCC2
#define decDigitsFromDPD __decDigitsFromDPD
#define decDigitsToDPD __decDigitsToDPD
#define decimal128Canonical __decimal128Canonical
#define decimal128FromNumber __decimal128FromNumber
#define decimal128FromString __decimal128FromString
#define decimal128IsCanonical __decimal128IsCanonical
#define decimal128ToEngString __decimal128ToEngString
#define decimal128ToNumber __decimal128ToNumber
#define decimal128ToString __decimal128ToString
#endif

#endif
