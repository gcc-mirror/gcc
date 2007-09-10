#if !defined(DECIMAL64SYMBOLS)
#define DECIMAL64SYMBOLS

#ifdef IN_LIBGCC2
#define decDigitsFromDPD __decDigitsFromDPD
#define decDigitsToDPD __decDigitsToDPD
#define decimal64Canonical __decimal64Canonical
#define decimal64FromNumber __decimal64FromNumber
#define decimal64FromString __decimal64FromString
#define decimal64IsCanonical __decimal64IsCanonical
#define decimal64ToEngString __decimal64ToEngString
#define decimal64ToNumber __decimal64ToNumber
#define decimal64ToString __decimal64ToString
#endif

#endif
