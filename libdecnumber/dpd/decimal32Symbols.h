#if !defined(DECIMAL32SYMBOLS)
#define DECIMAL32SYMBOLS

#ifdef IN_LIBGCC2
#define decDigitsFromDPD __decDigitsFromDPD
#define decDigitsToDPD __decDigitsToDPD
#define decimal32Canonical __decimal32Canonical
#define decimal32FromNumber __decimal32FromNumber
#define decimal32FromString __decimal32FromString
#define decimal32IsCanonical __decimal32IsCanonical
#define decimal32ToEngString __decimal32ToEngString
#define decimal32ToNumber __decimal32ToNumber
#define decimal32ToString __decimal32ToString
#endif

#endif
