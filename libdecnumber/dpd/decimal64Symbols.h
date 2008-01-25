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
#define COMBEXP __decnnCOMBEXP
#define COMBMSD __decnnCOMBMSD

/* DPD2BIN and BIN2DPD are used in support for decimal32/decimal64/decimal128
   and also in support for decSingle/decDouble/decQuad.  Rename them in case
   both types of support are used in the same executable.  */
#undef DPD2BIN
#define DPD2BIN __decnnDPD2BIN
#undef BIN2DPD
#define BIN2DPD __decnnBIN2DPD
#endif

#endif
