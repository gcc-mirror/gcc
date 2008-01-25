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
