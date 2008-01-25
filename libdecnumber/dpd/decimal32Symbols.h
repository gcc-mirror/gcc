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
