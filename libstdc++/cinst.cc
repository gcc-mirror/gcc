// Instantiation file for the -*- C++ -*- complex number classes.
// Copyright (C) 1994 Free Software Foundation

#ifdef F
typedef float f;
#endif
#ifdef D
typedef double f;
#endif
#ifdef LD
typedef long double f;
#endif

#if defined (MAIN) && defined (__GNUG__)
#ifdef F
#pragma implementation "fcomplex"
#endif
#ifdef D
#pragma implementation "dcomplex"
#endif
#ifdef LD
#pragma implementation "ldcomplex"
#endif
#endif

#if 0
#define _G_NO_EXTERN_TEMPLATES
#endif
#include <std/complext.cc>

typedef complex<f> c;
typedef const c& ccr;

#ifdef MAIN
template c& __doapl (c*, ccr);
template c& __doaml (c*, ccr);
template c& __doami (c*, ccr);
template c& __doadv (c*, ccr);
#endif

#ifdef ADDCC
template c operator+ (ccr, ccr);
#endif
#ifdef ADDCF
template c operator+ (ccr, f);
#endif
#ifdef ADDFC
template c operator+ (f, ccr);
#endif
#ifdef SUBCC
template c operator- (ccr, ccr);
#endif
#ifdef SUBCF
template c operator- (ccr, f);
#endif
#ifdef SUBFC
template c operator- (f, ccr);
#endif
#ifdef MULCC
template c operator* (ccr, ccr);
#endif
#ifdef MULCF
template c operator* (ccr, f);
#endif
#ifdef MULFC
template c operator* (f, ccr);
#endif
#ifdef DIVCC
template c operator/ (ccr, ccr);
#endif
#ifdef DIVCF
template c operator/ (ccr, f);
#endif
#ifdef DIVFC
template c operator/ (f, ccr);
#endif
#ifdef PLUS
template c operator+ (ccr);
#endif
#ifdef MINUS
template c operator- (ccr);
#endif
#ifdef EQCC
template bool operator== (ccr, ccr);
#endif
#ifdef EQCF
template bool operator== (ccr, f);
#endif
#ifdef EQFC
template bool operator== (f, ccr);
#endif
#ifdef NECC
template bool operator!= (ccr, ccr);
#endif
#ifdef NECF
template bool operator!= (ccr, f);
#endif
#ifdef NEFC
template bool operator!= (f, ccr);
#endif
#ifdef ABS
template f abs (ccr);
#endif
#ifdef ARG
template f arg (ccr);
#endif
#ifdef POLAR
template c polar (f, f);
#endif
#ifdef CONJ
template c conj (ccr);
#endif
#ifdef NORM
template f norm (ccr);
#endif
#ifdef COS
template c cos (ccr);
#endif
#ifdef COSH
template c cosh (ccr);
#endif
#ifdef EXP
template c exp (ccr);
#endif
#ifdef LOG
template c log (ccr);
#endif
#ifdef POWCC
template c pow (ccr, ccr);
#endif
#ifdef POWCF
template c pow (ccr, f);
#endif
#ifdef POWCI
template c pow (ccr, int);
#endif
#ifdef POWFC
template c pow (f, ccr);
#endif
#ifdef SIN
template c sin (ccr);
#endif
#ifdef SINH
template c sinh (ccr);
#endif
#ifdef SQRT
template c sqrt (ccr);
#endif
#ifdef EXTRACT
template istream& operator>> (istream&, complex<f>&);
#endif
#ifdef INSERT
template ostream& operator<< (ostream&, const complex<f>&);
#endif
