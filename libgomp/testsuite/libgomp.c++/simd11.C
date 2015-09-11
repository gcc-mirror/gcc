// { dg-do run }
// { dg-additional-options "-msse2" { target sse2_runtime } }
// { dg-additional-options "-mavx" { target avx_runtime } }

#include "../libgomp.c/simd-15.c"
