#include "matrix-helper.h"

#ifndef COMMON_TOP_TRANSFORM
#define COMMON_TOP_TRANSFORM
#endif

#ifndef IMPLEMENTATION_FILE
#define IMPLEMENTATION_FILE "matrix-1.h"
#endif

#define FUN_NAME_SUFFIX 1
#ifdef COMMON_DIRECTIVE
#define DIRECTIVE DO_PRAGMA(COMMON_DIRECTIVE)
#else
#define DIRECTIVE
#endif
#define TRANSFORMATION1 DO_PRAGMA(COMMON_TOP_TRANSFORM) _Pragma("omp unroll partial(2)") _Pragma("omp tile sizes(10)")
#define TRANSFORMATION2
#define TRANSFORMATION3
#include IMPLEMENTATION_FILE

#define FUN_NAME_SUFFIX 2
#ifdef COMMON_DIRECTIVE
#define DIRECTIVE DO_PRAGMA(COMMON_DIRECTIVE COLLAPSE_3)
#else
#define DIRECTIVE
#endif
#define TRANSFORMATION1 DO_PRAGMA(COMMON_TOP_TRANSFORM) _Pragma("omp tile sizes(8,16,4)")
#define TRANSFORMATION2
#define TRANSFORMATION3
#include IMPLEMENTATION_FILE

#define FUN_NAME_SUFFIX 3
#ifdef COMMON_DIRECTIVE
#define DIRECTIVE DO_PRAGMA(COMMON_DIRECTIVE COLLAPSE_2)
#else
#define DIRECTIVE
#endif
#define TRANSFORMATION1 DO_PRAGMA(COMMON_TOP_TRANSFORM) _Pragma("omp tile sizes(8, 8)")
#define TRANSFORMATION2
#define TRANSFORMATION3
#include IMPLEMENTATION_FILE

#define FUN_NAME_SUFFIX 4
#ifdef COMMON_DIRECTIVE
#define DIRECTIVE DO_PRAGMA(COMMON_DIRECTIVE COLLAPSE_1)
#else
#define DIRECTIVE
#endif
#define TRANSFORMATION1 DO_PRAGMA(COMMON_TOP_TRANSFORM) _Pragma("omp tile sizes(8, 8)")
#define TRANSFORMATION2
#define TRANSFORMATION3
#include IMPLEMENTATION_FILE

#define FUN_NAME_SUFFIX 5
#ifdef COMMON_DIRECTIVE
#define DIRECTIVE DO_PRAGMA(COMMON_DIRECTIVE COLLAPSE_1)
#else
#define DIRECTIVE
#endif
#define TRANSFORMATION1 DO_PRAGMA(COMMON_TOP_TRANSFORM) _Pragma("omp tile sizes(8, 8, 8)")
#define TRANSFORMATION2
#define TRANSFORMATION3
#include IMPLEMENTATION_FILE

#define FUN_NAME_SUFFIX 6
#ifdef COMMON_DIRECTIVE
#define DIRECTIVE DO_PRAGMA(COMMON_DIRECTIVE COLLAPSE_1)
#else
#define DIRECTIVE
#endif
#define TRANSFORMATION1 DO_PRAGMA(COMMON_TOP_TRANSFORM) _Pragma("omp tile sizes(10)") _Pragma("omp unroll partial(2)")
#define TRANSFORMATION2
#define TRANSFORMATION3
#include IMPLEMENTATION_FILE

#define FUN_NAME_SUFFIX 7
#ifdef COMMON_DIRECTIVE
#define DIRECTIVE DO_PRAGMA(COMMON_DIRECTIVE COLLAPSE_2)
#else
#define DIRECTIVE
#endif
#define TRANSFORMATION1 DO_PRAGMA(COMMON_TOP_TRANSFORM) _Pragma("omp tile sizes(7, 11)")
#define TRANSFORMATION2 _Pragma("omp unroll partial(7)")
#define TRANSFORMATION3
#include IMPLEMENTATION_FILE

#define FUN_NAME_SUFFIX 8
#ifdef COMMON_DIRECTIVE
#define DIRECTIVE DO_PRAGMA(COMMON_DIRECTIVE COLLAPSE_2)
#else
#define DIRECTIVE
#endif
#define TRANSFORMATION1 DO_PRAGMA(COMMON_TOP_TRANSFORM) _Pragma("omp tile sizes(7, 11)")
#define TRANSFORMATION2 _Pragma("omp tile sizes(7)") _Pragma("omp unroll partial(7)")
#define TRANSFORMATION3
#include IMPLEMENTATION_FILE

#define FUN_NAME_SUFFIX 9
#ifdef COMMON_DIRECTIVE
#define DIRECTIVE DO_PRAGMA(COMMON_DIRECTIVE COLLAPSE_2)
#else
#define DIRECTIVE
#endif
#define TRANSFORMATION1 DO_PRAGMA(COMMON_TOP_TRANSFORM) _Pragma("omp tile sizes(7, 11)")
#define TRANSFORMATION2 _Pragma("omp tile sizes(7)") _Pragma("omp unroll partial(3)") _Pragma("omp tile sizes(7)")
#define TRANSFORMATION3
#include IMPLEMENTATION_FILE

#define FUN_NAME_SUFFIX 10
#ifdef COMMON_DIRECTIVE
#define DIRECTIVE DO_PRAGMA(COMMON_DIRECTIVE COLLAPSE_1)
#else
#define DIRECTIVE
#endif
#define TRANSFORMATION1 DO_PRAGMA(COMMON_TOP_TRANSFORM) _Pragma("omp unroll partial(5)") _Pragma("omp tile sizes(7)") _Pragma("omp unroll partial(3)") _Pragma("omp tile sizes(7)")
#define TRANSFORMATION2
#define TRANSFORMATION3
#include IMPLEMENTATION_FILE

#define FUN_NAME_SUFFIX 11
#ifdef COMMON_DIRECTIVE
#define DIRECTIVE DO_PRAGMA(COMMON_DIRECTIVE COLLAPSE_2)
#else
#define DIRECTIVE
#endif
#define TRANSFORMATION1 DO_PRAGMA(COMMON_TOP_TRANSFORM)
#define TRANSFORMATION2 _Pragma("omp unroll partial(5)") _Pragma("omp tile sizes(7)") _Pragma("omp unroll partial(3)") _Pragma("omp tile sizes(7)")
#define TRANSFORMATION3
#include IMPLEMENTATION_FILE

#define FUN_NAME_SUFFIX 12
#ifdef COMMON_DIRECTIVE
#define DIRECTIVE DO_PRAGMA(COMMON_DIRECTIVE COLLAPSE_3)
#else
#define DIRECTIVE
#endif
#define TRANSFORMATION1 DO_PRAGMA(COMMON_TOP_TRANSFORM)
#define TRANSFORMATION2
#define TRANSFORMATION3 _Pragma("omp unroll partial(5)") _Pragma("omp tile sizes(7)") _Pragma("omp unroll partial(3)") _Pragma("omp tile sizes(7)")
#include IMPLEMENTATION_FILE

#define FUN_NAME_SUFFIX 13
#ifdef COMMON_DIRECTIVE
#define DIRECTIVE DO_PRAGMA(COMMON_DIRECTIVE COLLAPSE_3)
#else
#define DIRECTIVE
#endif
#define TRANSFORMATION1 DO_PRAGMA(COMMON_TOP_TRANSFORM)
#define TRANSFORMATION2 _Pragma("omp tile sizes(7,8)")
#define TRANSFORMATION3 _Pragma("omp unroll partial(3)") _Pragma("omp tile sizes(7)")
#include IMPLEMENTATION_FILE

int
main ()
{
  main1 ();
  main2 ();
  main3 ();
  main4 ();
  main5 ();
  main6 ();
  main7 ();
  main8 ();
  main9 ();
  main10 ();
  main11 ();
  main12 ();
  main13 ();
  return 0;
}
