#ifndef REDUCTION_H
#define REDUCTION_H

#define DO_PRAGMA(x) _Pragma (#x)

#define check_reduction_op(type, op, init, b, gwv_par, gwv_loop)	\
  {									\
    type res, vres;							\
    res = (init);							\
DO_PRAGMA (acc parallel gwv_par copy (res))				\
DO_PRAGMA (acc loop gwv_loop reduction (op:res))			\
    for (i = 0; i < n; i++)						\
      res = res op (b);							\
									\
    vres = (init);							\
    for (i = 0; i < n; i++)						\
      vres = vres op (b);						\
									\
    if (res != vres)							\
      abort ();								\
  }

#define check_reduction_macro(type, op, init, b, gwv_par, gwv_loop)	\
  {									\
    type res, vres;							\
    res = (init);							\
    DO_PRAGMA (acc parallel gwv_par copy(res))				\
DO_PRAGMA (acc loop gwv_loop reduction (op:res))			\
    for (i = 0; i < n; i++)						\
      res = op (res, (b));						\
									\
    vres = (init);							\
    for (i = 0; i < n; i++)						\
      vres = op (vres, (b));						\
									\
    if (res != vres)							\
      abort ();								\
  }

#define max(a, b) (((a) > (b)) ? (a) : (b))
#define min(a, b) (((a) < (b)) ? (a) : (b))

#endif
