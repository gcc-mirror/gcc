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

#define check_reduction_array_xx(type, var, var_in_clause, op, init, b, \
				 gwv_par, gwv_loop, apply)		\
  {									\
   type var[N], var ## _check[N];					\
   for (int i = 0; i < N; i++)						\
     var[i] = var ## _check[i] = (init);				\
   DO_PRAGMA (acc parallel gwv_par copy (var_in_clause))		\
   DO_PRAGMA (acc loop gwv_loop reduction (op: var_in_clause))		\
   for (int i = 0; i < n; i++)						\
     for (int j = 0; j < N; j++)					\
       var[j] = apply (op, var[j], (b));				\
									\
   for (int i = 0; i < n; i++)						\
     for (int j = 0; j < N; j++)					\
       var ## _check[j] = apply (op, var ## _check[j], (b));		\
									\
   for (int j = 0; j < N; j++)						\
     if (var[j] != var ## _check[j])					\
       abort ();							\
  }

#define operator_apply(op, a, b) (a op b)
#define check_reduction_array_op(type, op, init, b, gwv_par, gwv_loop)	\
  check_reduction_array_xx (type, v, v, op, init, b, gwv_par, gwv_loop,	\
			    operator_apply)
#define check_reduction_arraysec_op(type, op, init, b, gwv_par, gwv_loop) \
  check_reduction_array_xx (type, v, v[:N], op, init, b, gwv_par, gwv_loop, \
			    operator_apply)


#define function_apply(op, a, b) (op (a, b))
#define check_reduction_array_macro(type, op, init, b, gwv_par, gwv_loop)\
  check_reduction_array_xx (type, v, v, op, init, b, gwv_par, gwv_loop,	\
			    function_apply)
#define check_reduction_arraysec_macro(type, op, init, b, gwv_par, gwv_loop)\
  check_reduction_array_xx (type, v, v[:N], op, init, b, gwv_par, gwv_loop, \
			    function_apply)

#define check_reduction_xxx_xx_all(tclass, form, type, op, init, b)	\
  check_reduction_ ## tclass ## _ ## form (type, op, init, b, num_gangs (ng), gang);	\
  check_reduction_ ## tclass ## _ ## form (type, op, init, b, num_workers (nw), worker); \
  check_reduction_ ## tclass ## _ ## form (type, op, init, b, vector_length (vl), vector); \
  check_reduction_ ## tclass ## _ ## form (type, op, init, b,			\
					   num_gangs (ng) num_workers (nw), gang worker); \
  check_reduction_ ## tclass ## _ ## form (type, op, init, b,			\
					   num_gangs (ng) vector_length (vl), gang vector); \
  check_reduction_ ## tclass ## _ ## form (type, op, init, b,			\
					   num_workers (nw) vector_length (vl), worker vector); \
  check_reduction_ ## tclass ## _ ## form (type, op, init, b, \
					   num_gangs (ng) num_workers (nw) vector_length (vl), \
					   gang worker vector);

#define max(a, b) (((a) > (b)) ? (a) : (b))
#define min(a, b) (((a) < (b)) ? (a) : (b))

#endif
