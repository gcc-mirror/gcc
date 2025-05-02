/* { dg-do run } */

/* Struct reductions.  */

#include <stdlib.h>
#include "reduction.h"

#define ng 8
#define nw 4
#define vl 32

#define N 10

typedef struct { int x, y; } int_pair;
typedef struct { float m, n; } flt_pair;
typedef struct
{
  int i;
  double d;
  float f;
  int a[N];
  int_pair ip;
  flt_pair fp;
} rectype;

static void
init_struct (rectype *rec, int val)
{
  rec->i = val;
  rec->d = (double) val;
  rec->f = (float) val;
  for (int i = 0; i < N; i++)
    rec->a[i] = val;
  rec->ip.x = val;
  rec->ip.y = val;
  rec->fp.m = (float) val;
  rec->fp.n = (float) val;
}

static int
struct_eq (rectype *a, rectype *b)
{
  if (a->i != b->i || a->d != b->d
      || a->f != b->f
      || a->ip.x != b->ip.x
      || a->ip.y != b->ip.y
      || a->fp.m != b->fp.m
      || a->fp.n != b->fp.n)
    return 0;

  for (int i = 0; i < N; i++)
    if (a->a[i] != b->a[i])
      return 0;
  return 1;
}

#define check_reduction_struct_xx(type, op, init, b, gwv_par, gwv_loop, apply) \
  {									\
    type res, vres;							\
    init_struct (&res, init);						\
    DO_PRAGMA (acc parallel gwv_par copy(res))				\
    DO_PRAGMA (acc loop gwv_loop reduction (op:res))			\
    for (int i = 0; i < n; i++)						\
      {									\
	res.i = apply (op, res.i, b);					\
	res.d = apply (op, res.d, b);					\
	res.f = apply (op, res.f, b);					\
	for (int j = 0; j < N; j++)					\
	  res.a[j] = apply (op, res.a[j], b);				\
	res.ip.x = apply (op, res.ip.x, b);				\
	res.ip.y = apply (op, res.ip.y, b);				\
	res.fp.m = apply (op, res.fp.m, b);				\
	res.fp.n = apply (op, res.fp.n, b);				\
      }									\
									\
    init_struct (&vres, init);						\
    for (int i = 0; i < n; i++)						\
      {									\
        vres.i = apply (op, vres.i, b);					\
	vres.d = apply (op, vres.d, b);					\
	vres.f = apply (op, vres.f, b);					\
	for (int j = 0; j < N; j++)					\
	  vres.a[j] = apply (op, vres.a[j], b);				\
	vres.ip.x = apply (op, vres.ip.x, b);				\
	vres.ip.y = apply (op, vres.ip.y, b);				\
	vres.fp.m = apply (op, vres.fp.m, b);				\
	vres.fp.n = apply (op, vres.fp.n, b);				\
      }									\
									\
    if (!struct_eq (&res, &vres))					\
      __builtin_abort ();						\
  }

#define operator_apply(op, a, b) (a op b)
#define check_reduction_struct_op(type, op, init, b, gwv_par, gwv_loop)	\
  check_reduction_struct_xx(type, op, init, b, gwv_par, gwv_loop, operator_apply)

#define function_apply(op, a, b) (op (a, b))
#define check_reduction_struct_macro(type, op, init, b, gwv_par, gwv_loop) \
  check_reduction_struct_xx(type, op, init, b, gwv_par, gwv_loop, function_apply)

#define check_reduction_struct_op_all(type, opr, init, b)	\
  check_reduction_xxx_xx_all (struct, op, type, opr, init, b)
#define check_reduction_struct_macro_all(type, opr, init, b)		\
  check_reduction_xxx_xx_all (struct, macro, type, opr, init, b)

int
main (void)
{
  const int n = 10;
  int ints[n];

  for (int i = 0; i < n; i++)
    ints[i] = i + 1;

  check_reduction_struct_op_all (rectype, +, 0, ints[i]);
  check_reduction_struct_op_all (rectype, *, 1, ints[i]);
  check_reduction_struct_macro_all (rectype, min, n + 1, ints[i]);
  check_reduction_struct_macro_all (rectype, max, -1, ints[i]);
  return 0;
}
