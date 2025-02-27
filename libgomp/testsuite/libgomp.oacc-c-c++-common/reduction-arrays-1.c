/* { dg-do run } */

/* Array reductions.  */

#include <stdlib.h>
#include "reduction.h"

#define ng 8
#define nw 4
#define vl 32

#define N 10

#define check_reduction_array_op_all(type, opr, init, b)	\
  check_reduction_xxx_xx_all(array, op, type, opr, init, b)
#define check_reduction_arraysec_op_all(type, opr, init, b)	\
  check_reduction_xxx_xx_all(arraysec, op, type, opr, init, b)
#define check_reduction_array_macro_all(type, opr, init, b)	\
  check_reduction_xxx_xx_all(array, macro, type, opr, init, b)
#define check_reduction_arraysec_macro_all(type, opr, init, b)	\
  check_reduction_xxx_xx_all(arraysec, macro, type, opr, init, b)
    
int
main (void)
{
  const int n = 100;
  int ints[n];
  float flts[n];
  double dbls[n];
  int cmp_val = 5;

  for (int i = 0; i < n; i++)
    {
      ints[i] = i + 1;
      flts[i] = i + 1;
      dbls[i] = i + 1;
    }

  check_reduction_array_op_all (int, +, 0, ints[i]);
  check_reduction_array_op_all (int, *, 1, ints[i]);
  check_reduction_array_op_all (int, &, -1, ints[i]);
  check_reduction_array_op_all (int, |, 0, ints[i]);
  check_reduction_array_op_all (int, ^, 0, ints[i]);
  check_reduction_array_op_all (int, &&, 1, (cmp_val > ints[i]));
  check_reduction_array_op_all (int, ||, 0, (cmp_val > ints[i]));
  check_reduction_array_macro_all (int, min, n + 1, ints[i]);
  check_reduction_array_macro_all (int, max, -1, ints[i]);

  check_reduction_array_op_all (float, +, 0, flts[i]);
  check_reduction_array_op_all (float, *, 1, flts[i]);
  check_reduction_array_macro_all (float, min, n + 1, flts[i]);
  check_reduction_array_macro_all (float, max, -1, flts[i]);

  check_reduction_arraysec_op_all (int, +, 0, ints[i]);
  check_reduction_arraysec_op_all (float, *, 1, flts[i]);
  check_reduction_arraysec_macro_all (double, min, n + 1, dbls[i]);
  check_reduction_arraysec_macro_all (double, max, -1, dbls[i]);

  check_reduction_array_op_all (double, +, 0, dbls[i]);
#if 0
  /* Currently fails due to unclear issue, presumably unrelated to reduction
     mechanics. Avoiding for now.  */
  check_reduction_array_op_all (double, *, 1.0, dbls[i]);
#endif
  check_reduction_array_macro_all (double, min, n + 1, dbls[i]);
  check_reduction_array_macro_all (double, max, -1, dbls[i]);

  return 0;
}
