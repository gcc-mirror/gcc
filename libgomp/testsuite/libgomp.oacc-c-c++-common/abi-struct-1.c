/* Inspired by 'gcc.target/nvptx/abi-struct-arg.c', 'gcc.target/nvptx/abi-struct-ret.c'.  */

/* See also '../libgomp.c-c++-common/target-abi-struct-1.c'.  */

/* To exercise PR119835 (if optimizations enabled): disable inlining, so that
   GIMPLE passes still see the functions that return aggregate types.  */
#pragma GCC optimize "-fno-inline"

typedef struct {} empty;  /* See 'gcc/doc/extend.texi', "Empty Structures".  */
typedef struct {char a;} schar;
typedef struct {short a;} sshort;
typedef struct {int a;} sint;
typedef struct {long long a;} slonglong;
typedef struct {int a, b[12];} sint_13;

#pragma omp declare target

#define M(T) ({T t; t.a = sizeof t; t;})

static __SIZE_TYPE__ empty_a;
#pragma acc declare create(empty_a)
#pragma acc routine
static empty rempty(void)
{
  return ({empty t; empty_a = sizeof t; t;});
}

#pragma acc routine
static schar rschar(void)
{
  return M(schar);
}

#pragma acc routine
static sshort rsshort(void)
{
  return M(sshort);
}

#pragma acc routine
static sint rsint(void)
{
  return M(sint);
}

#pragma acc routine
static slonglong rslonglong(void)
{
  return M(slonglong);
}

#pragma acc routine
static sint_13 rsint_13(void)
{
  return M(sint_13);
}

#pragma acc routine
static void aempty(empty empty)
{
  (void) empty;

  __SIZE_TYPE__ empty_a_exp;
#ifndef __cplusplus
  empty_a_exp = 0;
#else
  empty_a_exp = sizeof (char);
#endif
  if (empty_a != empty_a_exp)
    __builtin_abort();
}

#pragma acc routine
static void aschar(schar schar)
{
  if (schar.a != sizeof (char))
    __builtin_abort();
}

#pragma acc routine
static void asshort(sshort sshort)
{
  if (sshort.a != sizeof (short))
    __builtin_abort();
}

#pragma acc routine
static void asint(sint sint)
{
  if (sint.a != sizeof (int))
    __builtin_abort();
}

#pragma acc routine
static void aslonglong(slonglong slonglong)
{
  if (slonglong.a != sizeof (long long))
    __builtin_abort();
}

#pragma acc routine
static void asint_13(sint_13 sint_13)
{
  if (sint_13.a != (sizeof (int) * 13))
    __builtin_abort();
}

#pragma omp end declare target

int main()
{
#pragma omp target
#pragma acc serial
  /* { dg-bogus {using 'vector_length \(32\)', ignoring 1} {} { target openacc_nvidia_accel_selected xfail *-*-* } .-1 } */
  {
    aempty(rempty());
    aschar(rschar());
    asshort(rsshort());
    asint(rsint());
    aslonglong(rslonglong());
    asint_13(rsint_13());
  }

  return 0;
}
