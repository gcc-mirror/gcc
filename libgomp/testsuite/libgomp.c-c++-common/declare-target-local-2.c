#include <assert.h>

#ifdef USE_SELF_MAPS
#pragma omp self_maps
#endif

int var[2] = { 5, 6 };
#pragma omp declare target local(var)

int
get_var_inc (void)
{
  var[0]++;
  return var[0];
}

__UINTPTR_TYPE__
get_addr (void)
{
  return (__UINTPTR_TYPE__) &var[0];
}

int main (void)
{
  __UINTPTR_TYPE__ addr_tgt, addr_fn;
  bool is_initial_dev;
  int val;
  var[0] = 7;

  #pragma omp target map(from: val, is_initial_dev, addr_tgt, addr_fn)
  {
    is_initial_dev = __builtin_omp_is_initial_device ();
    addr_tgt = (__UINTPTR_TYPE__) &var[0];
    addr_fn = get_addr ();

    var[0] += 5;
    val = get_var_inc ();
  }

  if (is_initial_dev)
    {
      assert (val == var[0]);
      assert (val == 13);
      assert (&var[0] == (void*) addr_fn);
    }
  else
    {
      assert (val == 11);
      assert (var[0] == 7);
      assert (&var[0] != (void*) addr_fn);
    }

  assert (addr_tgt == addr_fn);

  return 0;
}
