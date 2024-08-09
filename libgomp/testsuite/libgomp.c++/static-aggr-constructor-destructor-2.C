// { dg-do run }
// { dg-additional-options "-fdump-tree-gimple -fdump-tree-optimized" }
// { dg-additional-options -foffload-options=-fdump-tree-optimized { target { offload_target_nvptx || offload_target_amdgcn } } }

// { dg-final { scan-tree-dump-times "omp_is_initial_device" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "_GLOBAL__off_I_v1" 1 "gimple" } }
// { dg-final { scan-tree-dump-times "__omp_target_static_init_and_destruction" 2 "gimple" } }
// { dg-final { scan-tree-dump-times "__attribute__\\(\\(\[^\n\r]*omp declare target nohost" 2 "gimple" } }

// { dg-final { scan-tree-dump-not "omp_is_initial_device" "optimized" } }
// { dg-final { scan-tree-dump-not "__omp_target_static_init_and_destruction" "optimized" } }

// (A) No offloading configured: The symbols aren't present
//     Caveat: They are present with -foffload=disable - or offloading
//     configured but none of the optional offload packages/binaries installed.
//     But the 'offload_target_any' check cannot distinguish those
// { dg-final { scan-tree-dump-not "void _GLOBAL__off_I_v1" "optimized" { target { ! offload_target_any } } } }
// { dg-final { scan-tree-dump-not "__attribute__\\(\\(\[^\n\r]*omp declare target nohost" "optimized" { target { ! offload_target_any } } } }

// (B) With offload configured (and compiling for an offload target)
//     the symbols are present (missed optimization). Hence: FIXME.
// { dg-final { scan-tree-dump-times "void _GLOBAL__off_I_v1" 1 "optimized" { target offload_target_any } } }
// { dg-final { scan-tree-dump-times "__attribute__\\(\\(\[^\n\r]*omp declare target nohost" 1 "optimized" { target offload_target_any } } }


// { dg-final { only_for_offload_target amdgcn-amdhsa scan-offload-tree-dump-not "omp_initial_device;" "optimized" { target offload_target_amdgcn } } }
// { dg-final { only_for_offload_target amdgcn-amdhsa scan-offload-tree-dump "v1\\._x = 5;" "optimized" { target offload_target_amdgcn } } }
// { dg-final { only_for_offload_target nvptx-none scan-offload-tree-dump-not "omp_initial_device;" "optimized" { target offload_target_nvptx } } }
// { dg-final { only_for_offload_target nvptx-none scan-offload-tree-dump "v1\\._x = 5;" "optimized" { target offload_target_nvptx } } }


#include <cassert>

#pragma omp declare target

template<typename T>
struct str {
  str(T x) : _x(x) { }
  T add(str o) { return _x + o._x; }
  T _x;
};

str<long> v1(5);

#pragma omp end declare target

int main()
{
  long res = -1;
  str<long> v2(2);

#pragma omp target map(from:res)
  {
    res = v1.add(v2);
  }

  assert (res == 7);

  return 0;
}
