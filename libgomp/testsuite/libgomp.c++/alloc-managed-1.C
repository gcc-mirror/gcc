// { dg-do run }
// { dg-require-effective-target omp_managedmem }
// { dg-additional-options -foffload-options=amdgcn-amdhsa=-mxnack=on { target offload_target_amdgcn_with_xnack } }

// Check that the ompx::allocator::gnu_managed_mem allocator can allocate
// Managed Memory, and that host and target can see the data, at the same
// address, without a mapping.

#include <omp.h>
#include <cstdint>
#include <memory>

int
main ()
{
  using Allocator = ompx::allocator::gnu_managed_mem<int>;
  using Traits = std::allocator_traits<Allocator>;

  Allocator alloc;
  int *a = Traits::allocate (alloc, 1);
  if (!a)
    __builtin_abort ();

  Traits::construct (alloc, a, 42);
  std::uintptr_t a_p = reinterpret_cast<std::uintptr_t>(a);

  #pragma omp target is_device_ptr(a)
    {
      if (*a != 42 || a_p != reinterpret_cast<std::uintptr_t>(a))
	__builtin_abort ();
    }

  Traits::destroy (alloc, a);
  Traits::deallocate (alloc, a, 1);
  return 0;
}
