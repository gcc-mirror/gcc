#include <stddef.h>
#include <stdint.h>
#include <omp.h>

#define MIN(x,y) ((x) < (y) ? x : y)

enum { N = 524288 + 8 };

static void
init_val (int8_t *ptr, int val, size_t count)
{
  #pragma omp target is_device_ptr(ptr) firstprivate(val, count)
  __builtin_memset (ptr, val, count);
}

static void
check_val (int8_t *ptr, int val, size_t count)
{
  if (count == 0)
    return;
  #pragma omp target is_device_ptr(ptr) firstprivate(val, count)
  for (size_t i = 0; i < count; i++)
    if (ptr[i] != val) __builtin_abort ();
}

static void
test_it (void *ptr, int lshift, size_t count)
{
  if (N < count + lshift) __builtin_abort ();
  if (lshift >= 4) __builtin_abort ();
  ptr += lshift;

  init_val (ptr, 'z', MIN (count + 32, N - lshift));

  omp_target_memset (ptr, '1', count, omp_get_default_device());

  check_val (ptr, '1', count);
  check_val (ptr + count, 'z', MIN (32, N - lshift - count));
}


int main()
{
  size_t size;
  void *ptr = omp_target_alloc (N + 3, omp_get_default_device());
  ptr += (4 - (uintptr_t) ptr % 4) % 4;
  if ((uintptr_t) ptr % 4 != 0) __builtin_abort ();

  test_it (ptr, 0, 1);
  test_it (ptr, 3, 1);
  test_it (ptr, 0, 4);
  test_it (ptr, 3, 4);
  test_it (ptr, 0, 5);
  test_it (ptr, 3, 5);
  test_it (ptr, 0, 6);
  test_it (ptr, 3, 6);

  for (int i = 1; i <= 9; i++)
    {
      switch (i)
	{
	case 1: size = 16; break; // = 2^4 bytes
	case 2: size = 32; break; // = 2^5 bytes
	case 3: size = 64; break; // = 2^7 bytes
	case 4: size = 128; break; // = 2^7 bytes
	case 5: size = 256; break; // = 2^8 bytes
	case 6: size = 512; break; // = 2^9 bytes
	case 7: size = 65536; break; // = 2^16 bytes
	case 8: size = 262144; break; // = 2^18 bytes
	case 9: size = 524288; break; // = 2^20 bytes
	default: __builtin_abort ();
	}
      test_it (ptr, 0, size);
      test_it (ptr, 3, size);
      test_it (ptr, 0, size + 1);
      test_it (ptr, 3, size + 1);
      test_it (ptr, 3, size + 2);
    }
  omp_target_free (ptr, omp_get_default_device());
}
