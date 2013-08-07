#include <string.h>
#include <assert.h>
#include <signal.h>
#include <stdio.h>

#include "vtv_malloc.h"

bool vtv_debug = false;

static void
handler(int sig, siginfo_t *si, void *unused)
{
  printf("Got SIGSEGV at address: 0x%lx\n",
         (long) si->si_addr);
  exit(1);
}

int memchk(const void * s, int c, size_t n)
{
  const char * p = (const char *)s;
  for (; p < ((char *)s + n); p++)
    if (*p != c)
      return 1;
  return 0;
}

int main()
{
  char * ptr;
  int size;

  /* Set up handler for SIGSEGV. In this test case, we should never hit any SIGSEGV */
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = handler;
  if (sigaction(SIGSEGV, &sa, NULL) == -1)
    assert(0);

  __vtv_malloc_init();

  size = 10;

  /* Verify simple allocation and deallocation */
  __vtv_malloc_unprotect();
  ptr = (char *)__vtv_malloc(size);
  __vtv_malloc_protect();
  __vtv_free(ptr);

  /* Verify writable after unprotect */
  __vtv_malloc_unprotect();
  ptr = (char *)__vtv_malloc(size);
  memset(ptr, 'a', size);
  __vtv_malloc_protect();
  __vtv_free(ptr);

  /* verify readable after protect */
  __vtv_malloc_unprotect();
  ptr = (char *)__vtv_malloc(size);
  memset(ptr, 'a', size);
  __vtv_malloc_protect();
  assert(ptr[size - 1] == 'a');
  __vtv_free(ptr);

  /* verify writable after protect, unprotect */
  __vtv_malloc_unprotect();
  ptr = (char *)__vtv_malloc(size);
  memset(ptr, 'a', size);
  __vtv_malloc_protect();
  __vtv_malloc_unprotect();
  memset(ptr, 'a', size);
  assert(ptr[size - 1] == 'a');
  __vtv_malloc_protect();
  assert(ptr[size - 1] == 'a');
  __vtv_free(ptr);

  /* Allocate a bunch of small objects.
     Make sure the alignment is correct.
     Verify data has not been corrupted.
     Try to modify the data to verify everything gets unprotected */
  {
    int s;
    for (s = 3; s < 28; s += 3)
    {
      size = s;
      {
        int i;
        #define ITERS 1000
        char * ptrs[ITERS];

        __vtv_malloc_unprotect();
        for (i = 0; i < ITERS; i++)
        {
          ptr = (char *)__vtv_malloc(size);
          assert(((long)ptr & VTV_ALIGNMENT_MASK) == 0);
          memset(ptr, (i & 127), size);
          assert(ptr[size - 1] == (i & 127));
          ptrs[i] = ptr;
        }
        __vtv_malloc_protect();

        __vtv_malloc_unprotect();
        for (i = 0; i < ITERS; i++)
        {
          if (memchk(ptrs[i], i & 127, size) != 0)
            assert(0);
          memset(ptrs[i], (i + 1) & 127, size);
          if (memchk(ptrs[i], (i + 1) & 127, size) != 0)
            assert(0);
          __vtv_free(ptrs[i]);
        }
        __vtv_malloc_protect();
      }
    }
  }

  /* Allocate a bunch of medium size objects.
     Make sure the alignment is correct.
     Verify data has not been corrupted.
     Try to modify the data to verify everything gets unprotected */
  {
    int s;
    for (s = 501; s < 2500; s += 91)
    {
      size = s;
      {
        int i;
        #define ITERS2 100
        char * ptrs[ITERS2];

        __vtv_malloc_unprotect();
        for (i = 0; i < ITERS2; i++)
        {

          ptr = (char *)__vtv_malloc(size);
          assert(((long)ptr & VTV_ALIGNMENT_MASK) == 0);
          memset(ptr, i & 127, size);
          assert(ptr[size - 1] == i & 127);
          ptrs[i] = ptr;
        }
        __vtv_malloc_protect();

        __vtv_malloc_unprotect();
        for (i = 0; i < ITERS2; i++)
        {
          if (memchk(ptrs[i], i & 127, size) != 0)
            assert(0);
          memset(ptrs[i], (i + 1) & 127, size);
          if (memchk(ptrs[i], (i + 1) & 127, size) != 0)
            assert(0);
          __vtv_free(ptrs[i]);
        }
        __vtv_malloc_protect();
      }
    }
  }

  /* Allocate a bunch of medium size objects. Make sure the alignment is correct */
  {
    int s;
    for (s = 3001; s < 15000; s += 307)
    {
      size = s;
      {
        int i;
        #define ITERS3 50
        char * ptrs[ITERS3];

        __vtv_malloc_unprotect();
        for (i = 0; i < ITERS3; i++)
        {
          ptr = (char *)__vtv_malloc(size);
          assert(((long)ptr & VTV_ALIGNMENT_MASK) == 0);
          memset(ptr, i & 127, size);
          assert(ptr[size - 1] == i & 127);
          ptrs[i] = ptr;
        }
        __vtv_malloc_protect();

        __vtv_malloc_unprotect();
        for (i = 0; i < ITERS3; i++)
        {
          if (memchk(ptrs[i], i & 127, size) != 0)
            assert(0);
          memset(ptrs[i], (i + 1) & 127, size);
          if (memchk(ptrs[i], (i + 1) & 127, size) != 0)
            assert(0);
          __vtv_free(ptrs[i]);
        }
        __vtv_malloc_protect();
      }
    }
  }

  return 0;
}
