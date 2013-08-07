#include <string.h>
#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <setjmp.h>

#include "vtv_malloc.h"

volatile static int signal_count = 0;

sigjmp_buf before_segv;

bool vtv_debug = false;

static void
handler(int sig, siginfo_t *si, void *unused)
{
  /*
  printf("Got SIGSEGV at address: 0x%lx\n",
         (long) si->si_addr);
  */

  signal_count++;
  /* You are not supposed to longjmp out of a signal handler but it seems
     to work for this test case and it simplifies it */
  siglongjmp(before_segv, 1);
  /* exit(1); */
}

/* Try to modify the memory pointed by "s" but dont actually change the values.
   Assumes and verifies the memory to be modified is mprotected */
void mempoke(void * s, size_t n)
{
  volatile char * p = (char *)s;
  int ret;

  signal_count = 0;
  ret = sigsetjmp(before_segv, 1);
  if (ret == 0)
    p[0] = p[0];
  /* printf("after first setjmp ret=%d\n", ret); */
  assert(ret == 1 && signal_count == 1);

  ret = sigsetjmp(before_segv, 1);
  if (ret == 0)
    p[n - 1] = p[n - 1];
  /* printf("after second setjmp ret=%d\n", ret); */
  assert(ret == 1 && signal_count == 2);
}

int main()
{
  char * ptr;
  int size;

  /* Set up handler for SIGSEGV. */
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = handler;
  if (sigaction(SIGSEGV, &sa, NULL) == -1)
    assert(0);

  __vtv_malloc_init();

  size = 10;

  /* Verify not writable after unprotect */
  __vtv_malloc_unprotect();
  ptr = (char *)__vtv_malloc(size);
  memset(ptr, 'a', size);
  __vtv_malloc_protect();
  mempoke(ptr, size);
  __vtv_free(ptr);

  /* verify not-writable after protect, unprotect */
  __vtv_malloc_unprotect();
  ptr = (char *)__vtv_malloc(size);
  memset(ptr, 'a', size);
  __vtv_malloc_protect();
  __vtv_malloc_unprotect();
  memset(ptr, 'a', size);
  assert(ptr[size - 1] == 'a');
  __vtv_malloc_protect();
  assert(ptr[size - 1] == 'a');
  mempoke(ptr,size);
  __vtv_free(ptr);

  /* Allocate a bunch of small objects.
     Make sure the alignment is correct.
     Verify data has not been corrupted.
     Make sure the data cannot modified */
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

        for (i = 0; i < ITERS; i++)
          mempoke(ptrs[i], size);

        __vtv_malloc_unprotect();
        for (i = 0; i < ITERS; i++)
          __vtv_free(ptrs[i]);
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

        for (i = 0; i < ITERS2; i++)
          mempoke(ptrs[i], size);

        __vtv_malloc_unprotect();
        for (i = 0; i < ITERS2; i++)
          __vtv_free(ptrs[i]);
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

        for (i = 0; i < ITERS3; i++)
          mempoke(ptrs[i], size);

        __vtv_malloc_unprotect();
        for (i = 0; i < ITERS3; i++)
          __vtv_free(ptrs[i]);
        __vtv_malloc_protect();
      }
    }
  }

  return 0;
}
