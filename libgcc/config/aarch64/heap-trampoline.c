/* Copyright The GNU Toolchain Authors. */

#include <unistd.h>
#include <sys/mman.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if __APPLE__
/* For pthread_jit_write_protect_np */
#include <pthread.h>
#endif

void *allocate_trampoline_page (void);
int get_trampolines_per_page (void);
struct tramp_ctrl_data *allocate_tramp_ctrl (struct tramp_ctrl_data *parent);
void *allocate_trampoline_page (void);

void __builtin_nested_func_ptr_created (void *chain, void *func, void **dst);
void __builtin_nested_func_ptr_deleted (void);

#if defined(__gnu_linux__)
static const uint32_t aarch64_trampoline_insns[] = {
  0xd503245f, /* hint    34 */
  0x580000b1, /* ldr     x17, .+20 */
  0x580000d2, /* ldr     x18, .+24 */
  0xd61f0220, /* br      x17 */
  0xd5033f9f, /* dsb     sy */
  0xd5033fdf /* isb */
};

#elif __APPLE__
static const uint32_t aarch64_trampoline_insns[] = {
  0xd503245f, /* hint    34 */
  0x580000b1, /* ldr     x17, .+20 */
  0x580000d0, /* ldr     x16, .+24 */
  0xd61f0220, /* br      x17 */
  0xd5033f9f, /* dsb     sy */
  0xd5033fdf /* isb */
};

#else
#error "Unsupported AArch64 platform for heap trampolines"
#endif

struct aarch64_trampoline {
  uint32_t insns[6];
  void *func_ptr;
  void *chain_ptr;
};

struct tramp_ctrl_data
{
  struct tramp_ctrl_data *prev;

  int free_trampolines;

  /* This will be pointing to an executable mmap'ed page.  */
  struct aarch64_trampoline *trampolines;
};

int
get_trampolines_per_page (void)
{
  return getpagesize() / sizeof(struct aarch64_trampoline);
}

static _Thread_local struct tramp_ctrl_data *tramp_ctrl_curr = NULL;

void *
allocate_trampoline_page (void)
{
  void *page;

#if defined(__gnu_linux__)
  page = mmap (0, getpagesize (), PROT_WRITE | PROT_EXEC,
	       MAP_ANON | MAP_PRIVATE, 0, 0);
#elif __APPLE__
  page = mmap (0, getpagesize (), PROT_WRITE | PROT_EXEC,
	       MAP_ANON | MAP_PRIVATE | MAP_JIT, 0, 0);
#else
  page = MAP_FAILED;
#endif

  return page;
}

struct tramp_ctrl_data *
allocate_tramp_ctrl (struct tramp_ctrl_data *parent)
{
  struct tramp_ctrl_data *p = malloc (sizeof (struct tramp_ctrl_data));
  if (p == NULL)
    return NULL;

  p->trampolines = allocate_trampoline_page ();

  if (p->trampolines == MAP_FAILED)
    return NULL;

  p->prev = parent;
  p->free_trampolines = get_trampolines_per_page();

  return p;
}

void
__builtin_nested_func_ptr_created (void *chain, void *func, void **dst)
{
  if (tramp_ctrl_curr == NULL)
    {
      tramp_ctrl_curr = allocate_tramp_ctrl (NULL);
      if (tramp_ctrl_curr == NULL)
	abort ();
    }

  if (tramp_ctrl_curr->free_trampolines == 0)
    {
      void *tramp_ctrl = allocate_tramp_ctrl (tramp_ctrl_curr);
      if (!tramp_ctrl)
	abort ();

      tramp_ctrl_curr = tramp_ctrl;
    }

  struct aarch64_trampoline *trampoline
    = &tramp_ctrl_curr->trampolines[get_trampolines_per_page ()
				    - tramp_ctrl_curr->free_trampolines];

#if __APPLE__
  /* Disable write protection for the MAP_JIT regions in this thread (see
     https://developer.apple.com/documentation/apple-silicon/porting-just-in-time-compilers-to-apple-silicon) */
  pthread_jit_write_protect_np (0);
#endif

  memcpy (trampoline->insns, aarch64_trampoline_insns,
	  sizeof(aarch64_trampoline_insns));
  trampoline->func_ptr = func;
  trampoline->chain_ptr = chain;

#if __APPLE__
  /* Re-enable write protection.  */
  pthread_jit_write_protect_np (1);
#endif

  tramp_ctrl_curr->free_trampolines -= 1;

  __builtin___clear_cache ((void *)trampoline->insns,
			   ((void *)trampoline->insns + sizeof(trampoline->insns)));

  *dst = &trampoline->insns;
}

void
__builtin_nested_func_ptr_deleted (void)
{
  if (tramp_ctrl_curr == NULL)
    abort ();

  tramp_ctrl_curr->free_trampolines += 1;

  if (tramp_ctrl_curr->free_trampolines == get_trampolines_per_page ())
    {
      if (tramp_ctrl_curr->prev == NULL)
	return;

      munmap (tramp_ctrl_curr->trampolines, getpagesize());
      struct tramp_ctrl_data *prev = tramp_ctrl_curr->prev;
      free (tramp_ctrl_curr);
      tramp_ctrl_curr = prev;
    }
}
