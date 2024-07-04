/* Copyright (C) 2013-2024 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "libgomp.h"
#include "libgomp-nvptx.h"  /* For struct rev_offload + GOMP_REV_OFFLOAD_VAR. */
#include <limits.h>

extern int __gomp_team_num __attribute__((shared));
extern volatile struct gomp_offload_icvs GOMP_ADDITIONAL_ICVS;
volatile struct rev_offload *GOMP_REV_OFFLOAD_VAR;

bool
GOMP_teams4 (unsigned int num_teams_lower, unsigned int num_teams_upper,
	     unsigned int thread_limit, bool first)
{
  unsigned int num_blocks, block_id;
  asm ("mov.u32 %0, %%nctaid.x;" : "=r" (num_blocks));
  if (!first)
    {
      unsigned int team_num;
      if (num_blocks > gomp_num_teams_var)
	return false;
      team_num = __gomp_team_num;
      if (team_num > gomp_num_teams_var - num_blocks)
	return false;
      __gomp_team_num = team_num + num_blocks;
      return true;
    }
  if (thread_limit)
    {
      struct gomp_task_icv *icv = gomp_icv (true);
      icv->thread_limit_var
	= thread_limit > INT_MAX ? UINT_MAX : thread_limit;
    }
  if (!num_teams_upper)
    num_teams_upper = ((GOMP_ADDITIONAL_ICVS.nteams > 0
			&& num_blocks > GOMP_ADDITIONAL_ICVS.nteams)
		       ? GOMP_ADDITIONAL_ICVS.nteams : num_blocks);
  else if (num_blocks < num_teams_lower)
    num_teams_upper = num_teams_lower;
  else if (num_blocks < num_teams_upper)
    num_teams_upper = num_blocks;
  asm ("mov.u32 %0, %%ctaid.x;" : "=r" (block_id));
  if (block_id >= num_teams_upper)
    return false;
  __gomp_team_num = block_id;
  gomp_num_teams_var = num_teams_upper - 1;
  return true;
}

int
omp_pause_resource (omp_pause_resource_t kind, int device_num)
{
  (void) kind;
  (void) device_num;
  return -1;
}

int
omp_pause_resource_all (omp_pause_resource_t kind)
{
  (void) kind;
  return -1;
}

ialias (omp_pause_resource)
ialias (omp_pause_resource_all)

void
GOMP_target_ext (int device, void (*fn) (void *), size_t mapnum,
		 void **hostaddrs, size_t *sizes, unsigned short *kinds,
		 unsigned int flags, void **depend, void **args)
{
  static int lock = 0;  /* == gomp_mutex_t lock; gomp_mutex_init (&lock); */
  (void) flags;
  (void) depend;
  (void) args;

  if (device != GOMP_DEVICE_HOST_FALLBACK
      || fn == NULL
      || GOMP_REV_OFFLOAD_VAR == NULL)
    return;

  gomp_mutex_lock (&lock);

  GOMP_REV_OFFLOAD_VAR->mapnum = mapnum;
  GOMP_REV_OFFLOAD_VAR->addrs = (uint64_t) hostaddrs;
  GOMP_REV_OFFLOAD_VAR->sizes = (uint64_t) sizes;
  GOMP_REV_OFFLOAD_VAR->kinds = (uint64_t) kinds;
  GOMP_REV_OFFLOAD_VAR->dev_num = GOMP_ADDITIONAL_ICVS.device_num;

  /* Set 'fn' to trigger processing on the host; wait for completion,
     which is flagged by setting 'fn' back to 0 on the host.  */
  uint64_t addr_struct_fn = (uint64_t) &GOMP_REV_OFFLOAD_VAR->fn;
#if __PTX_SM__ >= 700
  asm volatile ("st.global.release.sys.u64 [%0], %1;"
		: : "r"(addr_struct_fn), "r" (fn) : "memory");
#else
  __sync_synchronize ();  /* membar.sys */
  asm volatile ("st.volatile.global.u64 [%0], %1;"
		: : "r"(addr_struct_fn), "r" (fn) : "memory");
#endif

#if __PTX_SM__ >= 700
  uint64_t fn2;
  do
    {
      asm volatile ("ld.acquire.sys.global.u64 %0, [%1];"
		    : "=r" (fn2) : "r" (addr_struct_fn) : "memory");
    }
  while (fn2 != 0);
#else
  /* ld.global.u64 %r64,[__gomp_rev_offload_var];
     ld.u64 %r36,[%r64];
     membar.sys;  */
  while (__atomic_load_n (&GOMP_REV_OFFLOAD_VAR->fn, __ATOMIC_ACQUIRE) != 0)
    ;  /* spin  */
#endif

  gomp_mutex_unlock (&lock);
}

void
GOMP_target_data_ext (int device, size_t mapnum, void **hostaddrs,
		      size_t *sizes, unsigned short *kinds)
{
  (void) device;
  (void) mapnum;
  (void) hostaddrs;
  (void) sizes;
  (void) kinds;
  __builtin_unreachable ();
}

void
GOMP_target_end_data (void)
{
  __builtin_unreachable ();
}

void
GOMP_target_update_ext (int device, size_t mapnum, void **hostaddrs,
			size_t *sizes, unsigned short *kinds,
			unsigned int flags, void **depend)
{
  (void) device;
  (void) mapnum;
  (void) hostaddrs;
  (void) sizes;
  (void) kinds;
  (void) flags;
  (void) depend;
  __builtin_unreachable ();
}

void
GOMP_target_enter_exit_data (int device, size_t mapnum, void **hostaddrs,
			     size_t *sizes, unsigned short *kinds,
			     unsigned int flags, void **depend)
{
  (void) device;
  (void) mapnum;
  (void) hostaddrs;
  (void) sizes;
  (void) kinds;
  (void) flags;
  (void) depend;
  __builtin_unreachable ();
}
