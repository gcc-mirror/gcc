/* Trampoline support for MeP
   Copyright (C) 2004, 2007 Free Software Foundation, Inc.
   Contributed by Red Hat Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.
  
This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
  
Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
  7a0a		ldc $10,$pc
  c0ae000a	lw $0,10($10)
  caae000e	lw $10,14($10)
  10ae		jmp $10
  00000000	static chain
  00000000	function address
*/

static inline int
cache_config_register(void) {
  int rv;
  asm ("ldc\t%0, $ccfg" : "=r" (rv));
  return rv;
}

#define ICACHE_SIZE ((cache_config_register() >> 16) & 0x7f)
#define DCACHE_SIZE (cache_config_register() & 0x7f)

#define ICACHE_DATA_BASE 0x00300000
#define ICACHE_TAG_BASE  0x00310000
#define DCACHE_DATA_BASE 0x00320000
#define DCACHE_TAG_BASE  0x00330000

static inline void
flush_dcache (int addr)
{
  asm volatile ("cache\t0, (%0)" : : "r" (addr));
}

void
__mep_trampoline_helper (unsigned long *tramp,
			 int function_address,
			 int static_chain);

void
__mep_trampoline_helper (unsigned long *tramp,
			 int function_address,
			 int static_chain)
{
  int dsize, isize;

#ifdef __LITTLE_ENDIAN__
  tramp[0] = 0xc0ae7a0a;
  tramp[1] = 0xcaae000a;
  tramp[2] = 0x10ae000e;
#else
  tramp[0] = 0x7a0ac0ae;
  tramp[1] = 0x000acaae;
  tramp[2] = 0x000e10ae;
#endif
  tramp[3] = static_chain;
  tramp[4] = function_address;

  dsize = DCACHE_SIZE;
  isize = ICACHE_SIZE;

  if (dsize)
    {
      flush_dcache ((int)tramp);
      flush_dcache ((int)tramp+16);
    }

  if (isize)
    {
      int imask = (isize * 1024) - 1;
      int tmask = ~imask;
      unsigned int i;
      volatile unsigned int *tags;

      imask &= 0xffe0;

      for (i=(unsigned int)tramp; i<(unsigned int)tramp+20; i+=16)
	{
	  tags = (unsigned int *)(ICACHE_TAG_BASE + (i & imask));
	  if ((*tags & tmask) == (i & tmask))
	    *tags &= ~1;
	}
    }
}
