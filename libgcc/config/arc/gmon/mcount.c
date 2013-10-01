/*-
 * Copyright (c) 1983, 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Copyright (C) 2007-2012 Free Software Foundation, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if !defined(lint) && !defined(KERNEL) && defined(LIBC_SCCS)
static char sccsid[] = "@(#)mcount.c	8.1 (Berkeley) 6/4/93";
#endif

#if 0
#include <unistd.h>
#include <sys/param.h>
#endif
#include <sys/gmon.h>

/* This file provides the machine-dependent definitions of the _MCOUNT_DECL
   and MCOUNT macros.  */
#include <machine-gmon.h>

#include <atomic.h>

/*
 * mcount is called on entry to each function compiled with the profiling
 * switch set.  _mcount(), which is declared in a machine-dependent way
 * with _MCOUNT_DECL, does the actual work and is either inlined into a
 * C routine or called by an assembly stub.  In any case, this magic is
 * taken care of by the MCOUNT definition in <machine/profile.h>.
 *
 * _mcount updates data structures that represent traversals of the
 * program's call graph edges.  frompc and selfpc are the return
 * address and function address that represents the given call graph edge.
 *
 * Note: the original BSD code used the same variable (frompcindex) for
 * both frompcindex and frompc.  Any reasonable, modern compiler will
 * perform this optimization.
 */
_MCOUNT_DECL(count_ptr, selfpc)	/* _mcount; may be static, inline, etc */
{
	register ARCINDEX *frompcindex;
	register struct tostruct *top, *prevtop;
	register struct gmonparam *p;
	register ARCINDEX toindex;

	/* Check for nested function trampoline.  */
	if (selfpc & 2)
	  selfpc = *(u_long *) (selfpc + 10);

	p = &_gmonparam;
	/*
	 * check that we are profiling
	 * and that we aren't recursively invoked.
	 */
#if 0
	if (catomic_compare_and_exchange_bool_acq (&p->state, GMON_PROF_BUSY,
						   GMON_PROF_ON))
	  return;
#elif defined (__ARC700__)
/* ??? This could temporarily lose the ERROR / OFF condition in a race,
   but doing an actual compare_and_exchange would be too costly.  It would
   be better if we had a semaphore independent of the 'sticky' state, but
   then we could run into ABI compatibility problems with the size of struct
   gmonparam.  */
	{
	  u_long old_state;

	  __asm ("ex %0,%1": "=r" (old_state), "+m" (p->state)
		 : "0" (GMON_PROF_BUSY));
	  if (old_state != GMON_PROF_ON)
	    {
	      switch (old_state)
		{
		case GMON_PROF_OFF:
		  __asm ("ex %0,%1": "+r" (old_state), "+m" (p->state));
		  if (old_state == GMON_PROF_BUSY
		      /* Switching off while we say we are busy while profiling
			 was actually already switched off is all right.  */
		      || old_state == GMON_PROF_OFF)
		    break;
		  /* It is not clear if we should allow switching on
		     profiling at this point, and how to handle further races.
		     For now, record an error in this case.  */
		  /* Fall through.  */
		default: /* We expect here only GMON_PROF_ERROR.  */
		  p->state = GMON_PROF_ERROR;
		  break;
		case GMON_PROF_BUSY: break;
		}
	      return;
	    }
	}
#else /* ??? No semaphore primitives available.  */
	if (p->state != GMON_PROF_ON)
	  return;
	p->state = GMON_PROF_BUSY;
#endif

	frompcindex = count_ptr;
	toindex = *frompcindex;
	if (toindex == 0) {
		/*
		 *	first time traversing this arc
		 */
		toindex = ++p->tos[0].link;
		if (toindex >= (ARCINDEX) p->tolimit)
			/* halt further profiling */
			goto overflow;

		*frompcindex = toindex;
		top = &p->tos[toindex];
		top->selfpc = selfpc;
		top->count = 1;
		top->link = 0;
		goto done;
	}
	top = &p->tos[toindex];
	if (top->selfpc == selfpc) {
		/*
		 * arc at front of chain; usual case.
		 */
		top->count++;
		goto done;
	}
	/*
	 * have to go looking down chain for it.
	 * top points to what we are looking at,
	 * prevtop points to previous top.
	 * we know it is not at the head of the chain.
	 */
	for (; /* goto done */; ) {
		if (top->link == 0) {
			/*
			 * top is end of the chain and none of the chain
			 * had top->selfpc == selfpc.
			 * so we allocate a new tostruct
			 * and link it to the head of the chain.
			 */
			toindex = ++p->tos[0].link;
			if (toindex >= (ARCINDEX) p->tolimit)
				goto overflow;

			top = &p->tos[toindex];
			top->selfpc = selfpc;
			top->count = 1;
			top->link = *frompcindex;
			*frompcindex = toindex;
			goto done;
		}
		/*
		 * otherwise, check the next arc on the chain.
		 */
		prevtop = top;
		top = &p->tos[top->link];
		if (top->selfpc == selfpc) {
			/*
			 * there it is.
			 * increment its count
			 * move it to the head of the chain.
			 */
			top->count++;
			toindex = prevtop->link;
			prevtop->link = top->link;
			top->link = *frompcindex;
			*frompcindex = toindex;
			goto done;
		}

	}
done:
	p->state = GMON_PROF_ON;
	return;
overflow:
	p->state = GMON_PROF_ERROR;
	return;
}

/*
 * Actual definition of mcount function.  Defined in <machine/profile.h>,
 * which is included by <sys/gmon.h>.
 */
MCOUNT
