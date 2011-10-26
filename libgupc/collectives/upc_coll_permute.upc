/*****************************************************************************/
/*                                                                           */
/*  Copyright (c) 2004, Michigan Technological University                    */
/*  All rights reserved.                                                     */
/*                                                                           */
/*  Redistribution and use in source and binary forms, with or without       */
/*  modification, are permitted provided that the following conditions       */
/*  are met:                                                                 */
/*                                                                           */
/*  * Redistributions of source code must retain the above copyright         */
/*  notice, this list of conditions and the following disclaimer.            */
/*  * Redistributions in binary form must reproduce the above                */
/*  copyright notice, this list of conditions and the following              */
/*  disclaimer in the documentation and/or other materials provided          */
/*  with the distribution.                                                   */
/*  * Neither the name of the Michigan Technological University              */
/*  nor the names of its contributors may be used to endorse or promote      */
/*  products derived from this software without specific prior written       */
/*  permission.                                                              */
/*                                                                           */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      */
/*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        */
/*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A  */
/*  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER */
/*  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, */
/*  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,      */
/*  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR       */
/*  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   */
/*  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     */
/*  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             */
/*                                                                           */
/*****************************************************************************/

#include <upc.h>
#include <upc_collective.h>
#include <upc_coll.h>

/*****************************************************************************/
/*                                                                           */
/*        UPC collective function library, reference implementation          */
/*                                                                           */
/*   Steve Seidel, Dept. of Computer Science, Michigan Technological Univ.   */
/*   steve@mtu.edu                                        March 1, 2004      */
/*                                                                           */
/*****************************************************************************/

void
upc_all_permute (shared void *dst,
		 shared const void *src,
		 shared const int *perm, size_t nbytes, upc_flag_t sync_mode)
{
#ifndef PULL
#ifndef PUSH
#define PULL TRUE
#endif
#endif

#ifdef PULL
  int i;
#endif

  if (!upc_coll_init_flag)
    upc_coll_init ();

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, perm, nbytes, sync_mode, 0, 0, 0, UPC_PERM);
#endif
  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))

    upc_barrier;

#ifdef PULL

  // Each dst thread finds its src thread and "pulls" the data from it.

  for (i = 0; i < THREADS; ++i)
    if (perm[i] == MYTHREAD)
      break;

  upc_memcpy ((shared char *) dst + MYTHREAD,
	      (shared char *) src + i, nbytes);

#endif

#ifdef PUSH

  // The src thread "pushes" its data to its dst thread.

  upc_memcpy ((shared char *) dst + perm[MYTHREAD],
	      (shared char *) src + MYTHREAD, nbytes);

#endif

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))

    upc_barrier;
}
