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

#include <stdio.h>

int upc_coll_init_flag = 0;

void
upc_coll_init ()
{
  if (upc_coll_init_flag)
    {
      printf ("Internal error: Multiple attempts to initialize ");
      printf ("\tthecollective library in the same thread.\n");
      exit (1);
    }

  // Set the flag so that this thread never initializes again.

  upc_coll_init_flag = 1;

// Nothing to initialize yet.

}
