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

							February 6, 2004

This is a reference implementation of the UPC V1.0 collective functions.
This implementation conforms to UPC V1.1.  The specification documents
can be found at http://www.upc.gwu/~upc/documentation.html.

1) Installation notes

The Makefile compiles the collective functions to collective.o and it
compiles three main programs, try_*.c, that exercise the collective
functions.  The Makefile has macros that control the number of threads,
whether argument checking is turned on, and whether certain collective
functions "push" or "pull".  Assuming it can find your upc compiler,
typing "make" will make collective.o with the "pull" versions of the
collective function for the dynamic threads environment and with
argument checking turned on.

2) Implementation notes

This implementation is written in UPC.  The 6 "relocalization"
functions (upc_all_broadcast(), upc_all_scatter(), etc.) are
implemented using memcpy.  Each function has a "push" and a "pull"
version.  "Push" means that the source thread(s) memcpy's the data to
the destination thread(s); "pull" means that the destination thread(s)
memcpy's the data from the source thread(s).  For example, in a "pull"
broadcast, each thread does one memcpy.  In the "push" version the src
thread does THREADS memcpys.  Since there is more explicit parallelism
in a "pull", that is the default.  For upc_all_reduceT(), "push" means
that each thread writes its local result to the shared dst argument;
"pull" means that the dst thread reads each thread's local result to
compute the final result.  No performance measurements have been made
yet to compare these alternatives.  upc_all_prefix_reduceT() and
upc_all_sort() do not distinguish "push" and "pull".

3) Algorithms

The relocalization functions are implemented as simple copies.  Tree-
based recursive algorithms, or other more sophisticated approaches,
are not used.  Similarly, in upc_all_reduceT() the local "sums"
are combined sequentially.  upc_all_prefix_reduceT() is also uses a linear
algorithm.  Performance of this function is likely to suffer if the src
and dst arrays "wrap" a lot, such as when their block size is small.
The same is true if the affinities of the src and dst are different.
The sort algorithm in upc_all_sort is the simplest possible and all
sorting is done on thread 0.  Users are advised to implement their
own parallel sort if performance is a concern.

4) Synchronization

MYSYNC and ALLSYNC (IN and OUT) are all implemented as barriers.
NOSYNC is implemented as no barrier, of course.  upc_all_reduceT()
has one internal barrier to synchronize access to the local "sums".
upc_prefix_reduceT() has a minimum of three barriers.  Two synchronize
access to the "sums" and the third synchronizes the freeing of a
dynamically allocated array.  Two additional barriers are incurred
for each "wrap" of the src array.

5) Initialization

An initialization function is provided.  The first time a collective
function is called the initialization function is called automatically.
However, the initialization function currently does not do anything
and it is safe to eliminate it.

6) Argument checking

If the _UPC_COLL_DEBUG macro is defined at compile time the
upc_coll_err() function will be invoked before each call to a
collective function.  This function checks the single-valuedness of
arguments to the collective function and it does a few other simple
sanity checks.  It uses two barriers to coordinate inter-thread
checking.  The results of the collective functions are not checked.

7) Memory usage

upc_all_reduceT(), upc_all_prefix_reduceT(), and upc_coll_err()
dynamically allocate a block of memory proportional to the number of
threads. That memory is freed on function exit.

8) Compilation environment

This set of functions can be compiled in both the dynamic and the
static (fixed number of threads) environment.

9) Limitations

upc_all_prefix_reduceT() violates the collectives spec because it
cannot handle the case of src and dst arguments that do not have
the same phase.  This condition is detected if argument checking is
turned on.

The PUSH version of upc_all_reduceT() has exhibited an instability
on the Alphaserver platform due to Elan's handling of locks.

10) Test programs

Three small test programs are provided.  try_all.c exercises all
of the functions at least once, except only one instance of each
of upc_all_reduceT() and upc_all_prefix_reduceT() is called.
try_reduce.c and try_prefix.c exercise upc_all_reduceT() and
upc_all_prefix_reduceT().  Compliance, correctness, and performance
tests are not provided.
