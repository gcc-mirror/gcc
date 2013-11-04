/* metacall_impl.c                  -*-C-*-
 *
 *************************************************************************
 *
 *  @copyright
 *  Copyright (C) 2009-2013, Intel Corporation
 *  All rights reserved.
 *  
 *  @copyright
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *  
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *    * Neither the name of Intel Corporation nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *  
 *  @copyright
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 *  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 *  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 *  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 **************************************************************************/

#include "metacall_impl.h"

NOINLINE
CILK_API_VOID
__cilkrts_metacall(unsigned int tool, unsigned int code, void *data)
{
#ifdef ENABLE_NOTIFY_ZC_INTRINSIC
    // The metacall type, code and data are packed together into a single
    // struct which will be interpreted by the tool. This function is the
    // one and only use of a "cilkscreen_metacall" annotation
    metacall_data_t d = { tool, code, data };

    // Note that Inspector uses probe mode, and is implementing the metacall
    // interface to force the runtime to run with a single worker.  So
    // __cilkrts_metacall must use __notify_intrinsic instead of
    // __notify_zc_intrinsic
    __notify_intrinsic("cilkscreen_metacall", &d);
#endif // ENABLE_NOTIFY_ZC_INTRINSIC
}

int __cilkrts_running_under_sequential_ptool(void)
{
    static int running_under_sequential_ptool = -1;
    volatile char c = ~0;

    // If we haven't been called before, see if we're running under Cilkscreen
    // or Cilkview
    if (-1 == running_under_sequential_ptool)
    {
        // metacall #2 writes 0 in C if we are running under
        // a p-tools that requires serial execution, and is a 
        // no-op otherwise
        //
        // Note that removing the volatile is required to prevent the compiler
        // from assuming that the value has not changed
        __cilkrts_metacall(METACALL_TOOL_SYSTEM,
                           HYPER_ZERO_IF_SEQUENTIAL_PTOOL, (void *)&c);

        running_under_sequential_ptool = (0 == c);
    }

    return running_under_sequential_ptool;
}

/*
 * __cilkrts_cilkscreen_establish_c_stack
 *
 * Notify Cilkscreen of the extent of the stack
 */

void __cilkrts_cilkscreen_establish_c_stack(char *begin, char *end)
{
    char *limits[2] = {begin, end};

    __cilkrts_metacall(METACALL_TOOL_SYSTEM, HYPER_ESTABLISH_C_STACK, limits);
}

#ifdef WORKSPAN // Workspan stuff - remove when we're sure what we can drop

void __cilkview_workspan_start(void) {
  __cilkrts_metacall(HYPER_WORKSPAN_START, 0);
}

void __cilkview_workspan_stop(void) {
  __cilkrts_metacall(HYPER_WORKSPAN_STOP, 0);
}

void __cilkview_workspan_dump(const char *str) {
  __cilkrts_metacall(HYPER_WORKSPAN_DUMP, (void*)str);
}


void __cilkview_workspan_reset(void) {
  __cilkrts_metacall(HYPER_WORKSPAN_RESET, 0);
}


void __cilkview_use_default_grain(void) {
    __cilkrts_metacall(HYPER_USE_DEFAULT_GRAIN, 0);
}

void __cilkview_get_workspan_data(unsigned long long *values, int size)
{
    void *data[2];

    /* reset counters to zero in case we are not running under
       a p-tool */

    values[0] = 0;

    data[0] = (void*) values;
    data[1] = (void*) &size;
     __cilkrts_metacall(HYPER_WORKSPAN_QUERY, &data);
}

void __cilkview_workspan_connected (int *flag) {
  *flag = 0;
  __cilkrts_metacall(HYPER_WORKSPAN_CONNECTED, (void *)flag);
}

void __cilkview_workspan_suspend() {
  __cilkrts_metacall(HYPER_WORKSPAN_SUSPEND, 0);
}

void __cilkview_workspan_resume() {
  __cilkrts_metacall(HYPER_WORKSPAN_RESUME, 0);
}

/* depreciated interfaces */
void __cilkometer_workspan_start(void) {
  __cilkrts_metacall(HYPER_WORKSPAN_START, 0);
}

void __cilkometer_workspan_stop(void) {
  __cilkrts_metacall(HYPER_WORKSPAN_STOP, 0);
}

void __cilkometer_workspan_dump(const char *str) {
  __cilkrts_metacall(HYPER_WORKSPAN_DUMP, (void*)str);
}


void __cilkometer_workspan_reset(void) {
  __cilkrts_metacall(HYPER_WORKSPAN_RESET, 0);
}

#endif // WORKSPAN

/* End metacall_impl.c */
