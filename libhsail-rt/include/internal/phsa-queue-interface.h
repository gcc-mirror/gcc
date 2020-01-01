/* phsa_queue_interface.h -- Definition for a minimalistic generic in-memory
   representation of a user mode queue to be used with the phsa/gccbrig
   implementation.

   Copyright (C) 2015-2020 Free Software Foundation, Inc.
   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#ifndef PHSA_QUEUE_INTERFACE_H
#define PHSA_QUEUE_INTERFACE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include "hsa.h"

typedef __attribute__ ((aligned (64))) struct phsa_queue_s
{
  /* An HSA Architectured Queue object.  Must be in the beginning
     of the struct to enable direct pointer casting between hsa_queue_
     and phsa_queue_t.  */
  hsa_queue_t hsa_queue;

  volatile uint64_t write_index;
  volatile uint64_t read_index;

  /* True if global mem addresses are 64b.  */
  uint64_t is_ptr64 : 1;

} phsa_queue_t;

#ifdef __cplusplus
}
#endif

#endif
