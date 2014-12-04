/*
    Copyright (c) 2014 Intel Corporation.  All Rights Reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of Intel Corporation nor the names of its
        contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "myo_service.h"

#include "myo_version_asm.h"


extern "C"
{

MYOACCESSAPI MyoError
SYMBOL_VERSION (myoAcquire, 1) ()
{
  MYOTRACE ("myoAcquire");

  assert (false);

  return MYO_ERROR;
}


MYOACCESSAPI MyoError
SYMBOL_VERSION (myoRelease, 1) ()
{
  MYOTRACE ("myoRelease");

  assert (false);

  return MYO_ERROR;
}


MYOACCESSAPI void
SYMBOL_VERSION (myoSharedAlignedFree, 1) (void *ptr)
{
  MYOTRACE ("myoSharedAlignedFree");

  assert (false);
}


MYOACCESSAPI void*
SYMBOL_VERSION (myoSharedAlignedMalloc, 1) (size_t size,
					    size_t alignment)
{
  MYOTRACE ("myoSharedAlignedMalloc");

  assert (false);

  return 0;
}


MYOACCESSAPI void
SYMBOL_VERSION (myoSharedFree, 1) (void *ptr)
{
  MYOTRACE ("myoSharedFree");
 
  assert (false);
}


MYOACCESSAPI void*
SYMBOL_VERSION (myoSharedMalloc, 1) (size_t size)
{
  MYOTRACE ("myoSharedMalloc");

  assert (false);

  return 0;
}


MYOACCESSAPI MyoError
SYMBOL_VERSION (myoiLibInit, 1) (void *args,
				 void *init_func)
{
  MYOTRACE ("myoiLibInit");

  assert (false);

  return MYO_ERROR;
}


MYOACCESSAPI void
SYMBOL_VERSION (myoiLibFini, 1) ()
{
  MYOTRACE ("myoiLibFini");

  assert (false);
}


MyoError
SYMBOL_VERSION (myoiMicVarTableRegister, 1) (void *table,
					     int num)
{
  MYOTRACE ("myoiMicVarTableRegister");

  assert (false);

  return MYO_ERROR;
}


MYOACCESSAPI MyoError
SYMBOL_VERSION (myoiRemoteFuncRegister, 1) (MyoiRemoteFuncType type,
					    const char *name)
{
  MYOTRACE ("myoiRemoteFuncRegister");

  /* Looks like we have nothing to do here.  */

  return MYO_SUCCESS;
}


MyoError
SYMBOL_VERSION (myoiTargetFptrTableRegister, 1) (void *table,
						 int num,
						 int ordered)
{
  MYOTRACE ("myoiTargetFptrTableRegister");

  assert (false);

  return MYO_ERROR;
}

} // extern "C"

