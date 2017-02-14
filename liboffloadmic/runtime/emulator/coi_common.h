/*
    Copyright (c) 2014-2016 Intel Corporation.  All Rights Reserved.

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

#ifndef COI_COMMON_H_INCLUDED
#define COI_COMMON_H_INCLUDED

#include <common/COIMacros_common.h>
#include <common/COIPerf_common.h>
#include <source/COIEngine_source.h>
#include <source/COIProcess_source.h>
#include <source/COIPipeline_source.h>
#include <source/COIBuffer_source.h>
#include <source/COIEvent_source.h>

#include <assert.h>
#include <dirent.h>
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>


/* Environment variable for path to 'target' files.  */
#define MIC_DIR_ENV		  "OFFLOAD_MIC_DIR"

/* Environment variable for engine index.  */
#define MIC_INDEX_ENV		  "OFFLOAD_MIC_INDEX"

/* Environment variable for target executable run command.  */
#define OFFLOAD_EMUL_RUN_ENV      "OFFLOAD_EMUL_RUN"

/* Environment variable for number of emulated devices.  */
#define OFFLOAD_EMUL_NUM_ENV	  "OFFLOAD_EMUL_NUM"


/* Path to engine directory.  */
#define ENGINE_PATH		  "/tmp/offload_XXXXXX"

/* Relative path to directory with pipes.  */
#define PIPES_PATH		  "/pipes"

/* Non-numerical part of host-to-target pipe file name.  */
#define PIPE_HOST2TGT_NAME	  PIPES_PATH "/host2tgt_"

/* Non-numerical part of target-to-host pipe file name.  */
#define PIPE_TGT2HOST_NAME	  PIPES_PATH "/tgt2host_"

/* Non-numerical part of shared memory file name.  */
#define SHM_NAME		  "/offload_shm_"


/* Use secure getenv if it's supported.  */
#ifdef HAVE_SECURE_GETENV
  #define getenv(x)	      secure_getenv(x)
#elif HAVE___SECURE_GETENV
  #define getenv(x)	      __secure_getenv(x)
#endif


/* Wrapper for malloc.  */
#define MALLOC(type, ptr, size)			\
{						\
  type p = (type) malloc (size);		\
  if (p == NULL)				\
    COIERROR ("Cannot allocate memory.");	\
  ptr = p;					\
}

/* Like MALLOC, but return NULL instead of COIRESULT.  */
#define MALLOCN(type, ptr, size)		\
{						\
  type p = (type) malloc (size);		\
  if (p == NULL)				\
    COIERRORN ("Cannot allocate memory.");	\
  ptr = p;					\
}

/* Wrapper for strdup.  */
#define STRDUP(ptr, str)			\
{						\
  char *p = strdup (str);			\
  if (p == NULL)				\
    COIERROR ("Cannot allocate memory.");	\
  ptr = p;					\
}

/* Wrapper for pipe reading.  */
#define READ(pipe, ptr, size)			\
{						\
  int s = (int) size;				\
  if (read (pipe, ptr, s) != s)			\
    COIERROR ("Cannot read from pipe.");	\
}

/* Like READ, but return NULL instead of COIRESULT.  */
#define READN(pipe, ptr, size)			\
{						\
  int s = (int) size;				\
  if (read (pipe, ptr, s) != s)			\
    COIERRORN ("Cannot read from pipe.");	\
}

/* Wrapper for pipe writing.  */
#define WRITE(pipe, ptr, size)			\
{						\
  int s = (int) size;				\
  if (write (pipe, ptr, s) != s)		\
    COIERROR ("Cannot write in pipe.");		\
}

/* Like WRITE, but return NULL instead of COIRESULT.  */
#define WRITEN(pipe, ptr, size)			\
{						\
  int s = (int) size;				\
  if (write (pipe, ptr, s) != s)		\
    COIERRORN ("Cannot write in pipe.");	\
}


/* Command codes enum.  */
typedef enum
{
  CMD_BUFFER_COPY,
  CMD_BUFFER_MAP,
  CMD_BUFFER_UNMAP,
  CMD_GET_FUNCTION_HANDLE,
  CMD_OPEN_LIBRARY,
  CMD_CLOSE_LIBRARY,
  CMD_PIPELINE_CREATE,
  CMD_PIPELINE_DESTROY,
  CMD_PIPELINE_RUN_FUNCTION,
  CMD_SHUTDOWN
} cmd_t;

#endif // COI_COMMON_H_INCLUDED
