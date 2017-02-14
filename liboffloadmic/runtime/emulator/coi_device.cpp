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

#include "coi_device.h"

#include "coi_version_asm.h"

#define CYCLE_FREQUENCY     1000000000


static uint32_t engine_index;
static char *engine_dir;


extern "C"
{

COIRESULT
SYMBOL_VERSION (COIBufferAddRef, 1) (void *ptr)
{
  COITRACE ("COIBufferAddRef");

  /* Looks like we have nothing to do here.  */

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIBufferReleaseRef, 1) (void *ptr)
{
  COITRACE ("COIBufferReleaseRef");

  /* Looks like we have nothing to do here.  */

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIEngineGetIndex, 1) (COI_ISA_TYPE *type,
				       uint32_t *index)
{
  COITRACE ("COIEngineGetIndex");

  /* type is not used in liboffloadmic.  */
  *index = engine_index;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIPipelineStartExecutingRunFunctions, 1) ()
{
  COITRACE ("COIPipelineStartExecutingRunFunctions");

  /* Looks like we have nothing to do here.  */

  return COI_SUCCESS;
}


/* The start routine for the COI pipeline thread.  */

static void *
pipeline_thread_routine (void *in_pipeline_num)
{
  uint32_t pipeline_num = *(uint32_t *) in_pipeline_num;
  free (in_pipeline_num);

  /* Open pipes.  */
  char *pipe_host2tgt_path, *pipe_tgt2host_path;
  MALLOCN (char *, pipe_host2tgt_path,
	  strlen (engine_dir) + sizeof (PIPE_HOST2TGT_NAME "0000000000"));
  MALLOCN (char *, pipe_tgt2host_path,
	  strlen (engine_dir) + sizeof (PIPE_TGT2HOST_NAME "0000000000"));
  sprintf (pipe_host2tgt_path, "%s" PIPE_HOST2TGT_NAME "%010d", engine_dir,
	   pipeline_num);
  sprintf (pipe_tgt2host_path, "%s" PIPE_TGT2HOST_NAME "%010d", engine_dir,
	   pipeline_num);
  int pipe_host2tgt = open (pipe_host2tgt_path, O_CLOEXEC | O_RDONLY);
  if (pipe_host2tgt < 0)
    COIERRORN ("Cannot open host-to-target pipe.");
  int pipe_tgt2host = open (pipe_tgt2host_path, O_CLOEXEC | O_WRONLY);
  if (pipe_tgt2host < 0)
    COIERRORN ("Cannot open target-to-host pipe.");

  free (pipe_host2tgt_path);
  free (pipe_tgt2host_path);

  while (1)
    {
      /* Read and execute command.  */
      cmd_t cmd = CMD_PIPELINE_DESTROY;
      int cmd_len = read (pipe_host2tgt, &cmd, sizeof (cmd_t));
      if (cmd_len != sizeof (cmd_t) && cmd_len != 0)
	COIERRORN ("Cannot read from pipe.");

      if (cmd == CMD_PIPELINE_DESTROY)
	break;
      else if (cmd == CMD_PIPELINE_RUN_FUNCTION)
	{
	  /* Receive data from host.  */
	  void (*func) (uint32_t, void **, uint64_t *, void *, uint16_t, void *,
			uint16_t);
	  uint32_t buffer_count;
	  READN (pipe_host2tgt, &func, sizeof (void *));
	  READN (pipe_host2tgt, &buffer_count, sizeof (uint32_t));
	  void **buffers;
	  uint64_t *buffers_len;
	  MALLOCN (void **, buffers, buffer_count * sizeof (void *));
	  MALLOCN (uint64_t *, buffers_len, buffer_count * sizeof (uint64_t));
	  for (uint32_t i = 0; i < buffer_count; i++)
	    {
	      READN (pipe_host2tgt, &buffers_len[i], sizeof (uint64_t));
	      READN (pipe_host2tgt, &buffers[i], sizeof (void *));
	    }
	  uint16_t misc_data_len;
	  READN (pipe_host2tgt, &misc_data_len, sizeof (uint16_t));
	  void *misc_data = NULL;
	  if (misc_data_len > 0)
	    {
	      MALLOCN (void *, misc_data, misc_data_len);
	      READN (pipe_host2tgt, misc_data, misc_data_len);
	    }
	  uint16_t return_data_len;
	  READN (pipe_host2tgt, &return_data_len, sizeof (uint16_t));
	  void *return_data;
	  if (return_data_len > 0)
	    MALLOCN (void *, return_data, return_data_len);

	  /* Run function.  */
	  func (buffer_count, buffers, buffers_len, misc_data,
		misc_data_len, return_data, return_data_len);

	  /* Send data to host if any or just send notification.  */
	  WRITEN (pipe_tgt2host, return_data_len > 0 ? return_data : &cmd,
		  return_data_len > 0 ? return_data_len : sizeof (cmd_t));

	  /* Clean up.  */
	  free (buffers);
	  free (buffers_len);
	  if (misc_data_len > 0)
	    free (misc_data);
	  if (return_data_len > 0)
	    free (return_data);
	}
      else
	COIERRORN ("Unrecognizable command from host.");
    }

  /* Close pipes.  */
  if (close (pipe_host2tgt) < 0)
    COIERRORN ("Cannot close host-to-target pipe.");
  if (close (pipe_tgt2host) < 0)
    COIERRORN ("Cannot close target-to-host pipe.");

  return NULL;
}


COIRESULT
SYMBOL_VERSION (COIProcessWaitForShutdown, 1) ()
{
  COITRACE ("COIProcessWaitForShutdown");

  engine_dir = getenv (MIC_DIR_ENV);
  char *mic_index = getenv (MIC_INDEX_ENV);
  assert (engine_dir != NULL && mic_index != NULL);

  /* Get engine index.  */
  engine_index = atoi (mic_index);

  /* Open main pipes.  */
  char *pipe_host2tgt_path, *pipe_tgt2host_path;
  MALLOC (char *, pipe_host2tgt_path,
	  strlen (engine_dir) + sizeof (PIPE_HOST2TGT_NAME "mainpipe"));
  MALLOC (char *, pipe_tgt2host_path,
	  strlen (engine_dir) + sizeof (PIPE_TGT2HOST_NAME "mainpipe"));
  sprintf (pipe_host2tgt_path, "%s" PIPE_HOST2TGT_NAME "mainpipe", engine_dir);
  sprintf (pipe_tgt2host_path, "%s" PIPE_TGT2HOST_NAME "mainpipe", engine_dir);
  int pipe_host2tgt = open (pipe_host2tgt_path, O_CLOEXEC | O_RDONLY);
  if (pipe_host2tgt < 0)
    COIERROR ("Cannot open host-to-target main pipe.");
  int pipe_tgt2host = open (pipe_tgt2host_path, O_CLOEXEC | O_WRONLY);
  if (pipe_tgt2host < 0)
    COIERROR ("Cannot open target-to-host main pipe.");

  /* Clean up.  */
  free (pipe_host2tgt_path);
  free (pipe_tgt2host_path);

  /* Handler.  */
  while (1)
    {
      /* Read and execute command.  */
      cmd_t cmd = CMD_SHUTDOWN;
      int cmd_len = read (pipe_host2tgt, &cmd, sizeof (cmd_t));
      if (cmd_len != sizeof (cmd_t) && cmd_len != 0)
	COIERROR ("Cannot read from main pipe.");

      switch (cmd)
	{
	case CMD_BUFFER_COPY:
	  {
	    uint64_t len;
	    void *dest, *source;

	    /* Receive data from host.  */
	    READ (pipe_host2tgt, &dest, sizeof (void *));
	    READ (pipe_host2tgt, &source, sizeof (void *));
	    READ (pipe_host2tgt, &len, sizeof (uint64_t));

	    /* Copy.  */
	    memcpy (dest, source, len);

	    /* Notify host about completion.  */
	    WRITE (pipe_tgt2host, &cmd, sizeof (cmd_t));

	    break;
	  }
	case CMD_BUFFER_MAP:
	  {
	    char *name;
	    size_t len;
	    uint64_t buffer_len;
	    void *buffer;

	    /* Receive data from host.  */
	    READ (pipe_host2tgt, &len, sizeof (size_t));
	    MALLOC (char *, name, len);
	    READ (pipe_host2tgt, name, len);
	    READ (pipe_host2tgt, &buffer_len, sizeof (uint64_t));

	    /* Open shared memory.  */
	    int fd = shm_open (name, O_CLOEXEC | O_RDWR, S_IRUSR | S_IWUSR);
	    if (fd < 0)
	      COIERROR ("Cannot open shared memory.");

	    /* Map shared memory.  */
	    buffer = mmap (NULL, buffer_len, PROT_READ | PROT_WRITE,
			   MAP_SHARED, fd, 0);
	    if (buffer == NULL)
	      COIERROR ("Cannot map shared memory.");

	    /* Send data to host.  */
	    WRITE (pipe_tgt2host, &fd, sizeof (int));
	    WRITE (pipe_tgt2host, &buffer, sizeof (void *));

	    /* Clean up.  */
	    free (name);

	    break;
	  }
	case CMD_BUFFER_UNMAP:
	  {
	    int fd;
	    uint64_t buffer_len;
	    void *buffer;

	    /* Receive data from host.  */
	    READ (pipe_host2tgt, &fd, sizeof (int));
	    READ (pipe_host2tgt, &buffer, sizeof (void *));
	    READ (pipe_host2tgt, &buffer_len, sizeof (uint64_t));

	    /* Unmap buffer.  */
	    if (munmap (buffer, buffer_len) < 0)
	      COIERROR ("Cannot unmap shared memory.");

	    /* Close shared memory.  */
	    if (close (fd) < 0)
	      COIERROR ("Cannot close shared memory file.");

	    /* Notify host about completion.  */
	    WRITE (pipe_tgt2host, &cmd, sizeof (cmd_t));

	    break;
	  }
	case CMD_GET_FUNCTION_HANDLE:
	  {
	    char *name;
	    size_t len;

	    /* Receive data from host.  */
	    READ (pipe_host2tgt, &len, sizeof (size_t));
	    MALLOC (char *, name, len);
	    READ (pipe_host2tgt, name, len);

	    /* Find function.  */
	    void *ptr = dlsym (RTLD_DEFAULT, name);
	    if (ptr == NULL)
	      COIERROR ("Cannot find symbol %s.", name);

	    /* Send data to host.  */
	    WRITE (pipe_tgt2host, &ptr, sizeof (void *));

	    /* Clean up.  */
	    free (name);

	    break;
	  }
	case CMD_OPEN_LIBRARY:
	  {
	    char *lib_path;
	    size_t len;

	    /* Receive data from host.  */
	    READ (pipe_host2tgt, &len, sizeof (size_t));
	    MALLOC (char *, lib_path, len);
	    READ (pipe_host2tgt, lib_path, len);

	    /* Open library.  */
	    void *handle = dlopen (lib_path, RTLD_LAZY | RTLD_GLOBAL);
	    if (handle == NULL)
	      COIERROR ("Cannot load %s: %s", lib_path, dlerror ());

	    /* Send data to host.  */
	    WRITE (pipe_tgt2host, &handle, sizeof (void *));

	    /* Clean up.  */
	    free (lib_path);

	    break;
	  }
	case CMD_CLOSE_LIBRARY:
	  {
	    /* Receive data from host.  */
	    void *handle;
	    READ (pipe_host2tgt, &handle, sizeof (void *));

	    dlclose (handle);

	    break;
	  }
	case CMD_PIPELINE_CREATE:
	  {
	    /* Receive data from host.  */
	    uint32_t *pipeline_num;
	    MALLOC (uint32_t *, pipeline_num, sizeof (uint32_t));
	    READ (pipe_host2tgt, pipeline_num, sizeof (*pipeline_num));

	    /* Create a new thread for the pipeline.  */
	    pthread_t thread;
	    if (pthread_create (&thread, NULL, pipeline_thread_routine,
				pipeline_num))
	      COIERROR ("Cannot create new thread.");
	    break;
	  }
	case CMD_SHUTDOWN:
	  if (close (pipe_host2tgt) < 0)
	    COIERROR ("Cannot close host-to-target main pipe.");
	  if (close (pipe_tgt2host) < 0)
	    COIERROR ("Cannot close target-to-host main pipe.");
	  return COI_SUCCESS;
	default:
	  COIERROR ("Unrecognizable command from host.");
	}
    }

  return COI_ERROR;
}



uint64_t
SYMBOL_VERSION (COIPerfGetCycleFrequency, 1) ()
{
  COITRACE ("COIPerfGetCycleFrequency");

  return (uint64_t) CYCLE_FREQUENCY;
}

} // extern "C"

