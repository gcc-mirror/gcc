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

#include "coi_device.h"

#include "coi_version_asm.h"

#define CYCLE_FREQUENCY     1000000000


static uint32_t engine_index;


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

  /* type is not used in liboffload.  */
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


COIRESULT
SYMBOL_VERSION (COIProcessWaitForShutdown, 1) ()
{
  COITRACE ("COIProcessWaitForShutdown");

  char *mic_dir = getenv (MIC_DIR_ENV);
  char *mic_index = getenv (MIC_INDEX_ENV);
  char *pipe_host_path, *pipe_target_path;
  int pipe_host, pipe_target;
  int cmd_len;
  pid_t ppid = getppid ();
  cmd_t cmd;

  assert (mic_dir != NULL && mic_index != NULL);

  /* Get engine index.  */
  engine_index = atoi (mic_index);

  /* Open pipes.  */
  MALLOC (char *, pipe_host_path,
	  strlen (PIPE_HOST_PATH) + strlen (mic_dir) + 1);
  MALLOC (char *, pipe_target_path,
	  strlen (PIPE_TARGET_PATH) + strlen (mic_dir) + 1);
  sprintf (pipe_host_path, "%s"PIPE_HOST_PATH, mic_dir);
  sprintf (pipe_target_path, "%s"PIPE_TARGET_PATH, mic_dir);
  pipe_host = open (pipe_host_path, O_CLOEXEC | O_WRONLY);
  if (pipe_host < 0)
    COIERROR ("Cannot open target-to-host pipe.");
  pipe_target = open (pipe_target_path, O_CLOEXEC | O_RDONLY);
  if (pipe_target < 0)
    COIERROR ("Cannot open host-to-target pipe.");

  /* Clean up.  */
  free (pipe_host_path);
  free (pipe_target_path);

  /* Handler.  */
  while (1)
    {
      /* Read and execute command.  */
      cmd = CMD_SHUTDOWN;
      cmd_len = read (pipe_target, &cmd, sizeof (cmd_t));
      if (cmd_len != sizeof (cmd_t) && cmd_len != 0)
	COIERROR ("Cannot read from pipe.");

      switch (cmd)
	{
	case CMD_BUFFER_COPY:
	  {
	    uint64_t len;
	    void *dest, *source;

	    /* Receive data from host.  */
	    READ (pipe_target, &dest, sizeof (void *));
	    READ (pipe_target, &source, sizeof (void *));
	    READ (pipe_target, &len, sizeof (uint64_t));

	    /* Copy.  */
	    memcpy (dest, source, len);

	    /* Notify host about completion.  */
	    WRITE (pipe_host, &cmd, sizeof (cmd_t));

	    break;
	  }
	case CMD_BUFFER_MAP:
	  {
	    char *name;
	    int fd;
	    size_t len;
	    uint64_t buffer_len;
	    void *buffer;

	    /* Receive data from host.  */
	    READ (pipe_target, &len, sizeof (size_t));
	    MALLOC (char *, name, len);
	    READ (pipe_target, name, len);
	    READ (pipe_target, &buffer_len, sizeof (uint64_t));

	    /* Open shared memory.  */
	    fd = shm_open (name, O_CLOEXEC | O_RDWR, S_IRUSR | S_IWUSR);
	    if (fd < 0)
	      COIERROR ("Cannot open shared memory.");

	    /* Map shared memory.  */
	    buffer = mmap (NULL, buffer_len, PROT_READ | PROT_WRITE,
			   MAP_SHARED, fd, 0);
	    if (buffer == NULL)
	      COIERROR ("Cannot map shared memory.");

	    /* Send data to host.  */
	    WRITE (pipe_host, &fd, sizeof (int));
	    WRITE (pipe_host, &buffer, sizeof (void *));

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
	    READ (pipe_target, &fd, sizeof (int));
	    READ (pipe_target, &buffer, sizeof (void *));
	    READ (pipe_target, &buffer_len, sizeof (uint64_t));

	    /* Unmap buffer.  */
	    if (munmap (buffer, buffer_len) < 0)
	      COIERROR ("Cannot unmap shared memory.");

	    /* Close shared memory.  */
	    if (close (fd) < 0)
	      COIERROR ("Cannot close shared memory file.");

	    /* Notify host about completion.  */
	    WRITE (pipe_host, &cmd, sizeof (cmd_t));

	    break;
	  }
	case CMD_GET_FUNCTION_HANDLE:
	  {
	    char *name;
	    size_t len;
	    void *ptr;

	    /* Receive data from host.  */
	    READ (pipe_target, &len, sizeof (size_t));
	    MALLOC (char *, name, len);
	    READ (pipe_target, name, len);

	    /* Find function.  */
	    ptr = dlsym (RTLD_DEFAULT, name);
	    if (ptr == NULL)
	      COIERROR ("Cannot find symbol %s.", name);

	    /* Send data to host.  */
	    WRITE (pipe_host, &ptr, sizeof (void *));

	    /* Clean up.  */
	    free (name);

	    break;
	  }
	case CMD_OPEN_LIBRARY:
	  {
	    char *lib_path;
	    size_t len;

	    /* Receive data from host.  */
	    READ (pipe_target, &len, sizeof (size_t));
	    MALLOC (char *, lib_path, len);
	    READ (pipe_target, lib_path, len);

	    /* Open library.  */
	    if (dlopen (lib_path, RTLD_LAZY | RTLD_GLOBAL) == 0)
	      COIERROR ("Cannot load %s: %s", lib_path, dlerror ());

	    /* Clean up.  */
	    free (lib_path);

	    break;
	  }
	case CMD_RUN_FUNCTION:
	  {
	    uint16_t misc_data_len, return_data_len;
	    uint32_t buffer_count, i;
	    uint64_t *buffers_len, size;
	    void *ptr;
	    void **buffers, *misc_data, *return_data;

	    void (*func) (uint32_t, void **, uint64_t *,
			  void *, uint16_t, void*, uint16_t);

	    /* Receive data from host.  */
	    READ (pipe_target, &func, sizeof (void *));
	    READ (pipe_target, &buffer_count, sizeof (uint32_t));
	    MALLOC (void **, buffers, buffer_count * sizeof (void *));
	    MALLOC (uint64_t *, buffers_len, buffer_count * sizeof (uint64_t));

	    for (i = 0; i < buffer_count; i++)
	      {
		READ (pipe_target, &(buffers_len[i]), sizeof (uint64_t));
		READ (pipe_target, &(buffers[i]), sizeof (void *));
	      }
	    READ (pipe_target, &misc_data_len, sizeof (uint16_t));
	    if (misc_data_len > 0)
	      {
		MALLOC (void *, misc_data, misc_data_len);
		READ (pipe_target, misc_data, misc_data_len);
	      }
	    READ (pipe_target, &return_data_len, sizeof (uint16_t));
	    if (return_data_len > 0)
	      MALLOC (void *, return_data, return_data_len);

	    /* Run function.  */
	    func (buffer_count, buffers, buffers_len, misc_data,
		  misc_data_len, return_data, return_data_len);

	    /* Send data to host if any or just send notification.  */
	    WRITE (pipe_host, return_data_len > 0 ? return_data : &cmd,
		   return_data_len > 0 ? return_data_len : sizeof (cmd_t));

	    /* Clean up.  */
	    free (buffers);
	    free (buffers_len);
	    if (misc_data_len > 0)
	      free (misc_data);
	    if (return_data_len > 0)
	      free (return_data);

	    break;
	  }
	case CMD_SHUTDOWN:
	  if (close (pipe_host) < 0)
	    COIERROR ("Cannot close target-to-host pipe.");
	  if (close (pipe_target) < 0)
	    COIERROR ("Cannot close host-to-target pipe.");
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

