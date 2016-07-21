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

#include <set>
#include <map>
#include <queue>

#include "coi_host.h"

#include "coi_version_asm.h"

#define CYCLE_FREQUENCY     1000000000

enum buffer_t
{
  BUFFER_NORMAL,
  BUFFER_MEMORY
};

struct Engine
{
  COI_ISA_TYPE type;
  uint32_t index;
  char *dir;
};

struct Function
{
  void *ptr;
  uint32_t num_buffers;
  uint64_t *bufs_size;
  void * *bufs_data_target;
  uint16_t misc_data_len;
  void *misc_data;
  uint16_t return_value_len;
  void *return_value;
  COIEVENT completion_event;
};

struct Callback
{
  COI_EVENT_CALLBACK ptr;
  const void *data;
};

struct Process
{
  pid_t pid;
  int pipe_host2tgt;
  int pipe_tgt2host;
  Engine *engine;
  void **functions;
};

struct Pipeline
{
  pthread_t thread;
  bool destroy;
  bool is_destroyed;
  char *pipe_host2tgt_path;
  char *pipe_tgt2host_path;
  int pipe_host2tgt;
  int pipe_tgt2host;
  std::queue<Function> queue;
  Process *process;
};

struct Buffer
{
  buffer_t type;
  char *name;
  int fd;
  int fd_target;
  uint64_t size;
  void *data;
  void *data_target;
  Process *process;
};


/* Environment variables.  */
extern char **environ;

/* List of directories for removing on exit.  */
static char **tmp_dirs;
static unsigned tmp_dirs_num;

/* Number of emulated MIC engines.  */
static long num_engines;

/* Number of the last COI pipeline.  */
static uint32_t max_pipeline_num;

/* Set of undestroyed pipelines.  */
static std::set<Pipeline *> pipelines;

/* Number of the last COI event, the event #0 is always signalled.  */
static uint64_t max_event_num = 1;

/* Set of created COI events, which are not signalled.  */
static std::set<uint64_t> non_signalled_events;

/* Set of COI events, which encountered errors.  */
static std::map<uint64_t, COIRESULT> errored_events;

/* Set of registered callbacks, indexed by event number.  */
static std::map<uint64_t, Callback> callbacks;

/* Mutex to sync parallel execution.  */
static pthread_mutex_t mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;


static COIRESULT
read_long_env (const char *env_name, long *var, long var_default)
{
  char *str = getenv (env_name);
  char *s;

  if (!str || *str == '\0')
    *var = var_default;
  else
    {
      errno = 0;
      *var = strtol (str, &s, 0);
      if (errno != 0 || s == str || *s != '\0')
	COIERROR ("Variable %s has invalid value.", env_name);
    }

  return COI_SUCCESS;
}

__attribute__((constructor))
static void
init ()
{
  if (read_long_env (OFFLOAD_EMUL_NUM_ENV, &num_engines, 1) == COI_ERROR)
    exit (0);
}


/* Helper function for directory removing.  */
static COIRESULT remove_directory (char *path)
{
  char *file;
  struct dirent *entry;
  struct stat statfile;
  DIR *dir = opendir (path);
  if (dir == NULL)
    COIERROR ("Cannot open directory %s.", dir);

  while (entry = readdir (dir))
    {
      if (!strcmp (entry->d_name, ".") || !strcmp (entry->d_name, ".."))
	continue;

      MALLOC (char *, file, strlen (path) + strlen (entry->d_name) + 2);
      sprintf (file, "%s/%s", path, entry->d_name);

      if (stat (file, &statfile) < 0)
	COIERROR ("Cannot retrieve information about file %s.", file);

      if (S_ISDIR (statfile.st_mode))
	{
	  if (remove_directory (file) == COI_ERROR)
	    return COI_ERROR;
	}
      else
	{
	  if (unlink (file) < 0)
	    COIERROR ("Cannot unlink file %s.", file);
	}

      free (file);
    }

  if (closedir (dir) < 0)
    COIERROR ("Cannot close directory %s.", path);
  if (rmdir (path) < 0)
    COIERROR ("Cannot remove directory %s.", path);

  return COI_SUCCESS;
}

__attribute__((destructor))
static void
cleanup ()
{
  for (unsigned i = 0; i < tmp_dirs_num; i++)
    {
      remove_directory (tmp_dirs[i]);
      free (tmp_dirs[i]);
    }
  free (tmp_dirs);
}

static COIRESULT
start_critical_section ()
{
  if (pthread_mutex_lock (&mutex) != 0)
    COIERROR ("Cannot lock mutex.");
  return COI_SUCCESS;
}

static COIRESULT
finish_critical_section ()
{
  if (pthread_mutex_unlock (&mutex) != 0)
    COIERROR ("Cannot unlock mutex.");
  return COI_SUCCESS;
}

static bool
pipeline_is_destroyed (const Pipeline *pipeline)
{
  start_critical_section ();
  bool res = pipeline->is_destroyed;
  finish_critical_section ();
  return res;
}

static void
maybe_invoke_callback (const COIEVENT event, const COIRESULT result)
{
  std::map<uint64_t, Callback>::iterator cb = callbacks.find (event.opaque[0]);

  if (cb != callbacks.end ())
    {
      Callback callback = cb->second;
      callback.ptr (event, result, callback.data);
      callbacks.erase (cb);
    }
}

static void
signal_event (const COIEVENT event, const COIRESULT result)
{
  if (result != COI_SUCCESS)
    errored_events.insert (std::pair <uint64_t, COIRESULT> (event.opaque[0],
							    result));
  non_signalled_events.erase (event.opaque[0]);

  maybe_invoke_callback (event, result);
}

static COIRESULT
get_event_result (const COIEVENT event)
{
  COIRESULT res = COI_SUCCESS;

  std::map<uint64_t, COIRESULT>::iterator ee
    = errored_events.find (event.opaque[0]);

  if (ee != errored_events.end ())
    res = ee->second;

  return res;
}


extern "C"
{

COIRESULT
SYMBOL_VERSION (COIBufferCopy, 1) (COIBUFFER in_DestBuffer,
				   COIBUFFER in_SourceBuffer,
				   uint64_t in_DestOffset,
				   uint64_t in_SourceOffset,
				   uint64_t in_Length,
				   COI_COPY_TYPE in_Type,
				   uint32_t in_NumDependencies,
				   const COIEVENT *in_pDependencies,  // Ignored
				   COIEVENT *out_pCompletion)
{
  COITRACE ("COIBufferCopy");

  /* Features of liboffloadmic.  */
  assert (in_DestBuffer != NULL);
  assert (in_SourceBuffer != NULL);
  assert (in_Type == COI_COPY_UNSPECIFIED);
  assert (in_NumDependencies == 0);

  /* Convert input arguments.  */
  Buffer *dest = (Buffer *) in_DestBuffer;
  Buffer *source = (Buffer *) in_SourceBuffer;

  start_critical_section ();

  /* Map buffers if needed.  */
  if (dest->data == 0 && dest->type == BUFFER_NORMAL)
    if (COIBufferMap (in_DestBuffer, 0, dest->size, (COI_MAP_TYPE) 0,
		      0, 0, 0, 0, 0) == COI_ERROR)
      return COI_ERROR;
  if (source->data == 0 && source->type == BUFFER_NORMAL)
    if (COIBufferMap (in_SourceBuffer, 0, source->size, (COI_MAP_TYPE) 0,
		      0, 0, 0, 0, 0) == COI_ERROR)
      return COI_ERROR;

  /* Copy data.  */
  if (source->data != 0 && dest->data != 0)
    memcpy ((void *) ((uintptr_t) dest->data + in_DestOffset),
	    (void *) ((uintptr_t) source->data + in_SourceOffset), in_Length);
  else
    {
      assert (dest->process == source->process);

      Buffer *buffer;
      cmd_t cmd = CMD_BUFFER_COPY;

      /* Create intermediary buffer.  */
      if (COIBufferCreate (in_Length, COI_BUFFER_NORMAL, 0, 0, 1,
			   (COIPROCESS*) &dest->process,
			   (COIBUFFER *) &buffer) == COI_ERROR)
	return COI_ERROR;

      int pipe_host2tgt = dest->process->pipe_host2tgt;
      int pipe_tgt2host = dest->process->pipe_tgt2host;

      /* Copy from source to intermediary buffer.  */
      if (source->data == 0)
	{
	  assert (source->data_target != 0);

	  /* Send data to target.  */
	  WRITE (pipe_host2tgt, &cmd, sizeof (cmd_t));
	  WRITE (pipe_host2tgt, &buffer->data_target, sizeof (void *));
	  WRITE (pipe_host2tgt, &source->data_target, sizeof (void *));
	  WRITE (pipe_host2tgt, &buffer->size, sizeof (uint64_t));

	  /* Receive data from target.  */
	  READ (pipe_tgt2host, &cmd, sizeof (cmd_t));
	}
      else
	{
	  if (COIBufferCopy ((COIBUFFER) buffer, in_SourceBuffer, 0,
			     in_SourceOffset, in_Length, in_Type, 0, 0, 0)
	      == COI_ERROR)
	    return COI_ERROR;
	}

      /* Copy from intermediary buffer to dest.  */
      if (dest->data == 0)
	{
	  assert (dest->data_target != 0);

	  /* Send data to target.  */
	  WRITE (pipe_host2tgt, &cmd, sizeof (cmd_t));
	  WRITE (pipe_host2tgt, &dest->data_target, sizeof (void *));
	  WRITE (pipe_host2tgt, &buffer->data_target, sizeof (void *));
	  WRITE (pipe_host2tgt, &buffer->size, sizeof (uint64_t));

	  /* Receive data from target.  */
	  READ (pipe_tgt2host, &cmd, sizeof (cmd_t));
	}
      else
	{
	  if (COIBufferCopy (in_DestBuffer, (COIBUFFER) buffer, in_DestOffset,
			     0, in_Length, in_Type, 0, 0, 0) == COI_ERROR)
	    return COI_ERROR;
	}

      /* Unmap on target and destroy intermediary buffer.  */
      if (COIBufferDestroy ((COIBUFFER) buffer) == COI_ERROR)
	return COI_ERROR;
    }

  /* Unmap buffers if needed.  */
  if (dest->type == BUFFER_NORMAL)
    if (COIBufferUnmap ((COIMAPINSTANCE) dest, 0, 0, 0) == COI_ERROR)
      return COI_ERROR;
  if (source->type == BUFFER_NORMAL)
    if (COIBufferUnmap ((COIMAPINSTANCE) source, 0, 0, 0) == COI_ERROR)
      return COI_ERROR;

  finish_critical_section ();

  if (out_pCompletion)
    out_pCompletion->opaque[0] = 0;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIBufferCreate, 1) (uint64_t in_Size,
				     COI_BUFFER_TYPE in_Type,
				     uint32_t in_Flags,
				     const void *in_pInitData,
				     uint32_t in_NumProcesses,
				     const COIPROCESS *in_pProcesses,
				     COIBUFFER *out_pBuffer)
{
  COITRACE ("COIBufferCreate");

  char *shm_name;
  int shm_fd;
  const int ullong_max_len = 20;

  /* Features of liboffloadmic.  */
  assert (in_Type == COI_BUFFER_NORMAL || in_Type == COI_BUFFER_OPENCL);
  assert ((in_Flags & COI_SINK_MEMORY) == 0);
  assert ((in_Flags & COI_SAME_ADDRESS_SINKS) == 0);
  assert ((in_Flags & COI_SAME_ADDRESS_SINKS_AND_SOURCE) == 0);
  assert (in_pInitData == NULL);
  assert (in_NumProcesses == 1);
  assert (in_pProcesses != NULL);
  assert (out_pBuffer != NULL);

  /* Create shared memory with an unique name.  */
  MALLOC (char *, shm_name, strlen (SHM_NAME) + ullong_max_len + 1);
  for (unsigned long long i = 0; i >= 0; i++)
    {
      sprintf (shm_name, SHM_NAME "%lu", i);
      shm_fd = shm_open (shm_name, O_CLOEXEC | O_CREAT | O_EXCL | O_RDWR,
			 S_IRUSR | S_IWUSR);
      if (shm_fd > 0)
	break;
    }
  if (ftruncate (shm_fd, in_Size) < 0)
    COIERROR ("Cannot truncate shared memory file.");

  /* Create buffer.  */
  Buffer *buf = new Buffer;
  buf->data = 0;
  buf->fd = shm_fd;
  buf->process = (Process *) in_pProcesses[0];
  buf->size = in_Size;
  buf->type = BUFFER_NORMAL;
  STRDUP (buf->name, shm_name);

  /* Map buffer on target.  */
  size_t len = strlen (buf->name) + 1;

  start_critical_section ();

  /* Send data to target.  */
  const cmd_t cmd = CMD_BUFFER_MAP;
  int pipe_host2tgt = buf->process->pipe_host2tgt;
  WRITE (pipe_host2tgt, &cmd, sizeof (cmd_t));
  WRITE (pipe_host2tgt, &len, sizeof (size_t));
  WRITE (pipe_host2tgt, buf->name, len);
  WRITE (pipe_host2tgt, &buf->size, sizeof (uint64_t));

  /* Receive data from target.  */
  int pipe_tgt2host = buf->process->pipe_tgt2host;
  READ (pipe_tgt2host, &buf->fd_target, sizeof (int));
  READ (pipe_tgt2host, &buf->data_target, sizeof (void *));

  finish_critical_section ();

  /* Prepare output arguments.  */
  *out_pBuffer = (COIBUFFER) buf;

  /* Clean up.  */
  free (shm_name);

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIBufferCreateFromMemory, 1) (uint64_t in_Size,
					       COI_BUFFER_TYPE in_Type,
					       uint32_t in_Flags,
					       void *in_Memory,
					       uint32_t in_NumProcesses,
					       const COIPROCESS *in_pProcesses,
					       COIBUFFER *out_pBuffer)
{
  COITRACE ("COIBufferCreateFromMemory");

  /* Features of liboffloadmic.  */
  assert (in_Type == COI_BUFFER_NORMAL);
  assert ((in_Flags & COI_SAME_ADDRESS_SINKS) == 0);
  assert ((in_Flags & COI_SAME_ADDRESS_SINKS_AND_SOURCE) == 0);
  assert (in_NumProcesses == 1);
  assert (in_pProcesses != NULL);
  assert (out_pBuffer != NULL);

  /* Create buffer.  */
  Buffer *buf = new Buffer;
  buf->data = (in_Flags & COI_SINK_MEMORY) == 0 ? in_Memory : 0;
  buf->data_target = (in_Flags & COI_SINK_MEMORY) != 0 ? in_Memory : 0;
  buf->process = (Process *) in_pProcesses[0];
  buf->size = in_Size;
  buf->type = BUFFER_MEMORY;

  /* Prepare output argument.  */
  *out_pBuffer = (COIBUFFER) buf;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIBufferDestroy, 1) (COIBUFFER in_Buffer)
{
  COITRACE ("COIBufferDestroy");

  cmd_t cmd = CMD_BUFFER_UNMAP;

  assert (in_Buffer != NULL);

  /* Convert input arguments.  */
  Buffer *buf = (Buffer *) in_Buffer;

  /* Unmap buffer on host.  */
  if (buf->data != 0 && buf->type == BUFFER_NORMAL)
    if (COIBufferUnmap ((COIMAPINSTANCE) in_Buffer, 0, 0, 0) == COI_ERROR)
      return COI_ERROR;

  /* Unmap buffer on target.  */
  if (buf->data_target != 0)
    {
      start_critical_section ();

      /* Send data to target.  */
      int pipe_host2tgt = buf->process->pipe_host2tgt;
      WRITE (pipe_host2tgt, &cmd, sizeof (cmd_t));
      WRITE (pipe_host2tgt, &buf->fd_target, sizeof (int));
      WRITE (pipe_host2tgt, &buf->data_target, sizeof (void *));
      WRITE (pipe_host2tgt, &buf->size, sizeof (uint64_t));

      /* Receive data from target.  */
      READ (buf->process->pipe_tgt2host, &cmd, sizeof (cmd_t));

      finish_critical_section ();
    }

  /* Unlink shared memory.  */
  if (buf->type == BUFFER_NORMAL)
    {
      if (close (buf->fd) < 0)
	COIERROR ("Cannot close shared memory file.");
      if (shm_unlink (buf->name) < 0)
	COIERROR ("Cannot unlink shared memory.");
      free (buf->name);
    }

  /* Clean up.  */
  delete buf;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIBufferGetSinkAddress, 1) (COIBUFFER in_Buffer,
					     uint64_t *out_pAddress)
{
  COITRACE ("COIBufferGetSinkAddress");

  assert (in_Buffer != NULL);
  assert (out_pAddress != NULL);

  /* Convert input arguments.  */
  Buffer *buf = (Buffer *) in_Buffer;

  /* Here should come BUFFER_NORMAL buffer.  */
  assert (buf->type == BUFFER_NORMAL);

  /* Prepare output argument.  */
  *out_pAddress = (uint64_t) buf->data_target;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIBufferMap, 1) (COIBUFFER in_Buffer,
				  uint64_t in_Offset,
				  uint64_t in_Length,		    // Ignored
				  COI_MAP_TYPE in_Type,		    // Ignored
				  uint32_t in_NumDependencies,
				  const COIEVENT *in_pDependencies, // Ignored
				  COIEVENT *out_pCompletion,
				  COIMAPINSTANCE *out_pMapInstance,
				  void **out_ppData)
{
  COITRACE ("COIBufferMap");

  /* Features of liboffloadmic.  */
  assert (in_Offset == 0);
  assert (in_NumDependencies == 0);

  /* Convert input arguments.  */
  Buffer *buf = (Buffer *) in_Buffer;

  /* Only BUFFER_NORMAL buffers should come here.  */
  assert (buf->type == BUFFER_NORMAL);

  /* Map shared memory.  */
  buf->data = mmap (NULL, buf->size, PROT_READ | PROT_WRITE,
		    MAP_SHARED, buf->fd, 0);
  if (buf->data == NULL)
    COIERROR ("Cannot map shared memory.");

  /* Prepare output arguments.  */
  if (out_pMapInstance != 0)
    *out_pMapInstance = (COIMAPINSTANCE) buf;
  if (out_ppData != 0)
    *out_ppData = buf->data;

  if (out_pCompletion)
    out_pCompletion->opaque[0] = 0;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIBufferRead, 1) (COIBUFFER in_SourceBuffer,
				   uint64_t in_Offset,
				   void *in_pDestData,
				   uint64_t in_Length,
				   COI_COPY_TYPE in_Type,
				   uint32_t in_NumDependencies,
				   const COIEVENT *in_pDependencies,  // Ignored
				   COIEVENT *out_pCompletion)
{
  COITRACE ("COIBufferRead");

  /* Features of liboffloadmic.  */
  assert (in_pDestData != NULL);
  assert (in_Type == COI_COPY_UNSPECIFIED);
  assert (in_NumDependencies == 0);

  /* Convert input arguments.  */
  Buffer *buf = (Buffer *) in_SourceBuffer;

  start_critical_section ();

  /* Map buffers if needed.  */
  if (buf->data == 0 && buf->type == BUFFER_NORMAL)
    if (COIBufferMap (in_SourceBuffer, 0, buf->size, (COI_MAP_TYPE) 0, 0, 0, 0,
		      0, 0) == COI_ERROR)
      return COI_ERROR;

  /* Copy data.  */
  memcpy (in_pDestData, (void *) ((uintptr_t) buf->data + in_Offset),
	  in_Length);

  /* Unmap buffers if needed.  */
  if (buf->type == BUFFER_NORMAL)
    if (COIBufferUnmap ((COIMAPINSTANCE) buf, 0, 0, 0) == COI_ERROR)
      return COI_ERROR;

  finish_critical_section ();

  if (out_pCompletion)
    out_pCompletion->opaque[0] = 0;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIBufferSetState, 1) (COIBUFFER in_Buffer,		  // Ignored
				       COIPROCESS in_Process,		  // Ignored
				       COI_BUFFER_STATE in_State,	  // Ignored
				       COI_BUFFER_MOVE_FLAG in_DataMove,
				       uint32_t in_NumDependencies,
				       const COIEVENT *in_pDependencies,  // Ignored
				       COIEVENT *out_pCompletion)
{
  COITRACE ("COIBufferSetState");

  /* Features of liboffloadmic.  */
  assert (in_DataMove == COI_BUFFER_NO_MOVE);
  assert (in_NumDependencies == 0);

  /* Looks like we have nothing to do here.  */

  if (out_pCompletion)
    out_pCompletion->opaque[0] = 0;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIBufferUnmap, 1) (COIMAPINSTANCE in_MapInstance,
				    uint32_t in_NumDependencies,
				    const COIEVENT *in_pDependencies, // Ignored
				    COIEVENT *out_pCompletion)
{
  COITRACE ("COIBufferUnmap");

  /* Features of liboffloadmic.  */
  assert (in_MapInstance != NULL);
  assert (in_NumDependencies == 0);

  /* Convert input arguments.  */
  Buffer *buffer = (Buffer *) in_MapInstance;

  /* Only BUFFER_NORMAL buffers should come here.  */
  assert (buffer->type == BUFFER_NORMAL);

  /* Unmap shared memory.  */
  if (munmap (buffer->data, buffer->size) < 0)
    COIERROR ("Cannot unmap shared memory.");

  buffer->data = 0;

  if (out_pCompletion)
    out_pCompletion->opaque[0] = 0;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIBufferWrite, 1) (COIBUFFER in_DestBuffer,
				    uint64_t in_Offset,
				    const void *in_pSourceData,
				    uint64_t in_Length,
				    COI_COPY_TYPE in_Type,
				    uint32_t in_NumDependencies,
				    const COIEVENT *in_pDependencies, // Ignored
				    COIEVENT *out_pCompletion)
{
  COITRACE ("COIBufferWrite");

  /* Features of liboffloadmic.  */
  assert (in_DestBuffer != NULL);
  assert (in_pSourceData != NULL);
  assert (in_Type == COI_COPY_UNSPECIFIED);
  assert (in_NumDependencies == 0);

  /* Convert input arguments.  */
  Buffer *buf = (Buffer *) in_DestBuffer;

  start_critical_section ();

  /* Map buffers if needed.  */
  if (buf->data == 0 && buf->type == BUFFER_NORMAL)
    if (COIBufferMap (in_DestBuffer, 0, buf->size, (COI_MAP_TYPE) 0, 0, 0, 0, 0,
		      0) == COI_ERROR)
      return COI_ERROR;

  /* Copy data.  */
  memcpy ((void *) ((uintptr_t) buf->data + in_Offset), in_pSourceData,
	  in_Length);

  /* Unmap buffers if needed.  */
  if (buf->type == BUFFER_NORMAL)
    if (COIBufferUnmap ((COIMAPINSTANCE) buf, 0, 0, 0) == COI_ERROR)
      return COI_ERROR;

  finish_critical_section ();

  if (out_pCompletion)
    out_pCompletion->opaque[0] = 0;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIEngineGetCount, 1) (COI_ISA_TYPE isa,
				       uint32_t *count)
{
  COITRACE ("COIEngineGetCount");

  /* Features of liboffloadmic.  */
  assert (isa == COI_ISA_MIC);
  assert (count != NULL);

  /* Prepare output arguments.  */
  *count = num_engines;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIEngineGetHandle, 1) (COI_ISA_TYPE in_ISA,
					uint32_t in_EngineIndex,
					COIENGINE *out_pEngineHandle)
{
  COITRACE ("COIEngineGetHandle");

  /* Features of liboffloadmic.  */
  assert (in_ISA == COI_ISA_MIC);
  assert (out_pEngineHandle != NULL);

  /* Check engine index.  */
  if (in_EngineIndex >= num_engines)
    COIERROR ("Wrong engine index.");

  /* Create engine handle.  */
  Engine *engine = new Engine;
  engine->dir = NULL;
  engine->index = in_EngineIndex;
  engine->type = in_ISA;

  /* Prepare output argument.  */
  *out_pEngineHandle = (COIENGINE) engine;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIEventWait, 1) (uint16_t in_NumEvents,
				  const COIEVENT *in_pEvents,
				  int32_t in_TimeoutMilliseconds,
				  uint8_t in_WaitForAll,
				  uint32_t *out_pNumSignaled,
				  uint32_t *out_pSignaledIndices)
{
  COITRACE ("COIEventWait");

  /* Features of liboffloadmic.  */
  assert (in_pEvents != NULL);
  assert (in_TimeoutMilliseconds == 0 || in_TimeoutMilliseconds == -1);
  assert (in_WaitForAll == 1);
  assert (out_pNumSignaled == NULL);
  assert (out_pSignaledIndices == NULL);

  if (in_TimeoutMilliseconds == 0)
    {
      /* If some event is not signalled, return timeout error.  */
      for (uint16_t i = 0; i < in_NumEvents; i++)
	if (non_signalled_events.count (in_pEvents[i].opaque[0]) > 0)
	  return COI_TIME_OUT_REACHED;
	else
	  {
	    /* If the event signalled with an error, return that error.  */
	    start_critical_section ();
	    COIRESULT res = get_event_result (in_pEvents[i]);
	    finish_critical_section ();
	    if (res != COI_SUCCESS)
	      return res;
	  }
    }
  else
    {
      /* Wait indefinitely for all events.  */
      for (uint16_t i = 0; i < in_NumEvents; i++)
	{
	  while (non_signalled_events.count (in_pEvents[i].opaque[0]) > 0)
	    usleep (1000);

	  /* If the event signalled with an error, return that error.  */
	  start_critical_section ();
	  COIRESULT res = get_event_result (in_pEvents[i]);
	  finish_critical_section ();
	  if (res != COI_SUCCESS)
	    return res;
	}
    }

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIEventRegisterCallback, 1) (const COIEVENT in_Event,
					      COI_EVENT_CALLBACK in_Callback,
					      const void *in_UserData,
					      const uint64_t in_Flags)
{
  COITRACE ("COIEventRegisterCallback");

  /* Features of liboffloadmic.  */
  assert (in_Callback != NULL);
  assert (in_UserData != NULL);
  assert (in_Flags == 0);

  start_critical_section ();
  if (non_signalled_events.count (in_Event.opaque[0]) == 0)
    {
      /* If the event is already signalled, invoke the callback immediately.  */
      COIRESULT res = get_event_result (in_Event);
      in_Callback (in_Event, res, in_UserData);
    }
  else
    {
      Callback callback;
      callback.ptr = in_Callback;
      callback.data = in_UserData;
      callbacks.insert (std::pair <uint64_t, Callback> (in_Event.opaque[0],
							callback));
    }
  finish_critical_section ();

  return COI_SUCCESS;
}


/* The start routine for the COI pipeline thread.  */

static void *
pipeline_thread_routine (void *in_Pipeline)
{
  /* Convert input arguments.  */
  Pipeline *pipeline = (Pipeline *) in_Pipeline;

  /* Open pipes.  */
  pipeline->pipe_host2tgt
    = open (pipeline->pipe_host2tgt_path, O_CLOEXEC | O_WRONLY);
  if (pipeline->pipe_host2tgt < 0)
    COIERRORN ("Cannot open host-to-target pipe.");
  pipeline->pipe_tgt2host
    = open (pipeline->pipe_tgt2host_path, O_CLOEXEC | O_RDONLY);
  if (pipeline->pipe_tgt2host < 0)
    COIERRORN ("Cannot open target-to-host pipe.");

  free (pipeline->pipe_host2tgt_path);
  free (pipeline->pipe_tgt2host_path);
  pipeline->pipe_host2tgt_path = NULL;
  pipeline->pipe_tgt2host_path = NULL;

  while (!pipeline->destroy)
    if (pipeline->queue.empty ())
      usleep (1000);
    else
      {
	Function func = pipeline->queue.front ();
	start_critical_section ();
	pipeline->queue.pop ();
	finish_critical_section ();

	/* Send data to target.  */
	cmd_t cmd = CMD_PIPELINE_RUN_FUNCTION;
	WRITEN (pipeline->pipe_host2tgt, &cmd, sizeof (cmd_t));
	WRITEN (pipeline->pipe_host2tgt, &func.ptr, sizeof (void *));
	WRITEN (pipeline->pipe_host2tgt, &func.num_buffers, sizeof (uint32_t));
	for (uint32_t i = 0; i < func.num_buffers; i++)
	  {
	    WRITEN (pipeline->pipe_host2tgt, &func.bufs_size[i],
		    sizeof (uint64_t));
	    WRITEN (pipeline->pipe_host2tgt, &func.bufs_data_target[i],
		    sizeof (void *));
	  }
	WRITEN (pipeline->pipe_host2tgt, &func.misc_data_len,
		sizeof (uint16_t));
	if (func.misc_data_len > 0)
	  WRITEN (pipeline->pipe_host2tgt, func.misc_data, func.misc_data_len);
	WRITEN (pipeline->pipe_host2tgt, &func.return_value_len,
		sizeof (uint16_t));

	delete [] func.bufs_size;
	delete [] func.bufs_data_target;

	/* Receive data from target.  Wait for target function to complete,
	   whether it has any data to return or not.  */
	bool has_return_value = func.return_value_len > 0;
	int ret_len
	  = read (pipeline->pipe_tgt2host,
		  has_return_value ? func.return_value : &cmd,
		  has_return_value ? func.return_value_len : sizeof (cmd_t));
	if (ret_len == 0)
	  {
	    start_critical_section ();
	    signal_event (func.completion_event, COI_PROCESS_DIED);
	    pipeline->is_destroyed = true;
	    finish_critical_section ();
	    return NULL;
	  }
	else if (ret_len != (has_return_value ? func.return_value_len
					      : sizeof (cmd_t)))
	  COIERRORN ("Cannot read from pipe.");

	start_critical_section ();
	signal_event (func.completion_event, COI_SUCCESS);
	finish_critical_section ();
      }

  /* Send data to target.  */
  const cmd_t cmd = CMD_PIPELINE_DESTROY;
  WRITEN (pipeline->pipe_host2tgt, &cmd, sizeof (cmd_t));

  /* Close pipes.  */
  if (close (pipeline->pipe_host2tgt) < 0)
    COIERRORN ("Cannot close host-to-target pipe.");
  if (close (pipeline->pipe_tgt2host) < 0)
    COIERRORN ("Cannot close target-to-host pipe.");

  start_critical_section ();
  pipeline->is_destroyed = true;
  finish_critical_section ();
  return NULL;
}


COIRESULT
SYMBOL_VERSION (COIPipelineCreate, 1) (COIPROCESS in_Process,
				       COI_CPU_MASK in_Mask,
				       uint32_t in_StackSize,	    // Ignored
				       COIPIPELINE *out_pPipeline)
{
  COITRACE ("COIPipelineCreate");

  /* Features of liboffloadmic.  */
  assert (in_Process != NULL);
  assert (in_Mask == 0);
  assert (out_pPipeline != NULL);

  /* Convert input arguments.  */
  Process *proc = (Process *) in_Process;

  start_critical_section ();

  /* Create pipeline handle.  */
  Pipeline *pipeline = new Pipeline;
  pipeline->destroy = false;
  pipeline->is_destroyed = false;
  pipeline->process = proc;
  pipelines.insert (pipeline);

  /* Create pipes.  */
  uint32_t pipeline_num = max_pipeline_num++;
  char *eng_dir = pipeline->process->engine->dir;
  MALLOC (char *, pipeline->pipe_host2tgt_path,
	  strlen (eng_dir) + sizeof (PIPE_HOST2TGT_NAME "0000000000"));
  MALLOC (char *, pipeline->pipe_tgt2host_path,
	  strlen (eng_dir) + sizeof (PIPE_TGT2HOST_NAME "0000000000"));
  sprintf (pipeline->pipe_host2tgt_path, "%s" PIPE_HOST2TGT_NAME "%010d",
	   eng_dir, pipeline_num);
  sprintf (pipeline->pipe_tgt2host_path, "%s" PIPE_TGT2HOST_NAME "%010d",
	   eng_dir, pipeline_num);
  if (mkfifo (pipeline->pipe_host2tgt_path, S_IRUSR | S_IWUSR) < 0)
    COIERROR ("Cannot create pipe %s.", pipeline->pipe_host2tgt_path);
  if (mkfifo (pipeline->pipe_tgt2host_path, S_IRUSR | S_IWUSR) < 0)
    COIERROR ("Cannot create pipe %s.", pipeline->pipe_tgt2host_path);

  /* Send data to target.  */
  const cmd_t cmd = CMD_PIPELINE_CREATE;
  WRITE (proc->pipe_host2tgt, &cmd, sizeof (cmd_t));
  WRITE (proc->pipe_host2tgt, &pipeline_num, sizeof (pipeline_num));

  /* Create a new thread for the pipeline.  */
  if (pthread_create (&pipeline->thread, NULL, pipeline_thread_routine,
		      pipeline))
    COIERROR ("Cannot create new thread.");

  finish_critical_section ();

  /* Prepare output arguments.  */
  *out_pPipeline = (COIPIPELINE) pipeline;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIPipelineDestroy, 1) (COIPIPELINE in_Pipeline)
{
  COITRACE ("COIPipelineDestroy");

  assert (in_Pipeline != NULL);

  /* Convert input arguments.  */
  Pipeline *pipeline = (Pipeline *) in_Pipeline;

  start_critical_section ();
  /* Remove pipeline from the set of undestroyed pipelines.  */
  pipelines.erase (pipeline);

  /* Exit pipeline thread.  */
  pipeline->destroy = true;
  finish_critical_section ();

  while (!pipeline_is_destroyed (pipeline))
    usleep (1000);

  /* Join with a destroyed thread.  */
  if (pthread_join (pipeline->thread, NULL))
    COIERROR ("Cannot join with a thread.");

  delete pipeline;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIPipelineRunFunction, 1) (COIPIPELINE in_Pipeline,
					    COIFUNCTION in_Function,
					    uint32_t in_NumBuffers,
					    const COIBUFFER *in_Buffers,
					    const COI_ACCESS_FLAGS *in_pBufferAccessFlags, // Ignored
					    uint32_t in_NumDependencies,
					    const COIEVENT *in_pDependencies,		   // Ignored
					    const void *in_pMiscData,
					    uint16_t in_MiscDataLen,
					    void *out_pAsyncReturnValue,
					    uint16_t in_AsyncReturnValueLen,
					    COIEVENT *out_pCompletion)
{
  COITRACE ("COIPipelineRunFunction");

  /* Features of liboffloadmic.  */
  assert (in_Pipeline != NULL);
  assert (in_Function != NULL);
  assert (in_NumDependencies == 0);

  Function func;
  func.ptr = (void *) in_Function;
  func.num_buffers = in_NumBuffers;
  func.bufs_size = new uint64_t [in_NumBuffers];
  func.bufs_data_target = new void * [in_NumBuffers];
  for (uint32_t i = 0; i < in_NumBuffers; i++)
    {
      Buffer **bufs = (Buffer **) in_Buffers;
      func.bufs_size[i] = bufs[i]->size;
      func.bufs_data_target[i] = bufs[i]->data_target;
    }
  func.misc_data = (void *) in_pMiscData;
  func.misc_data_len = in_MiscDataLen;
  func.return_value = out_pAsyncReturnValue;
  func.return_value_len = in_AsyncReturnValueLen;

  start_critical_section ();
  func.completion_event.opaque[0] = max_event_num++;
  non_signalled_events.insert (func.completion_event.opaque[0]);
  ((Pipeline *) in_Pipeline)->queue.push (func);
  finish_critical_section ();

  /* In case of synchronous execution we have to wait for target.  */
  if (out_pCompletion == NULL)
    COIEventWait (1, &func.completion_event, -1, 1, NULL, NULL);
  else
    *out_pCompletion = func.completion_event;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIProcessCreateFromMemory, 1) (COIENGINE in_Engine,
						const char *in_pBinaryName,
						const void *in_pBinaryBuffer,
						uint64_t in_BinaryBufferLength,
						int in_Argc,
						const char **in_ppArgv,
						uint8_t in_DupEnv,
						const char **in_ppAdditionalEnv,
						uint8_t in_ProxyActive,		// Ignored
						const char *in_Reserved,	// Ignored
						uint64_t in_InitialBufferSpace,	// Ignored
						const char *in_LibrarySearchPath,
						const char *in_FileOfOrigin,    // Ignored
						uint64_t in_FileOfOriginOffset,	// Ignored
						COIPROCESS *out_pProcess)
{
  COITRACE ("COIProcessCreateFromMemory");

  const int run_max_args_num = 128;
  char *run_argv[run_max_args_num];
  char *emul_run = getenv (OFFLOAD_EMUL_RUN_ENV);
  const int uint_max_len = 11;

  /* Features of liboffloadmic.  */
  assert (in_Engine != NULL);
  assert (in_pBinaryName != NULL);
  assert (in_pBinaryBuffer != NULL);
  assert (in_Argc == 0);
  assert (in_ppArgv == NULL);
  assert (in_ppAdditionalEnv == NULL);
  assert (in_LibrarySearchPath != NULL);
  assert (out_pProcess != NULL);

  /* Convert input arguments.  */
  Engine *eng = (Engine *) in_Engine;

  /* Create temporary directory for engine files.  */
  assert (eng->dir == NULL);
  STRDUP (eng->dir, ENGINE_PATH);
  if (mkdtemp (eng->dir) == NULL)
    COIERROR ("Cannot create temporary directory %s.", eng->dir);

  /* Save path to engine directory for clean up on exit.  */
  tmp_dirs_num++;
  tmp_dirs = (char **) realloc (tmp_dirs, tmp_dirs_num * sizeof (char *));
  if (!tmp_dirs)
    COIERROR ("Cannot allocate memory.");
  STRDUP (tmp_dirs[tmp_dirs_num - 1], eng->dir);

  /* Create target executable file.  */
  char *target_exe;
  MALLOC (char *, target_exe, strlen (eng->dir) + strlen (in_pBinaryName) + 2);
  sprintf (target_exe, "%s/%s", eng->dir, in_pBinaryName);
  int fd = open (target_exe, O_CLOEXEC | O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR);
  if (fd < 0)
    COIERROR ("Cannot create file %s.", target_exe);
  FILE *file = fdopen (fd, "wb");
  if (file == NULL)
    COIERROR ("Cannot associate stream with file descriptor.");
  if (fwrite (in_pBinaryBuffer, 1, in_BinaryBufferLength, file)
      != in_BinaryBufferLength)
    COIERROR ("Cannot write in file %s.", target_exe);
  if (fclose (file) != 0)
    COIERROR ("Cannot close file %s.", target_exe);

  /* Fix file permissions.  */
  if (chmod (target_exe, S_IRWXU) < 0)
    COIERROR ("Cannot change permissions for file %s.", target_exe);

  /* Create directory for pipes to prevent names collision.  */
  char *pipes_path;
  MALLOC (char *, pipes_path, strlen (eng->dir) + sizeof (PIPES_PATH));
  sprintf (pipes_path, "%s" PIPES_PATH, eng->dir);
  if (mkdir (pipes_path, S_IRWXU) < 0)
    COIERROR ("Cannot create folder %s.", pipes_path);

  /* Create 2 main pipes for inter-process communication.  */
  char *pipe_host2tgt_path, *pipe_tgt2host_path;
  MALLOC (char *, pipe_host2tgt_path,
	  strlen (eng->dir) + sizeof (PIPE_HOST2TGT_NAME "mainpipe"));
  MALLOC (char *, pipe_tgt2host_path,
	  strlen (eng->dir) + sizeof (PIPE_TGT2HOST_NAME "mainpipe"));
  sprintf (pipe_host2tgt_path, "%s" PIPE_HOST2TGT_NAME "mainpipe", eng->dir);
  sprintf (pipe_tgt2host_path, "%s" PIPE_TGT2HOST_NAME "mainpipe", eng->dir);
  if (mkfifo (pipe_host2tgt_path, S_IRUSR | S_IWUSR) < 0)
    COIERROR ("Cannot create main pipe %s.", pipe_host2tgt_path);
  if (mkfifo (pipe_tgt2host_path, S_IRUSR | S_IWUSR) < 0)
    COIERROR ("Cannot create main pipe %s.", pipe_tgt2host_path);

  /* Prepare argv.  */
  if (emul_run == NULL || strcmp (emul_run, "") == 0)
    {
      STRDUP (run_argv[0], target_exe);
      run_argv[1] = (char *) NULL;
    }
  else
    {
      char *ptr, *tmp;
      int i = 0;
      STRDUP (tmp, emul_run);
      char *tok = strtok_r (tmp, " ", &ptr);
      while (tok != NULL)
	{
	  if (i >= run_max_args_num)
	    COIERROR ("Run command has too many arguments.");
	  STRDUP (run_argv[i++], tok);
	  tok = strtok_r (NULL, " ", &ptr);
	}
      STRDUP (run_argv[i], target_exe);
      run_argv[i + 1] = (char *) NULL;
      free (tmp);
    }

  /* Prepare envp.  */
  int env_num = 0;
  if (in_DupEnv == true)
    while (environ[env_num++]);
  env_num += 4; // LD_LIBRARY_PATH, MIC_DIR, MIC_INDEX, NULL

  char **envp;
  MALLOC (char **, envp, env_num * sizeof (char *));

  int env_i = 0;
  if (in_DupEnv == true)
    for (unsigned i = 0; environ[i] != NULL; i++)
      {
	unsigned j;
	char *env_name;
	STRDUP (env_name, environ[i]);
	for (j = 0; env_name[j] != '=' && env_name[j] != '\0'; j++);
	env_name[j] = '\0';
	if (strcmp (env_name, "LD_LIBRARY_PATH") != 0
	    && strcmp (env_name, MIC_DIR_ENV) != 0
	    && strcmp (env_name, MIC_INDEX_ENV) != 0)
	  STRDUP (envp[env_i++], environ[i]);
	free (env_name);
      }

  MALLOC (char *, envp[env_i], strlen (MIC_DIR_ENV) + strlen (eng->dir) + 2);
  sprintf (envp[env_i], "%s=%s", MIC_DIR_ENV, eng->dir);

  MALLOC (char *, envp[env_i + 1], strlen (MIC_INDEX_ENV) + uint_max_len + 1);
  sprintf (envp[env_i + 1], "%s=%u", MIC_INDEX_ENV, eng->index);

  MALLOC (char *, envp[env_i + 2],
	  strlen ("LD_LIBRARY_PATH=") + strlen (in_LibrarySearchPath) + 1);
  sprintf (envp[env_i + 2], "LD_LIBRARY_PATH=%s", in_LibrarySearchPath);

  envp[env_i + 3] = (char *) NULL;

  /* Create target process.  */
  pid_t pid = vfork ();
  if (pid < 0)
    COIERROR ("Cannot create child process.");

  if (pid == 0)
    {
      /* Run target executable.  */
      if (execvpe (run_argv[0], run_argv, envp) == -1)
	COIERROR ("Cannot execute file %s.", target_exe);
    }

  /* Open main pipes.  */
  int pipe_host2tgt = open (pipe_host2tgt_path, O_CLOEXEC | O_WRONLY);
  if (pipe_host2tgt < 0)
    COIERROR ("Cannot open host-to-target main pipe.");
  int pipe_tgt2host = open (pipe_tgt2host_path, O_CLOEXEC | O_RDONLY);
  if (pipe_tgt2host < 0)
    COIERROR ("Cannot open target-to-host main pipe.");

  /* Create process handle.  */
  Process *proc = new Process;
  proc->pid = pid;
  proc->pipe_host2tgt = pipe_host2tgt;
  proc->pipe_tgt2host = pipe_tgt2host;
  proc->engine = eng;
  proc->functions = NULL;

  /* Prepare output arguments.  */
  *out_pProcess = (COIPROCESS) proc;

  /* Clean up.  */
  for (unsigned i = 0; run_argv[i] != NULL; i++)
    free (run_argv[i]);
  for (unsigned i = 0; envp[i] != NULL; i++)
    free (envp[i]);
  free (envp);
  free (pipe_host2tgt_path);
  free (pipe_tgt2host_path);
  free (pipes_path);
  free (target_exe);

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIProcessCreateFromFile, 1) (COIENGINE in_Engine,
					      const char *in_pBinaryName,
					      int in_Argc,
					      const char **in_ppArgv,
					      uint8_t in_DupEnv,
					      const char **in_ppAdditionalEnv,
					      uint8_t in_ProxyActive,
					      const char *in_Reserved,
					      uint64_t in_BufferSpace,
					      const char *in_LibrarySearchPath,
					      COIPROCESS *out_pProcess)
{
  COITRACE ("COIProcessCreateFromFile");

  /* liboffloadmic with GCC compiled binaries should never go here.  */
  assert (false);
  return COI_ERROR;
}


COIRESULT
SYMBOL_VERSION (COIProcessDestroy, 1) (COIPROCESS in_Process,
				       int32_t in_WaitForMainTimeout, // Ignored
				       uint8_t in_ForceDestroy,
				       int8_t *out_pProcessReturn,
				       uint32_t *out_pTerminationCode)
{
  COITRACE ("COIProcessDestroy");

  assert (in_Process != NULL);
  assert (out_pProcessReturn != NULL);
  assert (out_pTerminationCode != NULL);

  /* Convert input arguments.  */
  Process *proc = (Process *) in_Process;

  /* Destroy all undestroyed pipelines.  */
  while (!pipelines.empty ())
    {
      std::set<Pipeline *>::iterator p = pipelines.begin ();
      COIPipelineDestroy ((COIPIPELINE) *p);
    }

  /* Close main pipes.  */
  if (close (proc->pipe_host2tgt) < 0)
    COIERROR ("Cannot close host-to-target main pipe.");
  if (close (proc->pipe_tgt2host) < 0)
    COIERROR ("Cannot close target-to-host main pipe.");

  /* Shutdown target process by force.  */
  if (in_ForceDestroy)
    kill (proc->pid, SIGTERM);

  /* Clean up.  */
  free (proc->engine->dir);
  free (proc->functions);
  delete proc->engine;
  delete proc;

  /* Prepare output arguments.  */
  *out_pProcessReturn = 0;
  *out_pTerminationCode = 0;

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIProcessGetFunctionHandles, 1) (COIPROCESS in_Process,
						  uint32_t in_NumFunctions,
						  const char **in_ppFunctionNameArray,
						  COIFUNCTION *out_pFunctionHandleArray)
{
  COITRACE ("COIProcessGetFunctionHandles");

  assert (in_Process != NULL);
  assert (in_ppFunctionNameArray != NULL);
  assert (out_pFunctionHandleArray != NULL);

  /* Convert input arguments.  */
  Process *proc = (Process *) in_Process;

  /* This function should be called once for the process.  */
  assert (proc->functions == NULL);

  /* Create array of function pointers.  Last element is 0, what shows the end
     of the array.  This array is used to free memory when process is
     destroyed.  */
  proc->functions = (void **) calloc (in_NumFunctions + 1, sizeof (void *));
  if (proc->functions == NULL)
    COIERROR ("Cannot allocate memory.");

  /* Get handles for functions.  */
  for (uint32_t i = 0; i < in_NumFunctions; i++)
    {
      size_t len = strlen (in_ppFunctionNameArray[i]) + 1;

      start_critical_section ();

      /* Send data to target.  */
      const cmd_t cmd = CMD_GET_FUNCTION_HANDLE;
      WRITE (proc->pipe_host2tgt, &cmd, sizeof (cmd_t));
      WRITE (proc->pipe_host2tgt, &len, sizeof (size_t));
      WRITE (proc->pipe_host2tgt, in_ppFunctionNameArray[i], len);

      /* Receive data from target.  */
      void *fn_ptr;
      READ (proc->pipe_tgt2host, &fn_ptr, sizeof (void *));

      finish_critical_section ();

      /* Save function pointer.  */
      proc->functions[i] = fn_ptr;

      /* Prepare output arguments.  */
      out_pFunctionHandleArray[i] = (COIFUNCTION) fn_ptr;
    }

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIProcessLoadLibraryFromMemory, 2) (COIPROCESS in_Process,
						     const void *in_pLibraryBuffer,
						     uint64_t in_LibraryBufferLength,
						     const char *in_pLibraryName,
						     const char *in_LibrarySearchPath,	// Ignored
						     const char *in_FileOfOrigin,	// Ignored
						     uint64_t in_FileOfOriginOffset,	// Ignored
						     uint32_t in_Flags,			// Ignored
						     COILIBRARY *out_pLibrary)
{
  COITRACE ("COIProcessLoadLibraryFromMemory");

  assert (in_Process != NULL);
  assert (in_pLibraryBuffer != NULL);
  assert (out_pLibrary != NULL);

  /* Convert input arguments.  */
  Process *proc = (Process *) in_Process;

  /* Create target library file.  */
  char *lib_path;
  size_t len = strlen (proc->engine->dir) + strlen (in_pLibraryName) + 2;
  MALLOC (char *, lib_path, len);
  sprintf (lib_path, "%s/%s", proc->engine->dir, in_pLibraryName);
  int fd = open (lib_path, O_CLOEXEC | O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR);
  if (fd < 0)
    COIERROR ("Cannot create file %s.", lib_path);
  FILE *file = fdopen (fd, "wb");
  if (file == NULL)
    COIERROR ("Cannot associate stream with file descriptor.");
  if (fwrite (in_pLibraryBuffer, 1, in_LibraryBufferLength, file)
      != in_LibraryBufferLength)
    COIERROR ("Cannot write in file %s.", lib_path);
  if (fclose (file) != 0)
    COIERROR ("Cannot close file %s.", lib_path);

  start_critical_section ();

  /* Make target open library.  */
  const cmd_t cmd = CMD_OPEN_LIBRARY;
  WRITE (proc->pipe_host2tgt, &cmd, sizeof (cmd_t));
  WRITE (proc->pipe_host2tgt, &len, sizeof (size_t));
  WRITE (proc->pipe_host2tgt, lib_path, len);

  /* Receive data from target.  */
  void *handle;
  READ (proc->pipe_tgt2host, &handle, sizeof (void *));

  finish_critical_section ();

  /* Clean up.  */
  free (lib_path);

  *out_pLibrary = (COILIBRARY) handle;
  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIProcessRegisterLibraries, 1) (uint32_t in_NumLibraries,		      // Ignored
						 const void **in_ppLibraryArray,	      // Ignored
						 const uint64_t *in_pLibrarySizeArray,	      // Ignored
						 const char **in_ppFileOfOriginArray,	      // Ignored
						 const uint64_t *in_pFileOfOriginOffSetArray) // Ignored
{
  COITRACE ("COIProcessRegisterLibraries");

  /* Looks like we have nothing to do here.  */

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIProcessUnloadLibrary, 1) (COIPROCESS in_Process,
					     COILIBRARY in_Library)
{
  COITRACE ("COIProcessUnloadLibrary");

  assert (in_Process != NULL);
  assert (in_Library != NULL);

  const cmd_t cmd = CMD_CLOSE_LIBRARY;

  /* Convert input arguments.  */
  Process *proc = (Process *) in_Process;

  start_critical_section ();

  /* Make target close library.  */
  WRITE (proc->pipe_host2tgt, &cmd, sizeof (cmd_t));
  WRITE (proc->pipe_host2tgt, &in_Library, sizeof (void *));

  finish_critical_section ();

  return COI_SUCCESS;
}


uint64_t
SYMBOL_VERSION (COIPerfGetCycleFrequency, 1) ()
{
  COITRACE ("COIPerfGetCycleFrequency");

  return (uint64_t) CYCLE_FREQUENCY;
}


COIRESULT
SYMBOL_VERSION (COIPipelineClearCPUMask, 1) (COI_CPU_MASK *in_Mask)
{
  COITRACE ("COIPipelineClearCPUMask");

  /* Looks like we have nothing to do here.  */

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIPipelineSetCPUMask, 1) (COIPROCESS in_Process,
					   uint32_t in_CoreID,
					   uint8_t in_ThreadID,
					   COI_CPU_MASK *out_pMask)
{
  COITRACE ("COIPipelineSetCPUMask");

  /* Looks like we have nothing to do here.  */

  return COI_SUCCESS;
}


COIRESULT
SYMBOL_VERSION (COIEngineGetInfo, 1) (COIENGINE in_EngineHandle,  // Ignored
				      uint32_t in_EngineInfoSize, // Ignored
				      COI_ENGINE_INFO *out_pEngineInfo)
{
  COITRACE ("COIEngineGetInfo");

  assert (out_pEngineInfo != NULL);

  out_pEngineInfo->ISA = COI_DEVICE_KNL;
  out_pEngineInfo->NumCores = 1;
  out_pEngineInfo->NumThreads = 8;
  out_pEngineInfo->CoreMaxFrequency = SYMBOL_VERSION(COIPerfGetCycleFrequency,1)() / 1000000;
  out_pEngineInfo->PhysicalMemory = 1024;
  out_pEngineInfo->PhysicalMemoryFree = 1024;
  out_pEngineInfo->SwapMemory = 1024;
  out_pEngineInfo->SwapMemoryFree = 1024;
  out_pEngineInfo->MiscFlags = COI_ENG_ECC_DISABLED;

  return COI_SUCCESS;
}

} // extern "C"

