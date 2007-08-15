// natPosixProcess.cc - Native side of POSIX process code.

/* Copyright (C) 1998, 1999, 2000, 2002, 2003, 2004, 2005, 2006, 2007
  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>

#include <posix.h>
#include <posix-threads.h>
#include <jvm.h>

#include <java/lang/PosixProcess$ProcessManager.h>
#include <java/lang/PosixProcess.h>
#include <java/lang/IllegalThreadStateException.h>
#include <java/lang/InternalError.h>
#include <java/lang/InterruptedException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/Thread.h>
#include <java/io/File.h>
#include <java/io/FileDescriptor.h>
#include <gnu/java/nio/channels/FileChannelImpl.h>
#include <java/io/FileInputStream.h>
#include <java/io/FileOutputStream.h>
#include <java/io/IOException.h>
#include <java/lang/OutOfMemoryError.h>
#include <java/lang/PosixProcess$EOFInputStream.h>

using gnu::java::nio::channels::FileChannelImpl;
using namespace java::lang;

extern char **environ;

static char *
new_string (jstring string)
{
  jsize s = _Jv_GetStringUTFLength (string);
  char *buf = (char *) _Jv_Malloc (s + 1);
  _Jv_GetStringUTFRegion (string, 0, string->length(), buf);
  buf[s] = '\0';
  return buf;
}

static void
cleanup (char **args, char **env, char *path)
{
  if (args != NULL)
    {
      for (int i = 0; args[i] != NULL; ++i)
	_Jv_Free (args[i]);
      _Jv_Free (args);
    }
  if (env != NULL)
    {
      for (int i = 0; env[i] != NULL; ++i)
	_Jv_Free (env[i]);
      _Jv_Free (env);
    }
  if (path != NULL)
    _Jv_Free (path);
}

// This makes our error handling a bit simpler and it lets us avoid
// thread bugs where we close a possibly-reopened file descriptor for
// a second time.
static void
myclose (int &fd)
{
  if (fd != -1)
    close (fd);
  fd = -1;
}

namespace
{
  struct ProcessManagerInternal
  {
    int pipe_ends[2];
    struct sigaction old_sigaction;
  };
}


// There has to be a signal handler in order to be able to
// sigwait() on SIGCHLD.  The information passed is ignored as it
// will be recovered by the waitpid() call.
static void
#ifdef SA_SIGINFO
sigchld_handler (int sig, siginfo_t *si, void *third)
#else
sigchld_handler (int sig)
#endif
{
  if (PosixProcess$ProcessManager::nativeData != NULL)
    {
      ProcessManagerInternal *pmi =
        (ProcessManagerInternal *)PosixProcess$ProcessManager::nativeData;
      char c = 0;
      ::write(pmi->pipe_ends[1], &c, 1);
      if (pmi->old_sigaction.sa_handler != SIG_DFL
          && pmi->old_sigaction.sa_handler != SIG_IGN)
        {
#ifdef SA_SIGINFO
          if ((pmi->old_sigaction.sa_flags & SA_SIGINFO) != 0)
            pmi->old_sigaction.sa_sigaction(sig, si, third);
          else
#endif
            (*pmi->old_sigaction.sa_handler)(sig);
        }
    }
}


// Get ready to enter the main reaper thread loop.
void
java::lang::PosixProcess$ProcessManager::init ()
{
  // The nativeData is static to avoid races installing the signal
  // handler in the case that it is chained.
  if (nativeData == NULL )
    {
      ProcessManagerInternal *pmi =
        (ProcessManagerInternal *)JvAllocBytes(sizeof(ProcessManagerInternal));

      if (0 != ::pipe(pmi->pipe_ends))
        goto error;

      // Make writing non-blocking so that the signal handler will
      // never block.
      int fl = ::fcntl(pmi->pipe_ends[1], F_GETFL);
      ::fcntl(pmi->pipe_ends[1], F_SETFL, fl | O_NONBLOCK);

      nativeData = (::gnu::gcj::RawDataManaged *)pmi;

      // SIGCHLD is blocked in all threads in posix-threads.cc.
      // Setup the SIGCHLD handler.
      struct sigaction sa;
      memset (&sa, 0, sizeof (sa));

#ifdef SA_SIGINFO
      sa.sa_sigaction = sigchld_handler;
      // We only want signals when the things exit.
      sa.sa_flags = SA_NOCLDSTOP | SA_SIGINFO;
#else
      sa.sa_handler = sigchld_handler;
      // We only want signals when the things exit.
      sa.sa_flags = SA_NOCLDSTOP;
#endif

      if (-1 == sigaction (SIGCHLD, &sa, &pmi->old_sigaction))
        goto error;
    }
  // All OK.
  return;

error:
  throw new InternalError (JvNewStringUTF (strerror (errno)));
}

void
java::lang::PosixProcess$ProcessManager::waitForSignal ()
{
  // Wait for SIGCHLD
  _Jv_UnBlockSigchld();
  ProcessManagerInternal *pmi = (ProcessManagerInternal *)nativeData;

  // Try to read multiple (64) notifications in one go.
  char c[64];
 ::read(pmi->pipe_ends[0], c, sizeof (c));

  _Jv_BlockSigchld();

  return;
}

jboolean java::lang::PosixProcess$ProcessManager::reap (PosixProcess *p)
{
  pid_t rv;

  // Try to get the return code from the child process.
  int status;
  rv = ::waitpid ((pid_t)p->pid, &status, WNOHANG);
  if (rv == -1)
    throw new InternalError (JvNewStringUTF (strerror (errno)));

  if (rv == 0)
    return false;   // No children to wait for.

  JvSynchronize sync (p);
  p->status = WIFEXITED (status) ? WEXITSTATUS (status) : -1;
  p->state = PosixProcess::STATE_TERMINATED;
  p->processTerminationCleanup();
  p->notifyAll ();
  return true;
}

void
java::lang::PosixProcess$ProcessManager::signalReaper ()
{
  ProcessManagerInternal *pmi = (ProcessManagerInternal *)nativeData;
  char c = 0;
  ::write(pmi->pipe_ends[1], &c, 1);
  // Ignore errors.  If EPIPE the reaper has already exited.
}

void
java::lang::PosixProcess::nativeDestroy ()
{
  int c = ::kill ((pid_t) pid, SIGKILL);
  if (c == 0)
    return;
  // kill() failed.
  throw new InternalError (JvNewStringUTF (strerror (errno)));
}

void
java::lang::PosixProcess::nativeSpawn ()
{
  using namespace java::io;

  // Initialize all locals here to make cleanup simpler.
  char **args = NULL;
  char **env = NULL;
  char *path = NULL;
  int inp[2], outp[2], errp[2], msgp[2];
  inp[0] = -1;
  inp[1] = -1;
  outp[0] = -1;
  outp[1] = -1;
  errp[0] = -1;
  errp[1] = -1;
  msgp[0] = -1;
  msgp[1] = -1;
  errorStream = NULL;
  inputStream = NULL;
  outputStream = NULL;

  try
    {
      // Transform arrays to native form.
      args = (char **) _Jv_Malloc ((progarray->length + 1) * sizeof (char *));

      // Initialize so we can gracefully recover.
      jstring *elts = elements (progarray);
      for (int i = 0; i <= progarray->length; ++i)
	args[i] = NULL;

      for (int i = 0; i < progarray->length; ++i)
	args[i] = new_string (elts[i]);
      args[progarray->length] = NULL;

      if (envp)
	{
          bool need_path = true;
          bool need_ld_library_path = true;
          int i;

          // Preserve PATH and LD_LIBRARY_PATH unless specified
          // explicitly.  We need three extra slots.  Potentially PATH
          // and LD_LIBRARY_PATH will be added plus the NULL
          // termination.
	  env = (char **) _Jv_Malloc ((envp->length + 3) * sizeof (char *));
	  elts = elements (envp);

	  // Initialize so we can gracefully recover.
	  for (i = 0; i < envp->length + 3; ++i)
	    env[i] = NULL;

	  for (i = 0; i < envp->length; ++i)
            {
              env[i] = new_string (elts[i]);
              if (!strncmp (env[i], "PATH=", sizeof("PATH=")))
                need_path = false;
              if (!strncmp (env[i], "LD_LIBRARY_PATH=",
                            sizeof("LD_LIBRARY_PATH=")))
                need_ld_library_path = false;
            }

          if (need_path)
            {
	      char *path_val = getenv ("PATH");
              if (path_val)
                {
                  env[i] = (char *) _Jv_Malloc (strlen (path_val) +
                                                sizeof("PATH=") + 1);
                  strcpy (env[i], "PATH=");
                  strcat (env[i], path_val);
                  i++;
                }
            }
          if (need_ld_library_path)
            {
	      char *path_val = getenv ("LD_LIBRARY_PATH");
              if (path_val)
                {
                  env[i] =
                    (char *) _Jv_Malloc (strlen (path_val) +
                                         sizeof("LD_LIBRARY_PATH=") + 1);
                  strcpy (env[i], "LD_LIBRARY_PATH=");
                  strcat (env[i], path_val);
                  i++;
                }
            }
	  env[i] = NULL;
	}

      // We allocate this here because we can't call malloc() after
      // the fork.
      if (dir != NULL)
	path = new_string (dir->getPath ());

      // Create pipes for I/O.  MSGP is for communicating exec()
      // status.  If redirecting stderr to stdout, we don't need to
      // create the ERRP pipe.
      if (pipe (inp) || pipe (outp) || pipe (msgp)
	  || fcntl (msgp[1], F_SETFD, FD_CLOEXEC))
	throw new IOException (JvNewStringUTF (strerror (errno)));
      if (! redirect && pipe (errp))
	throw new IOException (JvNewStringUTF (strerror (errno)));

      // We create the streams before forking.  Otherwise if we had an
      // error while creating the streams we would have run the child
      // with no way to communicate with it.
      if (redirect)
	errorStream = PosixProcess$EOFInputStream::instance;
      else
	errorStream =
	  new FileInputStream (new
			       FileChannelImpl (errp[0],
						FileChannelImpl::READ));
      inputStream =
	new FileInputStream (new
			     FileChannelImpl (inp[0], FileChannelImpl::READ));
      outputStream =
	new FileOutputStream (new FileChannelImpl (outp[1],
						   FileChannelImpl::WRITE));

      // We don't use vfork() because that would cause the local
      // environment to be set by the child.

    // Use temporary for fork result to avoid dirtying an extra page.
    pid_t pid_tmp;
    if ((pid_tmp = fork ()) == -1)
      throw new IOException (JvNewStringUTF (strerror (errno)));

    if (pid_tmp == 0)
	{
	  // Child process, so remap descriptors, chdir and exec.
	  if (envp)
            environ = env;

	  // We ignore errors from dup2 because they should never occur.
	  dup2 (outp[0], 0);
	  dup2 (inp[1], 1);
	  dup2 (redirect ? inp[1] : errp[1], 2);

	  // Use close and not myclose -- we're in the child, and we
	  // aren't worried about the possible race condition.
	  close (inp[0]);
	  close (inp[1]);
	  if (! redirect)
	    {
	      close (errp[0]);
	      close (errp[1]);
	    }
	  close (outp[0]);
	  close (outp[1]);
	  close (msgp[0]);

	  // Change directory.
	  if (path != NULL)
	    {
	      if (chdir (path) != 0)
		{
		  char c = errno;
		  write (msgp[1], &c, 1);
		  _exit (127);
		}
	    }
          // Make sure all file descriptors are closed.  In
          // multi-threaded programs, there is a race between when a
          // descriptor is obtained, when we can set FD_CLOEXEC, and
          // fork().  If the fork occurs before FD_CLOEXEC is set, the
          // descriptor would leak to the execed process if we did not
          // manually close it.  So that is what we do.  Since we
          // close all the descriptors, it is redundant to set
          // FD_CLOEXEC on them elsewhere.
          int max_fd;
#ifdef HAVE_GETRLIMIT
          rlimit rl;
          int rv = getrlimit(RLIMIT_NOFILE, &rl);
          if (rv == 0)
            max_fd = rl.rlim_max - 1;
          else
            max_fd = 1024 - 1;
#else
          max_fd = 1024 - 1;
#endif
          while(max_fd > 2)
            {
              if (max_fd != msgp[1])
                close (max_fd);
              max_fd--;
            }
	  // Make sure that SIGCHLD is unblocked for the new process.
	  sigset_t mask;
	  sigemptyset (&mask);
	  sigaddset (&mask, SIGCHLD);
	  sigprocmask (SIG_UNBLOCK, &mask, NULL);

	  execvp (args[0], args);

	  // Send the parent notification that the exec failed.
	  char c = errno;
	  write (msgp[1], &c, 1);
	  _exit (127);
	}

      // Parent.  Close extra file descriptors and mark ours as
      // close-on-exec.
      pid = (jlong) pid_tmp;

      myclose (outp[0]);
      myclose (inp[1]);
      if (! redirect)
	myclose (errp[1]);
      myclose (msgp[1]);

      char c;
      int r = read (msgp[0], &c, 1);
      if (r == -1)
        throw new IOException (JvNewStringUTF (strerror (errno)));
      else if (r != 0)
        throw new IOException (JvNewStringUTF (strerror (c)));
    }
  catch (java::lang::Throwable *thrown)
    {
      // Do some cleanup we only do on failure.  If a stream object
      // has been created, we must close the stream itself (to avoid
      // duplicate closes when the stream object is collected).
      // Otherwise we simply close the underlying file descriptor.
      // We ignore errors here as they are uninteresting.

      try
	{
	  if (inputStream != NULL)
	    inputStream->close ();
	  else
	    myclose (inp[0]);
	}
      catch (java::lang::Throwable *ignore)
	{
	}

      try
	{
	  if (outputStream != NULL)
	    outputStream->close ();
	  else
	    myclose (outp[1]);
	}
      catch (java::lang::Throwable *ignore)
	{
	}

      try
	{
	  if (errorStream != NULL)
	    errorStream->close ();
	  else if (! redirect)
	    myclose (errp[0]);
	}
      catch (java::lang::Throwable *ignore)
	{
	}

      // These are potentially duplicate, but it doesn't matter due to
      // the use of myclose.
      myclose (outp[0]);
      myclose (inp[1]);
      if (! redirect)
	myclose (errp[1]);
      myclose (msgp[1]);

      exception = thrown;
    }

  myclose (msgp[0]);
  cleanup (args, env, path);
}
