// natPosixProcess.cc - Native side of POSIX process code.

/* Copyright (C) 1998, 1999, 2000, 2002, 2003, 2004  Free Software Foundation

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
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/lang/ConcreteProcess$ProcessManager.h>
#include <java/lang/ConcreteProcess.h>
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

using gnu::java::nio::channels::FileChannelImpl;

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

// There has to be a signal handler in order to be able to
// sigwait() on SIGCHLD.  The information passed is ignored as it
// will be recovered by the waitpid() call.
static void
sigchld_handler (int)
{
  // Ignore.
}


// Get ready to enter the main reaper thread loop.
void
java::lang::ConcreteProcess$ProcessManager::init ()
{
  using namespace java::lang;
  // Remenber our PID so other threads can kill us.
  reaperPID = (jlong) pthread_self ();

  // SIGCHLD is blocked in all threads in posix-threads.cc.
  // Setup the SIGCHLD handler.
  struct sigaction sa;
  memset (&sa, 0, sizeof (sa));

  sa.sa_handler = sigchld_handler;
  // We only want signals when the things exit.
  sa.sa_flags = SA_NOCLDSTOP;

  if (-1 == sigaction (SIGCHLD, &sa, NULL))
    goto error;

  // All OK.
  return;

error:
  throw new InternalError (JvNewStringUTF (strerror (errno)));
}

void
java::lang::ConcreteProcess$ProcessManager::waitForSignal ()
{
  // Wait for SIGCHLD
  sigset_t mask;
  pthread_sigmask (0, NULL, &mask);
  sigdelset (&mask, SIGCHLD);

  // Use sigsuspend() instead of sigwait() as sigwait() doesn't play
  // nicely with the GC's use of signals.
  sigsuspend (&mask);

  // Do not check sigsuspend return value.  The only legitimate return
  // is EINTR, but there is a known kernel bug affecting alpha-linux
  // wrt sigsuspend+handler+sigreturn that can result in a return value
  // of __NR_sigsuspend and errno unset.  Don't fail unnecessarily on
  // older kernel versions.

  // All OK.
  return;
}

jboolean java::lang::ConcreteProcess$ProcessManager::reap ()
{
  using namespace java::lang;

  pid_t pid;

  for (;;)
    {
      // Get the return code from a dead child process.
      int status;
      pid = waitpid ((pid_t) - 1, &status, WNOHANG);
      if (pid == -1)
	{
	  if (errno == ECHILD)
	    return false;
	  else
	    goto error;
	}

      if (pid == 0)
        return true;   // No children to wait for.

      // Look up the process in our pid map.
      ConcreteProcess * process = removeProcessFromMap ((jlong) pid);

      if (process)
	{
	  JvSynchronize sync (process);
	  process->status = WIFEXITED (status) ? WEXITSTATUS (status) : -1;
	  process->state = ConcreteProcess::STATE_TERMINATED;
          process->processTerminationCleanup();
	  process->notifyAll ();
	}
      else
	{
	  // Unknown child.  How did this happen?
	  fprintf (stderr, "Reaped unknown child pid = %ld\n", (long) pid);
	}
    }

error:
  throw new InternalError (JvNewStringUTF (strerror (errno)));
}

void
java::lang::ConcreteProcess$ProcessManager::signalReaper ()
{
  int c = pthread_kill ((pthread_t) reaperPID, SIGCHLD);
  if (c == 0)
    return;
  // pthread_kill() failed.
  throw new InternalError (JvNewStringUTF (strerror (c)));
}

void
java::lang::ConcreteProcess::nativeDestroy ()
{
  int c = kill ((pid_t) pid, SIGKILL);
  if (c == 0)
    return;
  // kill() failed.
  throw new InternalError (JvNewStringUTF (strerror (errno)));
}

void
java::lang::ConcreteProcess::nativeSpawn ()
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
	  env = (char **) _Jv_Malloc ((envp->length + 1) * sizeof (char *));
	  elts = elements (envp);

	  // Initialize so we can gracefully recover.
	  for (int i = 0; i <= envp->length; ++i)
	    env[i] = NULL;

	  for (int i = 0; i < envp->length; ++i)
	    env[i] = new_string (elts[i]);
	  env[envp->length] = NULL;
	}

      // We allocate this here because we can't call malloc() after
      // the fork.
      if (dir != NULL)
	path = new_string (dir->getPath ());

      // Create pipes for I/O.  MSGP is for communicating exec()
      // status.
      if (pipe (inp) || pipe (outp) || pipe (errp) || pipe (msgp)
	  || fcntl (msgp[1], F_SETFD, FD_CLOEXEC))
      throw new IOException (JvNewStringUTF (strerror (errno)));

      // We create the streams before forking.  Otherwise if we had an
      // error while creating the streams we would have run the child
      // with no way to communicate with it.
    errorStream =
      new FileInputStream (new
                           FileChannelImpl (errp[0], FileChannelImpl::READ));
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
	    {
	      // Preserve PATH and LD_LIBRARY_PATH unless specified
	      // explicitly.
	      char *path_val = getenv ("PATH");
	      char *ld_path_val = getenv ("LD_LIBRARY_PATH");
	      environ = env;
	      if (path_val && getenv ("PATH") == NULL)
		{
		char *path_env =
                  (char *) _Jv_Malloc (strlen (path_val) + 5 + 1);
		  strcpy (path_env, "PATH=");
		  strcat (path_env, path_val);
		  putenv (path_env);
		}
	      if (ld_path_val && getenv ("LD_LIBRARY_PATH") == NULL)
		{
		char *ld_path_env =
                  (char *) _Jv_Malloc (strlen (ld_path_val) + 16 + 1);
		  strcpy (ld_path_env, "LD_LIBRARY_PATH=");
		  strcat (ld_path_env, ld_path_val);
		  putenv (ld_path_env);
		}
	    }

	  // We ignore errors from dup2 because they should never occur.
	  dup2 (outp[0], 0);
	  dup2 (inp[1], 1);
	  dup2 (errp[1], 2);

	  // Use close and not myclose -- we're in the child, and we
	  // aren't worried about the possible race condition.
	  close (inp[0]);
	  close (inp[1]);
	  close (errp[0]);
	  close (errp[1]);
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
	  else
	    myclose (errp[0]);
	}
      catch (java::lang::Throwable *ignore)
	{
	}

      // These are potentially duplicate, but it doesn't matter due to
      // the use of myclose.
      myclose (outp[0]);
      myclose (inp[1]);
      myclose (errp[1]);
      myclose (msgp[1]);

    exception = thrown;
    }

  myclose (msgp[0]);
  cleanup (args, env, path);

  if (exception == NULL)
    {
      fcntl (outp[1], F_SETFD, FD_CLOEXEC);
      fcntl (inp[0], F_SETFD, FD_CLOEXEC);
      fcntl (errp[0], F_SETFD, FD_CLOEXEC);
    }
}
