// natPosixProcess.cc - Native side of POSIX process code.

/* Copyright (C) 1998, 1999, 2000, 2002  Free Software Foundation

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

#include <gcj/cni.h>
#include <jvm.h>

#include <java/lang/ConcreteProcess.h>
#include <java/lang/IllegalThreadStateException.h>
#include <java/lang/InterruptedException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/Thread.h>
#include <java/io/File.h>
#include <java/io/FileDescriptor.h>
#include <java/io/FileInputStream.h>
#include <java/io/FileOutputStream.h>
#include <java/io/IOException.h>
#include <java/lang/OutOfMemoryError.h>

extern char **environ;

void
java::lang::ConcreteProcess::destroy (void)
{
  if (! hasExited)
    {
      // Really kill it.
      kill ((pid_t) pid, SIGKILL);
    }
}

jint
java::lang::ConcreteProcess::waitFor (void)
{
  if (! hasExited)
    {
      int wstat;
      int r = waitpid ((pid_t) pid, &wstat, 0);

      if (r == -1)
        {
	  if (java::lang::Thread::interrupted())
	    throw new InterruptedException (JvNewStringLatin1 (strerror
	      (errno)));
	}
      else
	{
	  hasExited = true;

	  if (WIFEXITED (wstat))
	    status = WEXITSTATUS (wstat);
	  else
	    status = -1;
	}
    }

  return status;
}

static char *
new_string (jstring string)
{
  jsize s = _Jv_GetStringUTFLength (string);
  char *buf = (char *) _Jv_Malloc (s + 1);
  _Jv_GetStringUTFRegion (string, 0, s, buf);
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

void
java::lang::ConcreteProcess::startProcess (jstringArray progarray,
					   jstringArray envp,
					   java::io::File *dir)
{
  using namespace java::io;

  hasExited = false;

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
  java::lang::Throwable *exc = NULL;
  errorStream = NULL;
  inputStream = NULL;
  outputStream = NULL;

  try
    {
      // Transform arrays to native form.
      args = (char **) _Jv_Malloc ((progarray->length + 1)
				   * sizeof (char *));

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
	throw new IOException (JvNewStringLatin1 (strerror (errno)));

      // We create the streams before forking.  Otherwise if we had an
      // error while creating the streams we would have run the child
      // with no way to communicate with it.
      errorStream = new FileInputStream (new FileDescriptor (errp[0]));
      inputStream = new FileInputStream (new FileDescriptor (inp[0]));
      outputStream = new FileOutputStream (new FileDescriptor (outp[1]));

      // We don't use vfork() because that would cause the local
      // environment to be set by the child.
      if ((pid = (jlong) fork ()) == -1)
	throw new IOException (JvNewStringLatin1 (strerror (errno)));

      if (pid == 0)
	{
	  // Child process, so remap descriptors, chdir and exec.

	  if (envp)
	    {
	      // Preserve PATH and LD_LIBRARY_PATH unless specified
	      // explicitly.
	      char *path_val = getenv ("PATH");
	      char *ld_path_val = getenv ("LD_LIBRARY_PATH");
	      environ = env;
	      if (getenv ("PATH") == NULL)
		{
		  char *path_env = (char *) _Jv_Malloc (strlen (path_val)
							+ 5 + 1);
		  strcpy (path_env, "PATH=");
		  strcat (path_env, path_val);
		  putenv (path_env);
		}
	      if (getenv ("LD_LIBRARY_PATH") == NULL)
		{
		  char *ld_path_env
		    = (char *) _Jv_Malloc (strlen (ld_path_val) + 16 + 1);
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
      myclose (outp[0]);
      myclose (inp[1]);
      myclose (errp[1]);
      myclose (msgp[1]);

      char c;
      int r = read (msgp[0], &c, 1);
      if (r == -1)
	throw new IOException (JvNewStringLatin1 (strerror (errno)));
      else if (r != 0)
	throw new IOException (JvNewStringLatin1 (strerror (c)));
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

      exc = thrown;
    }

  myclose (msgp[0]);
  cleanup (args, env, path);

  if (exc != NULL)
    throw exc;
  else
    {
      fcntl (outp[1], F_SETFD, FD_CLOEXEC);
      fcntl (inp[0], F_SETFD, FD_CLOEXEC);
      fcntl (errp[0], F_SETFD, FD_CLOEXEC);
    }
}
