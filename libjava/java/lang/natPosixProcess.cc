// natPosixProcess.cc - Native side of POSIX process code.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

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
#include <java/io/FileDescriptor.h>
#include <java/io/FileInputStream.h>
#include <java/io/FileOutputStream.h>
#include <java/io/IOException.h>

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
java::lang::ConcreteProcess::exitValue (void)
{
  if (! hasExited)
    {
      int wstat;
      pid_t r = waitpid ((pid_t) pid, &wstat, WNOHANG);
      if (r == -1)
	{
	  jstring x = JvNewStringLatin1 (strerror (errno));
	  _Jv_Throw (new IllegalThreadStateException (x));
	}

      hasExited = true;
      // Just use the raw status.  FIXME: what is right?
      status = wstat;
    }

  return status;
}

jint
java::lang::ConcreteProcess::waitFor (void)
{
  if (! hasExited)
    {
      int wstat;
      int r = waitpid ((pid_t) pid, &wstat, 0);

      if (r != -1)
	{
	  hasExited = true;
	  // Just use the raw status.  FIXME: what is right?
	  status = wstat;
	}

      if (java::lang::Thread::interrupted())
	_Jv_Throw (new InterruptedException (JvNewStringLatin1 ("wait interrupted")));
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

void
java::lang::ConcreteProcess::startProcess (jstringArray progarray,
					   jstringArray envp)
{
  using namespace java::io;

  hasExited = false;

  if (! progarray)
    _Jv_Throw (new NullPointerException);

  // Transform arrays to native form.
  // FIXME: we use malloc here.  We shouldn't.  If an exception is
  // thrown we will leak memory.
  char **args = (char **) _Jv_Malloc ((progarray->length + 1)
				      * sizeof (char *));
  char **env = NULL;

  // FIXME: GC will fail here if _Jv_Malloc throws an exception.
  // That's because we have to manually free the contents, but we 
  jstring *elts = elements (progarray);
  for (int i = 0; i < progarray->length; ++i)
    args[i] = new_string (elts[i]);
  args[progarray->length] = NULL;

  if (envp)
    {
      env = (char **) _Jv_Malloc ((envp->length + 1) * sizeof (char *));
      elts = elements (envp);
      for (int i = 0; i < envp->length; ++i)
	env[i] = new_string (elts[i]);
      env[envp->length] = NULL;
    }

  // Create pipes for I/O.
  int inp[2], outp[2], errp[2];

  if (pipe (inp)
      || pipe (outp)
      || pipe (errp))
    {
    ioerror:
      // FIXME.
      _Jv_Free (args);
      if (env)
	_Jv_Free (env);
      _Jv_Throw (new IOException (JvNewStringLatin1 (strerror (errno))));
    }

  // We create the streams before forking.  Otherwise if we had an
  // error while creating the streams we would have run the child with
  // no way to communicate with it.
  errorStream = new FileInputStream (new FileDescriptor (errp[0]));
  inputStream = new FileInputStream (new FileDescriptor (inp[0]));
  outputStream = new FileOutputStream (new FileDescriptor (outp[1]));

  // We don't use vfork() because that would cause the local
  // environment to be set by the child.
  if ((pid = (jlong) fork ()) == -1)
    goto ioerror;

  if (pid == 0)
    {
      // Child process, so remap descriptors and exec.

      if (envp)
        {
	  // preserve PATH unless specified explicitly
	  char *path_val = getenv ("PATH");
	  environ = env;
	  if (getenv ("PATH") == NULL)
	    {
	      char *path_env = (char *) _Jv_Malloc (strlen (path_val) + 5 + 1);
	      strcpy (path_env, "PATH=");
	      strcat (path_env, path_val);
	      putenv (path_env);
	    }
	}
	
      // We ignore errors from dup2 because they should never occur.
      dup2 (outp[0], 0);
      dup2 (inp[1], 1);
      dup2 (errp[1], 2);

      close (inp[0]);
      close (inp[1]);
      close (errp[0]);
      close (errp[1]);
      close (outp[0]);
      close (outp[1]);

      execvp (args[0], args);
      // FIXME: should throw an IOException if execvp() fails. Not trivial,
      // because _Jv_Throw won't work from child process
      _exit (127);
    }

  // Parent.  Close extra file descriptors and mark ours as
  // close-on-exec.
  close (outp[0]);
  close (inp[1]);
  close (errp[1]);

  fcntl (outp[1], F_SETFD, 1);
  fcntl (inp[0], F_SETFD, 1);
  fcntl (errp[0], F_SETFD, 1);
}
