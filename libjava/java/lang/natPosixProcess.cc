// natPosixProcess.cc - Native side of POSIX process code.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

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
java::lang::ConcreteProcess::exitValue (void)
{
  if (! hasExited)
    {
      int wstat;
      pid_t r = waitpid ((pid_t) pid, &wstat, WNOHANG);
      if (r == -1)
	{
	  jstring x = JvNewStringLatin1 (strerror (errno));
	  throw new IllegalThreadStateException (x);
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
	throw new InterruptedException (JvNewStringLatin1 ("wait interrupted"));
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
cleanup (char **args, char **env)
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
}

static void
fail (int error_value, char **args, char **env,
      int *one = NULL, int *two = NULL,
      int *three = NULL, int *four = NULL,
      java::lang::Throwable *t = NULL)
{
  cleanup (args, env);
  if (one != NULL)
    {
      close (one[0]);
      close (one[1]);
    }
  if (two != NULL)
    {
      close (two[0]);
      close (two[1]);
    }
  if (three != NULL)
    {
      close (three[0]);
      close (three[1]);
    }
  if (four != NULL)
    {
      close (four[0]);
      close (four[1]);
    }
  if (t == NULL)
    t = new java::io::IOException (JvNewStringLatin1 (strerror (error_value)));
  throw t;
}

void
java::lang::ConcreteProcess::startProcess (jstringArray progarray,
					   jstringArray envp)
{
  using namespace java::io;

  hasExited = false;

  if (! progarray)
    throw new NullPointerException;

  // Transform arrays to native form.
  char **args = (char **) _Jv_Malloc ((progarray->length + 1)
				      * sizeof (char *));
  char **env = NULL;

  // Initialize so we can gracefully recover.
  jstring *elts = elements (progarray);
  for (int i = 0; i <= progarray->length; ++i)
    args[i] = NULL;

  try
    {
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
    }
  catch (java::lang::OutOfMemoryError *oome)
    {
      fail (0, args, env, NULL, NULL, NULL, NULL, oome);
      throw oome;
    }

  // Create pipes for I/O.  MSGP is for communicating exec() status.
  int inp[2], outp[2], errp[2], msgp[2];

  if (pipe (inp))
    fail (errno, args, env);
  if (pipe (outp))
    fail (errno, args, env, inp);
  if (pipe (errp))
    fail (errno, args, env, inp, outp);
  if (pipe (msgp))
    fail (errno, args, env, inp, outp, errp);
  if (fcntl (msgp[1], F_SETFD, FD_CLOEXEC))
    fail (errno, args, env, inp, outp, errp, msgp);

  // We create the streams before forking.  Otherwise if we had an
  // error while creating the streams we would have run the child with
  // no way to communicate with it.
  try
    {
      errorStream = new FileInputStream (new FileDescriptor (errp[0]));
      inputStream = new FileInputStream (new FileDescriptor (inp[0]));
      outputStream = new FileOutputStream (new FileDescriptor (outp[1]));
    }
  catch (java::lang::Throwable *t)
    {
      fail (0, args, env, inp, outp, errp, msgp, t);
    }

  // We don't use vfork() because that would cause the local
  // environment to be set by the child.
  if ((pid = (jlong) fork ()) == -1)
    fail (errno, args, env, inp, outp, errp, msgp);

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
      close (msgp[0]);

      execvp (args[0], args);

      // Send the parent notification that the exec failed.
      char c = errno;
      write (msgp[1], &c, 1);
      _exit (127);
    }

  // Parent.  Close extra file descriptors and mark ours as
  // close-on-exec.
  close (outp[0]);
  close (inp[1]);
  close (errp[1]);
  close (msgp[1]);

  char c;
  int r = read (msgp[0], &c, 1);
  if (r == -1)
    fail (errno, args, env, inp, outp, errp, msgp);
  else if (r != 0)
    fail (c, args, env, inp, outp, errp, msgp);

  close (msgp[0]);
  cleanup (args, env);

  fcntl (outp[1], F_SETFD, 1);
  fcntl (inp[0], F_SETFD, 1);
  fcntl (errp[0], F_SETFD, 1);
}
