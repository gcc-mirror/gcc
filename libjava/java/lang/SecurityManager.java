/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// SecurityManager

package java.lang;

/**
 * @author Anthony Green <green@cygnus.com>
 * @date October 5, 1998.  
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 */

import java.io.*;
import java.net.*;

public abstract class SecurityManager
{
  protected boolean inCheck = false;

  public void checkAccept (String host, int port)
    {
      throw new SecurityException();
    }

  public void checkAccess (Thread thrd)
    {
      throw new SecurityException();
    }

  public void checkAccess (ThreadGroup thrdGroup)
    {
      throw new SecurityException();
    }

  public void checkAwtEventQueueAccess ()
    {
      throw new SecurityException();
    }

  public void checkConnect (String host, int prt)
    {
      throw new SecurityException();
    }

  public void checkConnect (String host, int prt, Object ctx)
    {
      throw new SecurityException();
    }

  public void checkCreateClassLoader ()
    {
      throw new SecurityException();
    }

  public void checkDelete (String fileName)
    {
      throw new SecurityException();
    }

  public void checkExec (String prog)
    {
      throw new SecurityException();
    }

  public void checkExit (int stat)
    {
      throw new SecurityException();
    }

  public void checkLink (String lib)
    {
      throw new SecurityException();
    }

  public void checkListen (int lport)
    {
      throw new SecurityException();
    }

  public void checkMemberAccess (Class cl, int mtype)
    {
      throw new SecurityException();
    }

  public void checkMulticast (InetAddress maddr)
    {
      throw new SecurityException();
    }

  public void checkMulticast (InetAddress maddr, byte ttl)
    {
      throw new SecurityException();
    }

  public void checkPackageAccess (String pkg)
    {
      throw new SecurityException();
    }

  public void checkPackageDefinition (String pkg)
    {
      throw new SecurityException();
    }

  public void checkPrintJobAccess ()
    {
      throw new SecurityException();
    }

  public void checkPropertiesAccess ()
    {
      throw new SecurityException();
    }

  public void checkPropertyAccess (String prop)
    {
      throw new SecurityException();
    }

  public void checkPropertyAccess (String prop, String defval)
    {
      throw new SecurityException();
    }

  public void checkRead (FileDescriptor fd)
    {
      throw new SecurityException();
    }

  public void checkRead (String fileName)
    {
      throw new SecurityException();
    }

  public void checkRead (String fileName, Object ctx)
    {
      throw new SecurityException();
    }

  public void checkSecurityAccess (String action)
    {
      throw new SecurityException();
    }

  public void checkSetFactory ()
    {
      throw new SecurityException();
    }

  public void checkSystemClipboardAccess ()
    {
      throw new SecurityException();
    }

  public boolean checkTopLevelWindow (Object window)
    {
      throw new SecurityException();
    }

  public void checkWrite (FileDescriptor fd)
    {
      throw new SecurityException();
    }

  public void checkWrite (String fileName)
    {
      throw new SecurityException();
    }

  // Note: this method is deprecated in JDK 1.2
  protected /* native */ int classDepth (String className)
    {
      Class[] classStack = getClassContext ();
      for (int i = 0; i < classStack.length; i++)
	if (classStack[i].getName().compareTo(className) == 0)
	  return i;

      return -1;
    }

  // Note: this method is deprecated in JDK 1.2
  protected /* native */ int classLoaderDepth ()
    {
      Class[] classStack = getClassContext ();
      for (int i = 0; i < classStack.length; i++)
	if (classStack[i].getClassLoader() != null)
	  return i;

      return -1;
    }

  protected /* native */ ClassLoader currentClassLoader ()
    {
      Class[] classStack = getClassContext ();
      for (int i = 0; i < classStack.length; i++)
	{
	  ClassLoader loader = classStack[i].getClassLoader();
	  if (loader != null)
	    return loader;
	}

      return null;
    }

  protected /* native */ Class currentLoadedClass ()
    {
      Class[] classStack = getClassContext ();
      for (int i = 0; i < classStack.length; i++)
	{
	  ClassLoader loader = classStack[i].getClassLoader();
	  if (loader != null)
	    return classStack[i];
	}
      
      return null;
    }

  protected /* native */ Class[] getClassContext ()
    {
      return new Class[0];
    }

  // Note: this method is deprecated in JDK 1.2
  public boolean getInCheck ()
    {
      return inCheck;
    }

  public Object getSecurityContext ()
    {
      // FIXME: This has yet to be implemented.
      return new String("");
    }

  public ThreadGroup getThreadGroup ()
    {
      return Thread.currentThread().getThreadGroup();
    }

  protected boolean inClass (String className)
    {
      return (classDepth (className) != -1);
    }

  protected boolean inClassLoader ()
    {
      return (classLoaderDepth () != -1);
    }

  protected SecurityManager ()
    {
      if (System.getSecurityManager () != null)
	throw new SecurityException ();
    }
}
