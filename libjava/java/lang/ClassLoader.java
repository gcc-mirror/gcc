// ClassLoader.java - Define policies for loading Java classes.

/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.lang;
import java.io.InputStream;
import java.util.Hashtable;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 28, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * Status: Just a stub; not useful at all.
 */

public abstract class ClassLoader
{
  protected ClassLoader ()
    {
      cache = new Hashtable ();
    }

  protected final Class defineClass (String className, byte[] bytecode,
				     int offset, int length)
    {
      throw new ClassFormatError ("defineClass unimplemented");
    }

  protected final Class defineClass (byte[] bytecodes,
				     int offset, int length)
    {
      return defineClass (null, bytecodes, offset, length);
    }

  protected final Class findLoadedClass (String className)
    {
      return (Class) cache.get(className);
    }

  protected final Class findSystemClass (String className)
    throws ClassNotFoundException
    {
      Class c = system.findLoadedClass(className);
      system.resolveClass(c);
      return c;
    }

  // FIXME: Needs URL.
  // public URL getResource (String resName);

  public InputStream getResourceAsStream (String resName)
    {
      return null;
    }

  // FIXME: Needs URL.
  // public static final URL getSystemResource (String resName);

  public static final InputStream getSystemResourceAsStream (String resName)
    {
      return null;
    }

  protected abstract Class loadClass (String className, boolean resolve)
    throws ClassNotFoundException;
  public Class loadClass (String name) throws ClassNotFoundException
    {
      return loadClass (name, true);
    }

  protected final void resolveClass (Class c)
    {
      // Nothing for now.
    }

  protected final void setSigners (Class cl, Object[] signers)
    {
      // Nothing for now.
    }

  // Class cache.
  private Hashtable cache;

  // The system class loader.  FIXME: should have an actual value
  private static final ClassLoader system = null;
}
