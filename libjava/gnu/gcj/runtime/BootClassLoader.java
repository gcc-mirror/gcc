/* Copyright (C) 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;

import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;

/**
 * This is a helper for the bootstrap class loader.  It is a
 * URLClassLoader so that we can read a class path and re-use all the
 * existing code for finding classes, extracting them from jars, etc.
 * However, it is never called the way that an ordinary ClassLoader is
 * called.  For instance, loadClass() is never used.
 */
public final class BootClassLoader extends HelperClassLoader
{
  BootClassLoader(String libdir)
  {
    addDirectoriesFromProperty("java.endorsed.dirs");
    addDirectoriesFromProperty("gnu.gcj.runtime.endorsed.dirs");

    try
      {
	// Add core:/ to the end so any resources compiled into this
	// executable may be found.
	addURL(new URL("core", "", -1, "/"));
      }
    catch (java.net.MalformedURLException x)
      {
	// This should never happen.
	throw new RuntimeException(x);
      }
  }

  public Class bootLoadClass(String name)
    throws ClassNotFoundException
  {
    Class c = findLoadedClass(name);
    if (c == null)
      {
	try
	  {
	    // We could hack URLClassLoader to make this more
	    // efficient, if it mattered.
	    c = findClass(name);
	  }
	catch (ClassNotFoundException _)
	  {
	    c = null;
	  }
      }
    return c;
  }

  public URL bootGetResource(String name)
  {
    return findResource(name);
  }

  public Enumeration bootGetResources(String name) throws IOException
  {
    return findResources(name);
  }
}
