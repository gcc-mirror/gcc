/* Copyright (C) 2005, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;

import java.io.*;
import java.util.StringTokenizer;
import java.util.HashSet;
import java.net.URL;
import java.net.URLClassLoader;

public final class SystemClassLoader extends URLClassLoader
{
  SystemClassLoader(ClassLoader parent)
  {
    super(new URL[0], parent);
  }

  // This is called to register a native class which was linked into
  // the application but which is registered with the system class
  // loader after the VM is initialized.
  void addClass(Class klass)
  {
    String packageName = null;
    String className = klass.getName();
    int lastDot = className.lastIndexOf('.');
    if (lastDot != -1)
      packageName = className.substring(0, lastDot);
    if (packageName != null && getPackage(packageName) == null)
      {
	// Should have some way to store this information in a
	// precompiled manifest.
	definePackage(packageName, null, null, null, null, null, null, null);
      }
    loadedClasses.put(className, klass);
  }

  // We add the URLs to the system class loader late.  The reason for
  // this is that during bootstrap we don't want to parse URLs or
  // create URL connections, since that will result in circularities
  // causing a crash.
  void init()
  {
    String sep = File.pathSeparator;
    StringTokenizer st
      = new StringTokenizer (System.getProperty ("java.class.path", "."),
			     sep, true);
    // Pretend we start with a ':', so if we see a ':' first we add
    // '.'.
    boolean last_was_sep = true;
    while (st.hasMoreElements ()) 
      {  
	String e = st.nextToken ();
	try
	  {
	    if (sep.equals(e))
	      {
		if (last_was_sep)
		  {
		    // We saw two separators in a row, so add ".".
		    addURL(new URL("file", "", -1, "./"));
		    last_was_sep = false;
		  }
		else
		  last_was_sep = true;
		continue;
	      }

	    last_was_sep = false;
	    File path = new File(e);
	    // Ignore invalid paths.
	    if (!path.exists())
	      continue;
	    if (!e.endsWith (File.separator) && path.isDirectory ())
	      addURL(new URL("file", "", -1, e + File.separator));
	    else
	      addURL(new URL("file", "", -1, e));
	  } 
	catch (java.net.MalformedURLException x)
	  {
	    // This should never happen.
	    throw new RuntimeException(x);
	  }
      }
    // If we saw a trailing ":", add "." to the path.
    if (last_was_sep)
      {
	try
	  {
	    addURL(new URL("file", "", -1, "./"));
	  }
	catch (java.net.MalformedURLException x)
	  {
	    // This should never happen.
	    throw new RuntimeException(x);
	  }
      }
  }
}
