/* Copyright (C) 2005  Free Software Foundation

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

  // We add the URLs to the system class loader late.  The reason for
  // this is that during bootstrap we don't want to parse URLs or
  // create URL connections, since that will result in circularities
  // causing a crash.
  void init()
  {
    StringTokenizer st
      = new StringTokenizer (System.getProperty ("java.class.path", "."),
			     File.pathSeparator);
    while (st.hasMoreElements ()) 
      {  
	String e = st.nextToken ();
	try
	  {
	    if ("".equals(e))
	      e = ".";

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
  }
}
