/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

package gnu.gcj.runtime;

import java.io.*;
import java.util.StringTokenizer;
import java.net.URL;

final class VMClassLoader extends java.net.URLClassLoader
{
  private VMClassLoader ()
  {	
    super (init());
  }

  private static URL[] init() 
  {
    StringTokenizer st
	= new StringTokenizer (System.getProperty ("java.class.path", "."),
			       System.getProperty ("path.separator", ":"));

    java.util.Vector p = new java.util.Vector();
    while (st.hasMoreElements ()) 
      {  
	String e = st.nextToken ();
	try
	  {
	    if (e.endsWith(".jar") || e.endsWith (".zip"))
	      p.addElement(new URL("jar", "", -1, "file:///"+e+"!/"));
	    else if (e.endsWith ("/"))
	      p.addElement (new URL("file", "", -1, e));
	    else if (new File (e).isDirectory ())
	      p.addElement (new URL("file", "", -1, e + "/"));
	    else
	      /* Ignore path element. */;
	  } 
	catch (java.net.MalformedURLException x)
	  {
	    /* Ignore this path element */
	  }
      }

    URL[] urls = new URL[p.size()];
    p.copyInto (urls);
    return urls;
  }

  /** This is overridden to search the internal hash table, which 
   * will only search existing linked-in classes.   This will make
   * the default implementation of loadClass (in ClassLoader) work right.
   */
  protected final native Class findSystemClass(String name) 
    throws java.lang.ClassNotFoundException, java.lang.LinkageError;

  // Return the sole VMClassLoader.
  private static synchronized VMClassLoader getVMClassLoader ()
  {
    if (redirect == null)
      redirect = new VMClassLoader ();
    return redirect;
  }

  // The only VMClassLoader that can exist.
  private static VMClassLoader redirect;
}
