/* Copyright (C) 1999, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

package gnu.gcj.runtime;

import java.io.*;
import java.util.StringTokenizer;
import java.net.URL;

public final class VMClassLoader extends java.net.URLClassLoader
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
	      {
		File archive = new File (e);
		try {
		  p.addElement(new URL("jar", "", -1, "file://"
				       + archive.getCanonicalPath ()
				       + "!/"));
		} catch (IOException ex) {
		  // empty
		}
	      }
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
    // Add core:/ to the end of the java.class.path so any resources
    // compiled into this executable may be found.
    try
      {
	p.addElement (new URL("core", "", -1, "/"));
      }
    catch (java.net.MalformedURLException x)
      {
	// This should never happen.
      }

    URL[] urls = new URL[p.size()];
    p.copyInto (urls);
    return urls;
  }

  /** This is overridden to search the internal hash table, which 
   * will only search existing linked-in classes.   This will make
   * the default implementation of loadClass (in ClassLoader) work right.
   * The implementation of this method is in java/lang/natClassLoader.cc.
   */
  protected native Class findClass(String name) 
    throws java.lang.ClassNotFoundException;

  // The only VMClassLoader that can exist.
  public static VMClassLoader instance = new VMClassLoader ();
}
