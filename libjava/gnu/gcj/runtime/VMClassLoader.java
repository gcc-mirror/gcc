/* Copyright (C) 1999, 2001, 2002, 2003, 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

package gnu.gcj.runtime;

import java.io.*;
import java.util.StringTokenizer;
import java.util.HashSet;
import java.net.URL;

public final class VMClassLoader extends java.net.URLClassLoader
{
  private VMClassLoader ()
  {	
    super (new URL[0]);
    String p
      = System.getProperty ("gnu.gcj.runtime.VMClassLoader.library_control",
			    "");
    if ("never".equals(p))
      lib_control = LIB_NEVER;
    else if ("cache".equals(p))
      lib_control = LIB_CACHE;
    else if ("full".equals(p))
      {
	// In case we ever want to change the default.
	lib_control = LIB_FULL;
      }
    else
      lib_control = LIB_FULL;
  }

  private void init() 
  {
    StringTokenizer st
	= new StringTokenizer (System.getProperty ("java.class.path", "."),
			       System.getProperty ("path.separator", ":"));

    while (st.hasMoreElements ()) 
      {  
	String e = st.nextToken ();
	try
	  {
	    if (!e.endsWith (File.separator) && new File (e).isDirectory ())
	      addURL(new URL("file", "", -1, e + File.separator));
	    else
	      addURL(new URL("file", "", -1, e));
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
	addURL(new URL("core", "", -1, "/"));
      }
    catch (java.net.MalformedURLException x)
      {
	// This should never happen.
      }
  }

  /** This is overridden to search the internal hash table, which 
   * will only search existing linked-in classes.   This will make
   * the default implementation of loadClass (in ClassLoader) work right.
   * The implementation of this method is in java/lang/natClassLoader.cc.
   */
  protected native Class findClass(String name) 
    throws java.lang.ClassNotFoundException;

  // This can be package-private because we only call it from native
  // code during startup.
  static void initialize ()
  {
    instance.init();
  }

  // This keeps track of shared libraries we've already tried to load.
  private HashSet tried_libraries = new HashSet();

  // Holds one of the LIB_* constants; used to determine how shared
  // library loads are done.
  private int lib_control;

  // The only VMClassLoader that can exist.
  static VMClassLoader instance = new VMClassLoader();

  private static final int LIB_FULL = 0;
  private static final int LIB_CACHE = 1;
  private static final int LIB_NEVER = 2;
}
