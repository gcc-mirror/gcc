/* Copyright (C) 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.StringTokenizer;

/**
 * This is a helper for the bootstrap class loader.  It is a
 * URLClassLoader so that we can read a class path and re-use all the
 * existing code for finding classes, extracting them from jars, etc.
 * However, it is never called the way that an ordinary ClassLoader is
 * called.  For instance, loadClass() is never used.
 */
public final class BootClassLoader extends URLClassLoader
{
  BootClassLoader(String libdir)
  {
    super(new URL[0]);

    // Add the contents of the endorsed directories.
    StringTokenizer st
      = new StringTokenizer (System.getProperty ("java.endorsed.dirs", ""),
			     File.pathSeparator);
    try
      {
	while (st.hasMoreElements ())
	  {
	    String dirname = st.nextToken ();
	    File dir = new File (dirname);
            if (dir.exists ())
	      {
		if (! dirname.endsWith (File.separator))
		  dirname = dirname + File.separator;
		String files[] = dir.list (new FilenameFilter ()
		  {
		    public boolean accept (File dir, String name)
		    {
		      return name.endsWith (".jar") || name.endsWith (".zip");
		    }
		  });
		for (int i = files.length - 1; i >= 0; i--)
		  addURL(new URL("file", "", -1, dirname + files[i]));
	      }
	  }

	String w3clib = (libdir + File.separator
			 + System.mapLibraryName ("w3c-gcj"));
	addURL(new URL("gcjlib", "", -1, w3clib));
	String saxlib = (libdir + File.separator
			 + System.mapLibraryName ("sax-gcj"));
	addURL(new URL("gcjlib", "", -1, saxlib));
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
