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
import java.util.StringTokenizer;

/**
 * This is a URLClassLoader that has an extra helper method for
 * handling things like java.ext.dirs.
 */
class HelperClassLoader extends URLClassLoader
{
  HelperClassLoader()
  {
    super(new URL[0]);
  }

  /**
   * This is a helper method that adds all the jar and zip files from
   * a given list of paths to this class loader.  The paths are taken
   * from a system property whose name is provided as an argument.
   */
  final void addDirectoriesFromProperty(String propName)
  {
    StringTokenizer st
      = new StringTokenizer (System.getProperty (propName, ""),
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
      }
    catch (java.net.MalformedURLException x)
      {
	// This should never happen.
	throw new RuntimeException(x);
      }
  }
}
