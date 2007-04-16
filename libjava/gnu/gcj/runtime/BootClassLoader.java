/* Copyright (C) 2005, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;

import gnu.java.net.protocol.core.Handler;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.Vector;

/**
 * This is a helper for the bootstrap class loader.  It is a
 * URLClassLoader so that we can read a class path and re-use all the
 * existing code for finding classes, extracting them from jars, etc.
 * However, it is never called the way that an ordinary ClassLoader is
 * called.  For instance, loadClass() is never used.
 */
public final class BootClassLoader extends HelperClassLoader
{
  // This forces the core URL handler to be included in statically
  // linked executables.  The line that adds core:/ to the search
  // path fails otherwise.
  static Class coreHandler = gnu.java.net.protocol.core.Handler.class;

  private boolean initialized;
  private URLClassLoader bootURLLoader;

  BootClassLoader(String libdir)
  {
    // The BootClassLoader is the top of the delegation chain. It does not
    // have a parent.
    super((ClassLoader) null);
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

  // Parse the boot classpath and create a URLClassLoader that loads
  // resources from it.  This is provided for the benefit of code that
  // does things like
  //   ClassLoader.getResourceAsStream("java/lang/Object.class")
  private synchronized URLClassLoader getBootURLLoader()
  {
    if (initialized)
      return bootURLLoader;
    initialized = true;

    Vector<URL> urls = new Vector<URL>();
    String bootClasspath = System.getProperty ("sun.boot.class.path");
    StringTokenizer st =
      new StringTokenizer(bootClasspath, File.pathSeparator);
    while (st.hasMoreTokens())
      {
	try
	  {
	    urls.add(new File(st.nextToken()).toURL());
	  }
	catch (java.net.MalformedURLException e)
	  {
	  }
      }

    if (urls.size() > 0)
      bootURLLoader = new URLClassLoader(urls.toArray(new URL[urls.size()]));
    return bootURLLoader;
  }

  public URL bootGetResource(String name)
  {
    URL url = findResource(name);
    if (url != null)
      return url;

    URLClassLoader loader = getBootURLLoader();
    if (loader != null)
      url = loader.findResource(name);

    return url;
  }

  public Enumeration bootGetResources(String name) throws IOException
  {
    URLClassLoader loader = getBootURLLoader();
    Enumeration[] e =
      {
	findResources(name),
	(loader != null) ? loader.findResources(name) : null
      };

    Vector v = new Vector();
    for (Enumeration en : e)
      if (en != null)
	while (en.hasMoreElements())
	  v.add(en.nextElement());

    return v.elements();
  }
}
