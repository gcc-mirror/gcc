/* Copyright (C) 2001, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.CodeSource;
import java.util.Enumeration;
import java.util.Vector;

/**
 * A ClassLoader backed by a gcj-compiled shared library.
 * @author Per Bothner <per@bothner.com>, Brainfood Inc.
 */

public class SharedLibLoader extends ClassLoader
{
  /** Load a shared library, and associate a ClassLoader with it.
   * @param libname named of shared library (passed to dlopen)
   * @param parent the parent ClassLoader
   * @parem flags passed to dlopen
   */
  public SharedLibLoader(String libname, ClassLoader parent, int flags)
  {
    super(parent);
    URL url;
    try
      {
	url = new URL("file", "", libname);
      }
    catch (MalformedURLException _)
      {
	url = null;
      }
    helper = SharedLibHelper.findHelper(this, libname,
					new CodeSource(url, null));
  }

  /** Load a shared library, and asociate a ClassLoader with it.
   * @param libname named of shared library (passed to dlopen)
   */
  public SharedLibLoader(String libname)
  {
    this(libname, getSystemClassLoader(), 0);
  }

  public Class findClass(String name)
    throws ClassNotFoundException
  {
    Class cls = helper.findClass(name);
    if (cls == null)
      throw new ClassNotFoundException(name);
    return cls;
  }

  public URL findResource (String name)
  {
    return helper.findResource(name);
  }

  public Enumeration findResources (String name) throws IOException
  {
    URL url = findResource(name);
    if (url == null)
      return null;
    Vector v = new Vector(1);
    v.add(url);
    return v.elements();
  }

  /** The helper that does the work for us.  */
  SharedLibHelper helper;
}
