/* Copyright (C) 2001, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;
import java.util.Hashtable;

/**
 * A ClassLoader backed by a gcj-compiled shared library.
 * @author Per Bothner <per@bothner.com>, Brainfood Inc.
 */

public class SharedLibLoader extends ClassLoader
{
  public native void finalize ();

  /** Called during dlopen's processing of the init section. */
  void registerClass(String name, Class cls)
  {
    classMap.put(name, cls);
  }

  /** Load a shared library, and associate a ClassLoader with it.
   * @param libname named of shared library (passed to dlopen)
   * @param parent the parent ClassLoader
   * @param flags passed to dlopen
   */
  public SharedLibLoader(String libname, ClassLoader parent, int flags)
  {
    super(parent);
    init(libname, flags);
  }


  /** Load a shared library, and asociate a ClassLoader with it.
   * @param libname named of shared library (passed to dlopen)
   */
  public SharedLibLoader(String libname)
  {
    super(getSystemClassLoader());
    init(libname, 0);
  }

  void init(String libname, int flags)
  {
    init(libname.getBytes(), flags);
  }

  native void init(byte[] libname, int flags);

  public Class loadClass(String name)
    throws ClassNotFoundException
  {
    return super.loadClass(name);
  }

  public Class findClass(String name)
    throws ClassNotFoundException
  {
    Object cls = classMap.get(name);
    if (cls == null)
      throw new ClassNotFoundException(name);
    return (Class) cls;
  }

  /** The handle returned by dlopen. */
  gnu.gcj.RawData handler;

  /** Map classnames to Classes. */
  Hashtable classMap = new Hashtable(20);
}
