/* Copyright (C) 2001, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;
import java.lang.ref.WeakReference;
import java.net.URL;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.security.*;
import gnu.gcj.Core;

public class SharedLibHelper
{
  /** Load a shared library, and associate a ClassLoader with it.
   * @param libname named of shared library (passed to dlopen)
   * @param parent the parent ClassLoader
   * @parem flags passed to dlopen
   */
  SharedLibHelper(String libname, ClassLoader parent, CodeSource source,
		  int flags)
  {
    // FIXME: ask security manager first.
    loader = parent;
    baseName = libname;
    domain = new ProtectionDomain(source,
				  Policy.getPolicy().getPermissions(source));
    this.flags = flags;
  }

  public static SharedLibHelper findHelper (String libname)
  {
    synchronized (map)
      {
	WeakReference ref = (WeakReference) map.get(libname);
	if (ref != null)
	  return (SharedLibHelper) ref.get();
	return null;
      }
  }

  public static SharedLibHelper findHelper (ClassLoader loader, String libname,
					    CodeSource source)
  {
    synchronized (map)
      {
	SharedLibHelper result;
	WeakReference ref = (WeakReference) map.get(libname);
	if (ref != null)
	  {
	    result = (SharedLibHelper) ref.get();
	    if (result != null)
	      {
		if (result.loader != loader)
		  // FIXME
		  throw new UnknownError();
		return result;
	      }
	  }

	result = new SharedLibHelper(libname, loader, source, 0);
	map.put(libname, new WeakReference(result));
	return result;
      }
  }

  public native void finalize ();

  public Class findClass(String name)
  {
    ensureInit();
    return (Class) classMap.get(name);
  }

  public URL findResource (String name)
  {
    ensureInit();
    if (! hasResource(name))
      return null;
    try
      {
	return new URL("gcjlib", "", -1, baseName + "!/" + name);
      }
    catch (MalformedURLException _)
      {
      }
    return null;
  }

  public native Core findCore (String name);

  void ensureInit()
  {
    synchronized (classMap)
      {
	if (initialized)
	  return;
	init();
	initialized = true;
      }
  }

  native boolean hasResource(String name);
  native void init();

  /** Called during dlopen's processing of the init section. */
  void registerClass(String name, Class cls)
  {
    classMap.put(name, cls);
  }

  /** The handle returned by dlopen. */
  gnu.gcj.RawData handler;

  /** Holds a _Jv_core_chain for the loader.  */
  gnu.gcj.RawData core_chain;

  /** Map classnames to Classes. */
  HashMap classMap = new HashMap(20);

  /** Class loader we're helping.  */
  ClassLoader loader;

  /** Name of base file.  */
  String baseName;

  /** Protection domain for loaded classes.  */
  ProtectionDomain domain;

  /** Flags to pass to dlopen.  FIXME: platform dependent.
      0 is always "sensible" (defined by us).  */
  int flags;

  /** True if we've been initialized.  */
  boolean initialized = false;

  /** Map shared library names to a helper object.  This uses weak
      references in the values so we don't prevent collection.  */
  static HashMap map = new HashMap ();
}
