/* Copyright (C) 2001, 2003, 2004, 2005  Free Software Foundation

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
import java.util.Set;
import java.util.Iterator;
import java.util.HashSet;
import java.util.HashMap;
import java.nio.channels.FileChannel;
import java.io.*;

public class SharedLibHelper
{
  /** Load a shared library, and associate a ClassLoader with it.
   * @param libname named of shared library (passed to dlopen)
   * @param parent the parent ClassLoader
   * @parem flags passed to dlopen
   */
  SharedLibHelper(String libname, ClassLoader parent, CodeSource source,
		  ProtectionDomain domain, int flags)
  {
    // FIXME: ask security manager first.
    loader = parent;
    baseName = libname;
    if (domain == null)
      domain = new ProtectionDomain(source,
				    Policy.getPolicy().getPermissions(source));
    this.domain = domain;
    this.flags = flags;
  }

  public static SharedLibHelper findHelper (String libname)
  {
    synchronized (map)
      {
	Set s = (Set)map.get(libname);
	if (s == null)
	  return null;
	for (Iterator i=s.iterator(); i.hasNext();)
	  {
	    WeakReference ref = (WeakReference)i.next();
	    if (ref != null)
	      return (SharedLibHelper) ref.get();
	  }
	return null;
      }
  }

  static void copyFile (File in, File out) throws IOException 
  {
    FileChannel source = new FileInputStream(in).getChannel();
    FileChannel destination = new FileOutputStream(out).getChannel();
    source.transferTo(0, source.size(), destination);
    source.close();
    destination.close();
  }

  public static SharedLibHelper findHelper (ClassLoader loader, String libname,
					    CodeSource source,
					    boolean tryParents)
  {
    return findHelper (loader, libname, source, null, tryParents);
  }

  public static SharedLibHelper findHelper (ClassLoader loader, String libname,
					    CodeSource source,
					    ProtectionDomain domain, 
					    boolean tryParents)
  {
    synchronized (map)
      {
	SharedLibHelper result;
	Set s = (Set)map.get(libname);
	if (s == null)
	  {
	    s = new HashSet();
	    map.put(libname, s);
	  }
	else
	  {
	    for (Iterator i=s.iterator(); i.hasNext();)
	      {
		WeakReference ref = (WeakReference)i.next();
		if (ref != null)
		  {
		    result = (SharedLibHelper) ref.get();
		    if (result != null)
		      {			
			// A match succeeds if the library is already
			// loaded by LOADER or any of its ancestors.
			ClassLoader l = loader;
			do
			  {
			    if (result.loader == l)
			      return result;
			    l = l.getParent();
			  }
			while (tryParents && l != null);
		      }
		  }
	      }

	    // Oh dear.  We've already mapped this shared library, but
	    // with a different class loader.  We need to copy it.
	    try
	      {
		File copy 
		  = File.createTempFile(new File(libname).getName(), 
					".so", new File ("/tmp"));
		File src = new File(libname);
		copyFile (src, copy);
		copy.deleteOnExit();
		libname = copy.getPath();
	      }
	    catch (IOException e)
	      {
		return null;
	      }
	  }
	result = new SharedLibHelper(libname, loader, source, domain, 0);
	s.add(new WeakReference(result));
	return result;
      }
  }

  public native void finalize ();

  public Class findClass(String name)
  {
    ensureInit();
    Class result = (Class) classMap.get(name);
    if (result != null)
      {
	// We never want to return a class without its supers linked.
	// It isn't clear from the spec, but this is what other
	// implementations do in practice.
	ensureSupersLinked(result);
      }
    return result;
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
  native void ensureSupersLinked(Class k);

  public String toString ()
  {
    return "shared object " + baseName;
  }

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
