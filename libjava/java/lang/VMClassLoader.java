/* VMClassLoader.java -- Reference implementation of native interface
   required by ClassLoader
   Copyright (C) 1998, 2001, 2002, 2003, 2004, 2005 Free Software Foundation

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package java.lang;

import gnu.java.util.EmptyEnumeration;
import java.lang.reflect.Constructor;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.security.AllPermission;
import java.security.Permission;
import java.security.Permissions;
import java.security.ProtectionDomain;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.StringTokenizer;
import gnu.gcj.runtime.BootClassLoader;

/**
 * java.lang.VMClassLoader is a package-private helper for VMs to implement
 * on behalf of java.lang.ClassLoader.
 *
 * @author John Keiser
 * @author Mark Wielaard <mark@klomp.org>
 * @author Eric Blake <ebb9@email.byu.edu>
 */
final class VMClassLoader
{
  // Protection Domain definitions 
  // FIXME: should there be a special protection domain used for native code?
  
  // The permission required to check what a classes protection domain is.
  static final Permission protectionDomainPermission
    = new RuntimePermission("getProtectionDomain");
  // The protection domain returned if we cannot determine it. 
  static ProtectionDomain unknownProtectionDomain;

  static
  {
    Permissions permissions = new Permissions();
    permissions.add(new AllPermission());
    unknownProtectionDomain = new ProtectionDomain(null, permissions);  
  }

  static final HashMap definedPackages = new HashMap();

  // This is a helper for handling java.endorsed.dirs.  It is null
  // until we've initialized the system, at which point it is created.
  static BootClassLoader bootLoader;

  // This keeps track of shared libraries we've already tried to load.
  private static HashSet tried_libraries;

  // Holds one of the LIB_* constants; used to determine how shared
  // library loads are done.
  private static int lib_control;

  private static final int LIB_FULL = 0;
  private static final int LIB_CACHE = 1;
  private static final int LIB_NEVER = 2;

  /**
   * Helper to define a class using a string of bytes. This assumes that
   * the security checks have already been performed, if necessary.
   *
   * <strong>For backward compatibility, this just ignores the protection
   * domain; that is the wrong behavior, and you should directly implement
   * this method natively if you can.</strong>
   *
   * @param name the name to give the class, or null if unknown
   * @param data the data representing the classfile, in classfile format
   * @param offset the offset into the data where the classfile starts
   * @param len the length of the classfile data in the array
   * @param pd the protection domain
   * @return the class that was defined
   * @throws ClassFormatError if data is not in proper classfile format
   */
  static final native Class defineClass(ClassLoader cl, String name,
					byte[] data, int offset, int len,
					ProtectionDomain pd)
    throws ClassFormatError;

  /**
   * Helper to resolve all references to other classes from this class.
   *
   * @param c the class to resolve
   */
  static final void resolveClass(Class clazz)
  {
    // There doesn't seem to be a need for this to do anything.
    // Testing reveals that the JDK doesn't seem to do anything here,
    // either.
  }

  /**
   * Helper to load a class from the bootstrap class loader.
   *
   * @param name the class name to load
   * @param resolve whether to resolve it
   * @return the class, loaded by the bootstrap classloader or null
   * if the class wasn't found. Returning null is equivalent to throwing
   * a ClassNotFoundException (but a possible performance optimization).
   */
  static final native Class loadClass(String name, boolean resolve)
    throws ClassNotFoundException;

  /**
   * Helper to load a resource from the bootstrap class loader.
   *
   * In libgcj, this does nothing, as the default system loader knows
   * how to find resources that have been linked in.
   *
   * @param name the resource to find
   * @return the URL to the resource
   */
  static URL getResource(String name)
  {
    if (bootLoader != null)
      return bootLoader.bootGetResource(name);
    return null;
  }

  /**
   * Helper to get a list of resources from the bootstrap class loader.
   *
   * In libgcj, this does nothing, as the default system loader knows
   * how to find resources that have been linked in.
   *
   * @param name the resource to find
   * @return an enumeration of resources
   * @throws IOException if one occurs
   */
  static Enumeration getResources(String name) throws IOException
  {
    if (bootLoader != null)
      return bootLoader.bootGetResources(name);
    return EmptyEnumeration.getInstance();
  }

  /**
   * Helper to get a package from the bootstrap class loader.  The default
   * implementation of returning null may be adequate, or you may decide
   * that this needs some native help.
   *
   * @param name the name to find
   * @return the named package, if it exists
   */
  static synchronized Package getPackage(String name)
  {
    return (Package) definedPackages.get(name);
  }

  /**
   * Helper to get all packages from the bootstrap class loader.  The default
   * implementation of returning an empty array may be adequate, or you may
   * decide that this needs some native help.
   *
   * @return all named packages, if any exist
   */
  static synchronized Package[] getPackages()
  {
    Package[] packages = new Package[definedPackages.size()];
    return (Package[]) definedPackages.values().toArray(packages);
  }

  // Define a package for something loaded natively.
  static synchronized void definePackageForNative(String className)
  {
    int lastDot = className.lastIndexOf('.');
    if (lastDot != -1)
      {
	String packageName = className.substring(0, lastDot);
	if (getPackage(packageName) == null)
	  {
	    // FIXME: this assumes we're defining the core, which
	    // isn't necessarily so.  We could detect this and set up
	    // appropriately.  We could also look at a manifest file
	    // compiled into the .so.
	    Package p = new Package(packageName,
				    "Java Platform API Specification",
				    "GNU", "1.4", "gcj", "GNU",
				    null, // FIXME: gcj version.
				    null);
	    definedPackages.put(packageName, p);
	  }
      }
  }

  /**
   * Helper for java.lang.Integer, Byte, etc to get the TYPE class
   * at initialization time. The type code is one of the chars that
   * represents the primitive type as in JNI.
   *
   * <ul>
   * <li>'Z' - boolean</li>
   * <li>'B' - byte</li>
   * <li>'C' - char</li>
   * <li>'D' - double</li>
   * <li>'F' - float</li>
   * <li>'I' - int</li>
   * <li>'J' - long</li>
   * <li>'S' - short</li>
   * <li>'V' - void</li>
   * </ul>
   *
   * @param type the primitive type
   * @return a "bogus" class representing the primitive type
   */
  static final native Class getPrimitiveClass(char type);

  /**
   * The system default for assertion status. This is used for all system
   * classes (those with a null ClassLoader), as well as the initial value for
   * every ClassLoader's default assertion status.
   *
   * XXX - Not implemented yet; this requires native help.
   *
   * @return the system-wide default assertion status
   */
  static final boolean defaultAssertionStatus()
  {
    return true;
  }

  /**
   * The system default for package assertion status. This is used for all
   * ClassLoader's packageAssertionStatus defaults. It must be a map of
   * package names to Boolean.TRUE or Boolean.FALSE, with the unnamed package
   * represented as a null key.
   *
   * XXX - Not implemented yet; this requires native help.
   *
   * @return a (read-only) map for the default packageAssertionStatus
   */
  static final Map packageAssertionStatus()
  {
    return new HashMap();
  }

  /**
   * The system default for class assertion status. This is used for all
   * ClassLoader's classAssertionStatus defaults. It must be a map of
   * class names to Boolean.TRUE or Boolean.FALSE
   *
   * XXX - Not implemented yet; this requires native help.
   *
   * @return a (read-only) map for the default classAssertionStatus
   */
  static final Map classAssertionStatus()
  {
    return new HashMap();
  }

  static native ClassLoader getSystemClassLoaderInternal();

  static native void initBootLoader(String libdir);

  static void initialize(String libdir)
  {
    initBootLoader(libdir);

    String p
      = System.getProperty ("gnu.gcj.runtime.VMClassLoader.library_control",
			    "");
    if ("never".equals(p))
      lib_control = LIB_NEVER;
    else if ("cache".equals(p))
      lib_control = LIB_CACHE;
    else if ("full".equals(p))
      lib_control = LIB_FULL;
    else
      lib_control = LIB_CACHE;

    tried_libraries = new HashSet();
  }

  /**
   * Possibly load a .so and search it for classes.
   */
  static native Class nativeFindClass(String name);

  static ClassLoader getSystemClassLoader()
  {
    // This method is called as the initialization of systemClassLoader,
    // so if there is a null value, this is the first call and we must check
    // for java.system.class.loader.
    String loader = System.getProperty("java.system.class.loader");
    ClassLoader default_sys = getSystemClassLoaderInternal();
    if (loader != null)
      {
	try
	  {
	    Class load_class = Class.forName(loader, true, default_sys);
	    Constructor c
	      = load_class.getConstructor(new Class[] { ClassLoader.class });
	    default_sys
	      = (ClassLoader) c.newInstance(new Object[] { default_sys });
	  }
	catch (Exception ex)
	  {
	    throw new Error("Failed to load requested system classloader "
			       + loader, ex);
	  }
      }

    return default_sys;
  }
}
