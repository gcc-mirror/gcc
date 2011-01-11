/* VMClassLoader.java -- Reference implementation of native interface
   required by ClassLoader
   Copyright (C) 1998, 2001, 2002, 2004, 2005, 2006 Free Software Foundation

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

import gnu.classpath.Configuration;
import gnu.classpath.SystemProperties;
import gnu.java.lang.InstrumentationImpl;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.instrument.Instrumentation;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.ProtectionDomain;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.zip.ZipFile;

/**
 * java.lang.VMClassLoader is a package-private helper for VMs to implement
 * on behalf of java.lang.ClassLoader.
 *
 * @author John Keiser
 * @author Mark Wielaard (mark@klomp.org)
 * @author Eric Blake (ebb9@email.byu.edu)
 */
final class VMClassLoader
{


  /** packages loaded by the bootstrap class loader */
  static final HashMap definedPackages = new HashMap();

  /** jars from property java.boot.class.path */
  static final HashMap bootjars = new HashMap();


  /**
   * Converts the array string of native package names to
   * Packages. The packages are then put into the
   * definedPackages hashMap
   */
  static
  {
    String[] packages = getBootPackages();

    if( packages != null)
      {
        String specName =
              SystemProperties.getProperty("java.specification.name");
        String vendor =
              SystemProperties.getProperty("java.specification.vendor");
        String version =
              SystemProperties.getProperty("java.specification.version");

        Package p;

        for(int i = 0; i < packages.length; i++)
          {
            p = new Package(packages[i],
                  specName,
                  vendor,
                  version,
                  "GNU Classpath",
                  "GNU",
                  Configuration.CLASSPATH_VERSION,
                  null,
                  null);

            definedPackages.put(packages[i], p);
          }
      }
  }


  /**
   * Helper to define a class using a string of bytes. This assumes that
   * the security checks have already been performed, if necessary.
   *
   * Implementations of this method are advised to consider the
   * situation where user code modifies the byte array after it has
   * been passed to defineClass.  This can be handled by making a
   * private copy of the array, or arranging to only read any given
   * byte a single time.
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
  static final native void resolveClass(Class c);

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
   * @param name the resource to find
   * @return the URL to the resource
   */
  static URL getResource(String name)
  {
    Enumeration e = getResources(name);
    if (e.hasMoreElements())
      return (URL)e.nextElement();
    return null;
  }
  /**
   * Helper to get a list of resources from the bootstrap class loader.
   *
   * @param name the resource to find
   * @return an enumeration of resources
   */
  static Enumeration getResources(String name)
  {
    StringTokenizer st = new StringTokenizer(
      SystemProperties.getProperty("java.boot.class.path", "."),
      File.pathSeparator);
    Vector v = new Vector();
    while (st.hasMoreTokens())
      {
        File file = new File(st.nextToken());
        if (file.isDirectory())
          {
            try
              {
                File f = new File(file, name);
                if (!f.exists()) continue;
                v.add(new URL("file://" + f.getAbsolutePath()));
              }
            catch (MalformedURLException e)
              {
                throw new Error(e);
              }
          }
        else if (file.isFile())
          {
            ZipFile zip;
            synchronized(bootjars)
              {
                zip = (ZipFile) bootjars.get(file.getName());
              }
            if(zip == null)
              {
                try
                  {
                    zip = new ZipFile(file);
                    synchronized(bootjars)
                      {
                        bootjars.put(file.getName(), zip);
                      }
                  }
                catch (IOException e)
                  {
                    continue;
                  }
              }
            String zname = name.startsWith("/") ? name.substring(1) : name;
            if (zip.getEntry(zname) == null)
              continue;
            try
              {
                v.add(new URL("jar:file://"
                  + file.getAbsolutePath() + "!/" + zname));
              }
            catch (MalformedURLException e)
              {
                throw new Error(e);
              }
          }
      }
    return v.elements();
  }


  /**
   * Returns a String[] of native package names. The default
   * implementation tries to load a list of package from
   * the META-INF/INDEX.LIST file in the boot jar file.
   * If not found or if any exception is raised, it returns
   * an empty array. You may decide this needs native help.
   */
  private static String[] getBootPackages()
  {
    URL indexList = getResource("META-INF/INDEX.LIST");
    if (indexList != null)
      {
        try
          {
            Set packageSet = new HashSet();
            String line;
            int lineToSkip = 3;
            BufferedReader reader = new BufferedReader(
                                                       new InputStreamReader(
                                                                             indexList.openStream()));
            while ((line = reader.readLine()) != null)
              {
                if (lineToSkip == 0)
                  {
                    if (line.length() == 0)
                      lineToSkip = 1;
                    else
                      packageSet.add(line.replace('/', '.'));
                  }
                else
                  lineToSkip--;
              }
            reader.close();
            return (String[]) packageSet.toArray(new String[packageSet.size()]);
          }
        catch (IOException e)
          {
            return new String[0];
          }
      }
    else
      return new String[0];
  }


  /**
   * Helper to get a package from the bootstrap class loader.
   *
   * @param name the name to find
   * @return the named package, if it exists
   */
  static Package getPackage(String name)
  {
    return (Package)definedPackages.get(name);
  }



  /**
   * Helper to get all packages from the bootstrap class loader.
   *
   * @return all named packages, if any exist
   */
  static Package[] getPackages()
  {
    Package[] packages = new Package[definedPackages.size()];
    definedPackages.values().toArray(packages);
    return packages;
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

  static ClassLoader getSystemClassLoader()
  {
    return ClassLoader.defaultGetSystemClassLoader();
  }

  /**
   * Find the class if this class loader previously defined this class
   * or if this class loader has been recorded as the initiating class loader
   * for this class.
   */
  static native Class findLoadedClass(ClassLoader cl, String name);

  /**
   * The Instrumentation object created by the vm when agents are defined.
   */
  static final Instrumentation instrumenter = null;

  /**
   * Call the transformers of the possible Instrumentation object. This
   * implementation assumes the instrumenter is a
   * <code>InstrumentationImpl</code> object. VM implementors would
   * have to redefine this method if they provide their own implementation
   * of the <code>Instrumentation</code> interface.
   *
   * @param loader the initiating loader
   * @param name the name of the class
   * @param data the data representing the classfile, in classfile format
   * @param offset the offset into the data where the classfile starts
   * @param len the length of the classfile data in the array
   * @param pd the protection domain
   * @return the new data representing the classfile
   */
  static final Class defineClassWithTransformers(ClassLoader loader,
      String name, byte[] data, int offset, int len, ProtectionDomain pd)
  {

    if (instrumenter != null)
      {
        byte[] modifiedData = new byte[len];
        System.arraycopy(data, offset, modifiedData, 0, len);
        String jvmName = name.replace('.', '/');
        modifiedData =
          ((InstrumentationImpl)instrumenter).callTransformers(loader, jvmName,
            null, pd, modifiedData);

        return defineClass(loader, name, modifiedData, 0, modifiedData.length,
            pd);
      }
    else
      {
        return defineClass(loader, name, data, offset, len, pd);
      }
  }
}
