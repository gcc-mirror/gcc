/* RMIClassLoader.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2002, 2003, 2004
   Free Software Foundation, Inc.

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

package java.rmi.server;

import gnu.classpath.ServiceFactory;
import gnu.classpath.SystemProperties;
import gnu.java.rmi.server.RMIClassLoaderImpl;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;

/**
 * This class provides a set of public static utility methods for supporting
 * network-based class loading in RMI. These methods are called by RMI's
 * internal marshal streams to implement the dynamic class loading of types for
 * RMI parameters and return values.
 * @since 1.1
 */
public class RMIClassLoader
{
  /**
   * This class isn't intended to be instantiated.
   */
  private RMIClassLoader() {}

  /**
   * @deprecated
   */
  public static Class<?> loadClass(String name)
    throws MalformedURLException, ClassNotFoundException
  {
    return loadClass("", name);
  }

  public static Class<?> loadClass(String codebase, String name)
    throws MalformedURLException, ClassNotFoundException
  {
    RMIClassLoaderSpi spi = getProviderInstance();
    if (spi == null)
      spi = getDefaultProviderInstance();
    return spi.loadClass(codebase, name, null);
  }

  public static Class<?> loadClass(String codebase, String name,
                                   ClassLoader defaultLoader)
    throws MalformedURLException, ClassNotFoundException
  {
    RMIClassLoaderSpi spi = getProviderInstance();
    if (spi == null)
      spi = getDefaultProviderInstance();
    return spi.loadClass(codebase, name, defaultLoader);
  }

  public static Class<?> loadProxyClass (String codeBase, String[] interfaces,
                                         ClassLoader defaultLoader)
    throws MalformedURLException, ClassNotFoundException
  {
    RMIClassLoaderSpi spi = getProviderInstance();
    if (spi == null)
      spi = getDefaultProviderInstance();
    return spi.loadProxyClass(codeBase, interfaces, defaultLoader);
  }

  /**
   * Loads a class from <code>codeBase</code>.
   *
   * This method delegates to
   * {@link RMIClassLoaderSpi#loadClass(String, String, ClassLoader)} and
   * passes <code>codeBase.toString()</code> as first argument,
   * <code>name</code> as second argument and <code>null</code> as third
   * argument.
   *
   * @param codeBase the code base from which to load the class
   * @param name the name of the class
   *
   * @return the loaded class
   *
   * @throws MalformedURLException if the URL is not well formed
   * @throws ClassNotFoundException if the requested class cannot be found
   */
  public static Class<?> loadClass(URL codeBase, String name)
    throws MalformedURLException, ClassNotFoundException
  {
    RMIClassLoaderSpi spi = getProviderInstance();
    if (spi == null)
      spi = getDefaultProviderInstance();
    return spi.loadClass(codeBase.toString(), name, null);
  }

  /**
   * Gets a classloader for the given codebase and with the current
   * context classloader as parent.
   *
   * @param codebase
   *
   * @return a classloader for the given codebase
   *
   * @throws MalformedURLException if the codebase contains a malformed URL
   */
  public static ClassLoader getClassLoader(String codebase)
    throws MalformedURLException
  {
    RMIClassLoaderSpi spi = getProviderInstance();
    if (spi == null)
      spi = getDefaultProviderInstance();
    return spi.getClassLoader(codebase);
  }

  /**
   * Returns a string representation of the network location where a remote
   * endpoint can get the class-definition of the given class.
   *
   * @param cl
   *
   * @return a space seperated list of URLs where the class-definition
   * of cl may be found
   */
  public static String getClassAnnotation(Class<?> cl)
  {
    RMIClassLoaderSpi spi = getProviderInstance();
    if (spi == null)
      spi = getDefaultProviderInstance();
    return spi.getClassAnnotation(cl);
  }

  /**
   * @deprecated
   */
  public static Object getSecurityContext (ClassLoader loader)
  {
    throw new Error ("Not implemented");
  }

  /**
   * Returns the default service provider for <code>RMIClassLoader</code>.
   *
   * @return the default provider for <code>RMIClassLoader</code>
   */
  public static RMIClassLoaderSpi getDefaultProviderInstance()
  {
    return RMIClassLoaderImpl.getInstance();
  }

  /**
   * Chooses, instantiates and returns a service provider.
   *
   * @return a service provider
   */
  private static RMIClassLoaderSpi getProviderInstance()
  {
    // If the user asked for the default, return it.  We do a special
    // check here because our standard service lookup function does not
    // handle this -- nor should it.
    String prop = SystemProperties.getProperty("java.rmi.server.RMIClassLoaderSpi");
    if ("default".equals(prop))
      return null;
    Iterator it = ServiceFactory.lookupProviders(RMIClassLoaderSpi.class,
                                                 null);
    if (it == null || ! it.hasNext())
      return null;
    // FIXME: the spec says we ought to throw an Error of some kind if
    // the specified provider is not suitable for some reason.  However
    // our service factory simply logs the problem and moves on to the next
    // provider in this situation.
    return (RMIClassLoaderSpi) it.next();
  }
}
