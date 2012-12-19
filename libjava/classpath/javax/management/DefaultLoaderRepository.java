/* DefaultLoaderRepository.java -- Manages class loaders for the servers.
   Copyright (C) 2007 Free Software Foundation, Inc.

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

package javax.management;

import java.util.List;

/**
 * Maintains a list of the {@link ClassLoader} instances
 * registered with the management servers, allowing it
 * to be used to load classes.  In early versions of the
 * JMX API, this class represented a shared repository for
 * the classloaders of all management servers.  The management
 * of classloaders is now decentralised and this class is
 * deprecated.  The old behaviour is emulated by consulting
 * the {@link MBeanServer#getClassLoaderRepository()} method
 * of all the servers obtained from
 * {@link MBeanServerFactory#findMBeanServer(String)}.  Use of
 * this class should be avoided in new code.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 * @deprecated Use {@link MBeanServer#getClassLoaderRepository()}
 *             instead.
 */
@Deprecated public class DefaultLoaderRepository
{

  /**
   * Attempts to load the given class using class loaders
   * supplied by the repository of each {@link MBeanServer}.
   * The {@link ClassLoader#loadClass(String)}
   * method of each class loader is called.  If the method
   * returns successfully, then the returned {@link Class} instance
   * is returned.  If a {@link ClassNotFoundException} is thrown,
   * then the next loader is tried.  Any other exception thrown
   * by the method is passed back to the caller.  This method
   * throws a {@link ClassNotFoundException} itself if all the
   * class loaders listed prove fruitless.
   *
   * @param name the name of the class to load.
   * @return the loaded class.
   * @throws ClassNotFoundException if all the class loaders fail
   *                                to load the class.
   */
  // API issue with lack of <?> on Class
  @SuppressWarnings("rawtypes")
  public static Class loadClass(String name)
    throws ClassNotFoundException
  {
    List<MBeanServer> servers = MBeanServerFactory.findMBeanServer(null);
    for (MBeanServer server : servers)
      {
        try
          {
            return server.getClassLoaderRepository().loadClass(name);
          }
        catch (ClassNotFoundException e)
          {
            /* Ignored; try the next server. */
          }
      }
    throw new ClassNotFoundException("The class loaders of all registered " +
                                     "servers failed to load the class, " +
                                     name);
  }

  /**
   * <p>
   * Attempts to load the given class using class loaders
   * supplied by the repository of each {@link MBeanServer}.
   * The {@link ClassLoader#loadClass(String)}
   * method of each class loader is called.  If the method
   * returns successfully, then the returned {@link Class} instance
   * is returned.  If a {@link ClassNotFoundException} is thrown,
   * then the next loader is tried.  Any other exception thrown
   * by the method is passed back to the caller.  This method
   * throws a {@link ClassNotFoundException} itself if all the
   * class loaders listed prove fruitless.
   * </p>
   * <p>
   * Note that this method may deadlock if called simultaneously
   * by two class loaders in the list.
   * {@link loadClassBefore(ClassLoader, String)} should be used
   * in preference to this method to avoid this.
   * </p>
   *
   * @param exclude the class loader to exclude, or <code>null</code>
   *             to obtain the same behaviour as {@link #loadClass(String)}.
   * @param name the name of the class to load.
   * @return the loaded class.
   * @throws ClassNotFoundException if all the class loaders fail
   *                                to load the class.
   */
  // API issue with lack of <?> on Class
  @SuppressWarnings("rawtypes")
  public static Class loadClassWithout(ClassLoader exclude, String name)
    throws ClassNotFoundException
  {
    List<MBeanServer> servers = MBeanServerFactory.findMBeanServer(null);
    for (MBeanServer server : servers)
      {
        try
          {
            return server.getClassLoaderRepository().loadClassWithout(exclude,
                                                                      name);
          }
        catch (ClassNotFoundException e)
          {
            /* Ignored; try the next server. */
          }
      }
    throw new ClassNotFoundException("The class loaders of all registered " +
                                     "servers failed to load the class, " +
                                     name);
  }

}
