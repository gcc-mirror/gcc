/* ClassLoaderRepository.java -- Represents a collection of class loadersx.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package javax.management.loading;

/**
 * Implementations of this interface maintain a list of
 * {@link ClassLoader}s for use by the management servers,
 * allowing classes to be loaded by the first {@link ClassLoader}
 * that will do so.  A class loader is added to the list
 * whenever a {@link ClassLoader} instance is registered with
 * the management server, and it does not implement the
 * {@link PrivateClassLoader} interface.  They are removed when
 * unregistered.  The first class loader in the list is always
 * the one which was used to load the management server itself.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 * @see MBeanServerFactory
 */
public interface ClassLoaderRepository
{

  /**
   * Attempts to load the given class using class loaders
   * supplied by the list.  The {@link ClassLoader#loadClass(String)}
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
  Class<?> loadClass(String name)
    throws ClassNotFoundException;

  /**
   * <p>
   * Attempts to load the given class using class loaders
   * supplied by the list, stopping when the specified
   * loader is reached.  The {@link ClassLoader#loadClass(String)}
   * method of each class loader is called.  If the method
   * returns successfully, then the returned {@link Class} instance
   * is returned.  If a {@link ClassNotFoundException} is thrown,
   * then the next loader is tried.  Any other exception thrown
   * by the method is passed back to the caller.  This method
   * throws a {@link ClassNotFoundException} itself if all the
   * class loaders listed prove fruitless.
   * </p>
   * <p>
   * This method is usually used by the class loader specified
   * by the <code>stop</code> argument to load classes using the
   * loaders that appear before it in the list.  By stopping when
   * the loader is reached, the deadlock that occurs when the loader
   * is merely skipped is avoided.
   * </p>
   *
   * @param stop the class loader at which to stop, or <code>null</code>
   *             to obtain the same behaviour as {@link #loadClass(String)}.
   * @param name the name of the class to load.
   * @return the loaded class.
   * @throws ClassNotFoundException if all the class loaders fail
   *                                to load the class.
   */
  Class<?> loadClassBefore(ClassLoader stop, String name)
    throws ClassNotFoundException;

  /**
   * <p>
   * Attempts to load the given class using class loaders
   * supplied by the list, excluding the one specified.
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
  Class<?> loadClassWithout(ClassLoader exclude, String name)
    throws ClassNotFoundException;

}

