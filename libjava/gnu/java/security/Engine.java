/* Engine -- generic getInstance method.
   Copyright (C) 2003  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

package gnu.java.security;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import java.security.NoSuchAlgorithmException;
import java.security.Provider;

/**
 * Generic implementation of the getInstance methods in the various
 * engine classes in java.security.
 * <p>
 * These classes ({@link java.security.Signature} for example) can be
 * thought of as the "chrome, upholstery, and steering wheel", and the SPI
 * (service provider interface, e.g. {@link java.security.SignatureSpi})
 * classes can be thought of as the "engine" -- providing the actual
 * functionality of whatever cryptographic algorithm the instance
 * represents.
 *
 * @see Provider
 * @author Casey Marshall 
 */
public final class Engine
{

  // Constants.
  // ------------------------------------------------------------------------

  /** Prefix for aliases. */
  private static final String ALG_ALIAS = "Alg.Alias.";

  /** Maximum number of aliases to try. */
  private static final int MAX_ALIASES = 5;

  /** Argument list for no-argument constructors. */
  private static final Object[] NO_ARGS = new Object[0];

  // Constructor.
  // ------------------------------------------------------------------------

  /** This class cannot be instantiated. */
  private Engine() { }

  // Class method.
  // ------------------------------------------------------------------------

  /**
   * Get the implementation for <i>algorithm</i> for service
   * <i>service</i> from <i>provider</i>. The service is e.g.
   * "Signature", and the algorithm "DSA".
   *
   * @param service   The service name.
   * @param algorithm The name of the algorithm to get.
   * @param provider  The provider to get the implementation from.
   * @return The engine class for the specified algorithm; the object
   *         returned is typically a subclass of the SPI class for that
   *         service, but callers should check that this is so.
   * @throws NoSuchAlgorithmException If the implementation cannot be
   *         found or cannot be instantiated.
   * @throws InvocationTargetException If the SPI class's constructor
   *         throws an exception.
   * @throws IllegalArgumentException If any of the three arguments are null.
   */
  public static Object getInstance(String service, String algorithm,
                                   Provider provider)
    throws InvocationTargetException, NoSuchAlgorithmException
  {
    return getInstance(service, algorithm, provider, NO_ARGS);
  }

  /**
   * Get the implementation for <i>algorithm</i> for service
   * <i>service</i> from <i>provider</i>, passing <i>initArgs</i> to the
   * SPI class's constructor (which cannot be null; pass a zero-length
   * array if the SPI takes no arguments). The service is e.g.
   * "Signature", and the algorithm "DSA".
   *
   * @param service   The service name.
   * @param algorithm The name of the algorithm to get.
   * @param provider  The provider to get the implementation from.
   * @param initArgs  The arguments to pass to the SPI class's
   *        constructor (cannot be null).
   * @return The engine class for the specified algorithm; the object
   *         returned is typically a subclass of the SPI class for that
   *         service, but callers should check that this is so.
   * @throws NoSuchAlgorithmException If the implementation cannot be
   *         found or cannot be instantiated.
   * @throws InvocationTargetException If the SPI class's constructor
   *         throws an exception.
   * @throws IllegalArgumentException If any of the four arguments are null.
   */
  public static Object getInstance(String service, String algorithm,
                                   Provider provider, Object[] initArgs)
    throws InvocationTargetException, NoSuchAlgorithmException
  {
    if (service == null || algorithm == null
        || provider == null || initArgs == null)
      throw new IllegalArgumentException();

    // If there is no property "service.algorithm"
    if (provider.getProperty(service + "." + algorithm) == null)
      {
        // Iterate through aliases, until we find the class name or resolve
        // too many aliases.
        String alias = null;
        int count = 0;
        while ((alias = provider.getProperty(
                ALG_ALIAS + service + "." + algorithm)) != null)
          {
            if (algorithm.equals(alias))  // Refers to itself!
              break;
            algorithm = alias;
            if (count++ > MAX_ALIASES)
              throw new NoSuchAlgorithmException("too many aliases");
          }
        if (provider.getProperty(service + "." + algorithm) == null)
          throw new NoSuchAlgorithmException(algorithm);
      }

    // Find and instantiate the implementation.
    Class clazz = null;
    ClassLoader loader = provider.getClass().getClassLoader();
    Constructor constructor = null;
    String error = algorithm;

    try
      {
        if (loader != null)
          clazz = loader.loadClass(provider.getProperty(service+"."+algorithm));
        else
          clazz = Class.forName(provider.getProperty(service+"."+algorithm));
        constructor = getCompatibleConstructor(clazz, initArgs);
        return constructor.newInstance(initArgs);
      }
    catch (ClassNotFoundException cnfe)
      {
        error = "class not found: " + algorithm;
      }
    catch (IllegalAccessException iae)
      {
        error = "illegal access: " + iae.getMessage();
      }
    catch (InstantiationException ie)
      {
        error = "instantiation exception: " + ie.getMessage();
      }
    catch (ExceptionInInitializerError eiie)
      {
        error = "exception in initializer: " + eiie.getMessage();
      }
    catch (SecurityException se)
      {
        error = "security exception: " + se.getMessage();
      }
    catch (NoSuchMethodException nsme)
      {
        error = "no appropriate constructor found";
      }

    throw new NoSuchAlgorithmException(error);
  }

  // Own methods.
  // ------------------------------------------------------------------------

  /**
   * Find a constructor in the given class that can take the specified
   * argument list, allowing any of which to be null.
   *
   * @param clazz    The class from which to get the constructor.
   * @param initArgs The argument list to be passed to the constructor.
   * @return The constructor.
   * @throws NoSuchMethodException If no constructor of the given class
   *         can take the specified argument array.
   */
  private static Constructor getCompatibleConstructor(Class clazz,
                                                      Object[] initArgs)
    throws NoSuchMethodException
  {
    Constructor[] c = clazz.getConstructors();
    outer:for (int i = 0; i < c.length; i++)
      {
        Class[] argTypes = c[i].getParameterTypes();
        if (argTypes.length != initArgs.length)
          continue;
        for (int j = 0; j < argTypes.length; j++)
          {
            if (initArgs[j] != null &&
                !argTypes[j].isAssignableFrom(initArgs[j].getClass()))
              continue outer;
          }
        // If we reach this point, we know this constructor (c[i]) has
        // the same number of parameters as the target parameter list,
        // and all our parameters are either (1) null, or (2) assignable
        // to the target parameter type.
        return c[i];
      }
    throw new NoSuchMethodException();
  }
}
