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

package java.security;

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
final class Engine
{

  // Constants.
  // ------------------------------------------------------------------------

  /** Prefix for aliases. */
  private static final String ALG_ALIAS = "Alg.Alias.";

  /** Maximum number of aliases to try. */
  private static final int MAX_ALIASES = 5;

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
   * @throws IllegalArgumentException If any of the three arguments are null.
   */
  static Object
  getInstance(String service, String algorithm, Provider provider)
  throws NoSuchAlgorithmException
  {
    if (service == null || algorithm == null || provider == null)
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
    String error = algorithm;
    try
      {
        if (loader != null)
          clazz = loader.loadClass(provider.getProperty(service+"."+algorithm));
        else
          clazz = Class.forName(provider.getProperty(service+"."+algorithm));
        return clazz.newInstance();
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

    throw new NoSuchAlgorithmException(error);
  }
}
