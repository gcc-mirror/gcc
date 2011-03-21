/* SecretKeyFactory.java -- Factory for creating secret keys.
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package javax.crypto;

import gnu.java.security.Engine;

import java.lang.reflect.InvocationTargetException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Provider;
import java.security.Security;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;

/**
 * A secret key factory translates {@link SecretKey} objects to and from
 * {@link java.security.spec.KeySpec} objects, and can translate between
 * different vendors' representations of {@link SecretKey} objects (for
 * security or semantics; whichever applies).
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 * @see SecretKey
 */
public class SecretKeyFactory
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  private static final String SERVICE = "SecretKeyFactory";

  /** The underlying factory implementation. */
  private SecretKeyFactorySpi skfSpi;

  /** The provider of the implementation. */
  private Provider provider;

  /** The name of the algorithm. */
  private String algorithm;

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Create a new secret key factory.
   *
   * @param skfSpi   The underlying factory implementation.
   * @param provider The provider.
   * @param algorithm The algorithm name.
   */
  protected SecretKeyFactory(SecretKeyFactorySpi skfSpi, Provider provider,
                             String algorithm)
  {
    this.skfSpi = skfSpi;
    this.provider = provider;
    this.algorithm = algorithm;
  }

  /**
   * Create a new secret key factory from the first appropriate instance.
   *
   * @param algorithm The algorithm name.
   * @return The appropriate key factory, if found.
   * @throws NoSuchAlgorithmException If no provider implements the specified
   *           algorithm.
   * @throws IllegalArgumentException if <code>algorithm</code> is
   *           <code>null</code> or is an empty string.
   */
  public static final SecretKeyFactory getInstance(String algorithm)
      throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();
    NoSuchAlgorithmException lastException = null;
    for (int i = 0; i < p.length; i++)
      try
        {
          return getInstance(algorithm, p[i]);
        }
      catch (NoSuchAlgorithmException x)
        {
          lastException = x;
        }
      if (lastException != null)
        throw lastException;
      throw new NoSuchAlgorithmException(algorithm);
  }

  /**
   * Create a new secret key factory from the named provider.
   *
   * @param algorithm The algorithm name.
   * @param provider The provider name.
   * @return The appropriate key factory, if found.
   * @throws NoSuchAlgorithmException If the named provider does not implement
   *           the algorithm.
   * @throws NoSuchProviderException If the named provider does not exist.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   */
  public static final SecretKeyFactory getInstance(String algorithm,
                                                   String provider)
      throws NoSuchAlgorithmException, NoSuchProviderException
  {
    if (provider == null)
      throw new IllegalArgumentException("provider MUST NOT be null");
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    return getInstance(algorithm, p);
  }

  /**
   * Create a new secret key factory from the specified provider.
   *
   * @param algorithm The algorithm name.
   * @param provider The provider.
   * @return The appropriate key factory, if found.
   * @throws NoSuchAlgorithmException If the provider does not implement the
   *           algorithm.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   */
  public static final SecretKeyFactory getInstance(String algorithm,
                                                   Provider provider)
      throws NoSuchAlgorithmException
  {
    StringBuilder sb = new StringBuilder("SecretKeyFactory algorithm [")
        .append(algorithm).append("] from provider[")
        .append(provider).append("] could not be created");
    Throwable cause;
    try
      {
        Object spi = Engine.getInstance(SERVICE, algorithm, provider);
        return new SecretKeyFactory((SecretKeyFactorySpi) spi, provider, algorithm);
      }
    catch (InvocationTargetException x)
      {
        cause = x.getCause();
        if (cause instanceof NoSuchAlgorithmException)
          throw (NoSuchAlgorithmException) cause;
        if (cause == null)
          cause = x;
      }
    catch (ClassCastException x)
      {
        cause = x;
      }
    NoSuchAlgorithmException x = new NoSuchAlgorithmException(sb.toString());
    x.initCause(cause);
    throw x;
  }

  /**
   * Generate a secret key from a key specification, if possible.
   *
   * @param keySpec The key specification.
   * @return The secret key.
   * @throws java.security.InvalidKeySpecException If the key specification
   *         cannot be transformed into a secret key.
   */
  public final SecretKey generateSecret(KeySpec keySpec)
    throws InvalidKeySpecException
  {
    return skfSpi.engineGenerateSecret(keySpec);
  }

  /**
   * Get the algorithm name.
   *
   * @return The algorithm name.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Get the key specification from a secret key.
   *
   * @param key     The secret key.
   * @param keySpec The target key specification class.
   * @return The key specification.
   * @throws java.security.spec.InvalidKeySpecException If the secret key cannot
   *         be transformed into the specified key specification.
   */
  public final KeySpec getKeySpec(SecretKey key, Class keySpec)
    throws InvalidKeySpecException
  {
    return skfSpi.engineGetKeySpec(key, keySpec);
  }

  /**
   * Get the provider of this implementation.
   *
   * @return The provider.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Translate a secret key into another form.
   *
   * @param key The key to translate.
   * @return The translated key.
   * @throws java.security.InvalidKeyException If the argument cannot be
   *         translated.
   */
  public final SecretKey translateKey(SecretKey key)
    throws InvalidKeyException
  {
    return skfSpi.engineTranslateKey(key);
  }
}
