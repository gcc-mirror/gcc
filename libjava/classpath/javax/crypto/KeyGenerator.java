/* KeyGenerator.java -- Interface to a symmetric key generator.
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
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Provider;
import java.security.SecureRandom;
import java.security.Security;
import java.security.spec.AlgorithmParameterSpec;

/**
 * A generic producer of keys for symmetric cryptography. The keys
 * returned may be simple wrappers around byte arrays, or, if the
 * target cipher requires them, more complex objects.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 * @see Cipher
 * @see Mac
 */
public class KeyGenerator
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  private static final String SERVICE = "KeyGenerator";

  /** The underlying generator implementation. */
  private KeyGeneratorSpi kgSpi;

  /** The provider of the implementation. */
  private Provider provider;

  /** The name of the algorithm. */
  private String algorithm;

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Create a new key generator.
   *
   * @param kgSpi     The underlying generator.
   * @param provider  The provider of this implementation.
   * @param algorithm The algorithm's name.
   */
  protected KeyGenerator(KeyGeneratorSpi kgSpi, Provider provider,
                         String algorithm)
  {
    this.kgSpi = kgSpi;
    this.provider = provider;
    this.algorithm = algorithm;
  }

  /**
   * Create a new key generator, returning the first available implementation.
   *
   * @param algorithm The generator algorithm name.
   * @throws NoSuchAlgorithmException If the specified algorithm does not exist.
   * @throws IllegalArgumentException if <code>algorithm</code> is
   *           <code>null</code> or is an empty string.
   */
  public static final KeyGenerator getInstance(String algorithm)
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
   * Create a new key generator from the named provider.
   *
   * @param algorithm The generator algorithm name.
   * @param provider The name of the provider to use.
   * @return An appropriate key generator, if found.
   * @throws NoSuchAlgorithmException If the specified algorithm is not
   *           implemented by the named provider.
   * @throws NoSuchProviderException If the named provider does not exist.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   */
  public static final KeyGenerator getInstance(String algorithm, String provider)
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
   * Create a new key generator from the supplied provider.
   *
   * @param algorithm The generator algorithm name.
   * @param provider The provider to use.
   * @return An appropriate key generator, if found.
   * @throws NoSuchAlgorithmException If the specified algorithm is not
   *           implemented by the provider.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   */
  public static final KeyGenerator getInstance(String algorithm,
                                               Provider provider)
      throws NoSuchAlgorithmException
  {
    StringBuilder sb = new StringBuilder("KeyGenerator algorithm [")
        .append(algorithm).append("] from provider[")
        .append(provider).append("] could not be created");
    Throwable cause;
    try
      {
        Object spi = Engine.getInstance(SERVICE, algorithm, provider);
        KeyGenerator instance = new KeyGenerator((KeyGeneratorSpi) spi,
                                                 provider,
                                                 algorithm);
        instance.init(new SecureRandom());
        return instance;
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
   * Generate a key.
   *
   * @return The new key.
   */
  public final SecretKey generateKey()
  {
    return kgSpi.engineGenerateKey();
  }

  /**
   * Return the name of this key generator.
   *
   * @return The algorithm name.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Return the provider of the underlying implementation.
   *
   * @return The provider.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Initialize this key generator with a set of parameters; the
   * highest-priority {@link java.security.SecureRandom} implementation
   * will be used.
   *
   * @param params The algorithm parameters.
   * @throws java.security.InvalidAlgorithmParameterException If the
   *         supplied parameters are inapproprate.
   */
  public final void init(AlgorithmParameterSpec params)
    throws InvalidAlgorithmParameterException
  {
    init(params, new SecureRandom());
  }

  /**
   * Initialize this key generator with a set of parameters and a source
   * of randomness.
   *
   * @param params The algorithm parameters.
   * @param random The source of randomness.
   * @throws java.security.InvalidAlgorithmParameterException If the
   *         supplied parameters are inapproprate.
   */
  public final void init(AlgorithmParameterSpec params, SecureRandom random)
    throws InvalidAlgorithmParameterException
  {
    kgSpi.engineInit(params, random);
  }

  /**
   * Initialize this key generator with a key size (in bits); the
   * highest-priority {@link java.security.SecureRandom} implementation
   * will be used.
   *
   * @param keySize The target key size, in bits.
   * @throws java.security.InvalidParameterException If the
   *         key size is unsupported.
   */
  public final void init(int keySize)
  {
    init(keySize, new SecureRandom());
  }

  /**
   * Initialize this key generator with a key size (in bits) and a
   * source of randomness.
   *
   * @param keySize The target key size, in bits.
   * @param random  The source of randomness.
   * @throws java.security.InvalidAlgorithmParameterException If the
   *         key size is unsupported.
   */
  public final void init(int keySize, SecureRandom random)
  {
    kgSpi.engineInit(keySize, random);
  }

  /**
   * Initialize this key generator with a source of randomness. The
   * implementation-specific default parameters (such as key size) will
   * be used.
   *
   * @param random The source of randomness.
   */
  public final void init(SecureRandom random)
  {
    kgSpi.engineInit(random);
  }
}
