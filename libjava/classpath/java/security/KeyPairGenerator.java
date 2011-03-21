/* KeyPairGenerator.java --- Key Pair Generator Class
   Copyright (C) 1999, 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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


package java.security;

import gnu.java.lang.CPStringBuilder;

import gnu.java.security.Engine;

import java.lang.reflect.InvocationTargetException;
import java.security.spec.AlgorithmParameterSpec;

/**
 * <code>KeyPairGenerator</code> is a class used to generate key-pairs for a
 * security algorithm.
 *
 * <p>The <code>KeyPairGenerator</code> is created with the
 * <code>getInstance()</code> Factory methods. It is used to generate a pair of
 * public and private keys for a specific algorithm and associate this key-pair
 * with the algorithm parameters it was initialized with.</p>
 *
 * @see KeyPair
 * @see AlgorithmParameterSpec
 * @author Mark Benvenuto
 * @author Casey Marshall
 */
public abstract class KeyPairGenerator extends KeyPairGeneratorSpi
{
  /** The service name for key pair generators. */
  private static final String KEY_PAIR_GENERATOR = "KeyPairGenerator";

  Provider provider;
  private String algorithm;

  /**
   * Constructs a new instance of <code>KeyPairGenerator</code>.
   *
   * @param algorithm
   *          the algorithm to use.
   */
  protected KeyPairGenerator(String algorithm)
  {
    this.algorithm = algorithm;
    this.provider = null;
  }

  /**
   * Returns the name of the algorithm used.
   *
   * @return the name of the algorithm used.
   */
  public String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Returns a new instance of <code>KeyPairGenerator</code> which generates
   * key-pairs for the specified algorithm.
   *
   * @param algorithm the name of the algorithm to use.
   * @return a new instance repesenting the desired algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by any
   *           provider.
   * @throws IllegalArgumentException if <code>algorithm</code> is
   *           <code>null</code> or is an empty string.
   */
  public static KeyPairGenerator getInstance(String algorithm)
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
   * Returns a new instance of <code>KeyPairGenerator</code> which generates
   * key-pairs for the specified algorithm from a named provider.
   *
   * @param algorithm the name of the algorithm to use.
   * @param provider the name of a {@link Provider} to use.
   * @return a new instance repesenting the desired algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by the
   *           named provider.
   * @throws NoSuchProviderException if the named provider was not found.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code> or empty.
   */
  public static KeyPairGenerator getInstance(String algorithm, String provider)
      throws NoSuchAlgorithmException, NoSuchProviderException
  {
    if (provider == null)
      throw new IllegalArgumentException("provider MUST NOT be null");
    provider = provider.trim();
    if (provider.length() == 0)
      throw new IllegalArgumentException("provider MUST NOT be empty");
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    return getInstance(algorithm, p);
  }

  /**
   * Returns a new instance of <code>KeyPairGenerator</code> which generates
   * key-pairs for the specified algorithm from a designated {@link Provider}.
   *
   * @param algorithm
   *          the name of the algorithm to use.
   * @param provider
   *          the {@link Provider} to use.
   * @return a new insatnce repesenting the desired algorithm.
   * @throws NoSuchAlgorithmException
   *           if the algorithm is not implemented by the {@link Provider}.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   * @since 1.4
   * @see Provider
   */
  public static KeyPairGenerator getInstance(String algorithm,
                                             Provider provider)
    throws NoSuchAlgorithmException
  {
    CPStringBuilder sb = new CPStringBuilder("KeyPairGenerator for algorithm [")
        .append(algorithm).append("] from provider[")
        .append(provider).append("] ");
    Object o;
    try
      {
        o = Engine.getInstance(KEY_PAIR_GENERATOR, algorithm, provider);
      }
    catch (InvocationTargetException x)
      {
        Throwable cause = x.getCause();
        if (cause instanceof NoSuchAlgorithmException)
          throw (NoSuchAlgorithmException) cause;
        if (cause == null)
          cause = x;
        sb.append("could not be created");
        NoSuchAlgorithmException y = new NoSuchAlgorithmException(sb.toString());
        y.initCause(cause);
        throw y;
      }
    KeyPairGenerator result;
    if (o instanceof KeyPairGenerator)
      {
        result = (KeyPairGenerator) o;
        result.algorithm = algorithm;
      }
    else if (o instanceof KeyPairGeneratorSpi)
      result = new DummyKeyPairGenerator((KeyPairGeneratorSpi) o, algorithm);
    else
      {
        sb.append("is of an unexpected Type: ").append(o.getClass().getName());
        throw new NoSuchAlgorithmException(sb.toString());
      }
    result.provider = provider;
    return result;
  }

  /**
   * Returns the {@link Provider} of this instance.
   *
   * @return the {@link Provider} of this instance.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Initializes this instance for the specified key size. Since no source of
   * randomness is specified, a default one will be used.
   *
   * @param keysize
   *          the size of keys to use.
   */
  public void initialize(int keysize)
  {
    initialize(keysize, new SecureRandom());
  }

  /**
   * Initializes this instance for the specified key size and
   * {@link SecureRandom}.
   *
   * @param keysize
   *          the size of keys to use.
   * @param random
   *          the {@link SecureRandom} to use.
   * @since 1.2
   */
  public void initialize(int keysize, SecureRandom random)
  {
  }

  /**
   * Initializes this instance with the specified
   * {@link AlgorithmParameterSpec}. Since no source of randomness is specified,
   * a default one will be used.
   *
   * @param params
   *          the {@link AlgorithmParameterSpec} to use.
   * @throws InvalidAlgorithmParameterException
   *           if the designated specifications are invalid.
   * @since 1.2
   */
  public void initialize(AlgorithmParameterSpec params)
    throws InvalidAlgorithmParameterException
  {
    initialize(params, new SecureRandom());
  }

  /**
   * Initializes this instance with the specified {@link AlgorithmParameterSpec}
   * and {@link SecureRandom}.
   *
   * @param params
   *          the {@link AlgorithmParameterSpec} to use.
   * @param random
   *          the {@link SecureRandom} to use.
   * @throws InvalidAlgorithmParameterException
   *           if the designated specifications are invalid.
   * @since 1.2
   */
  public void initialize(AlgorithmParameterSpec params, SecureRandom random)
    throws InvalidAlgorithmParameterException
  {
    super.initialize(params, random);
  }

  /**
   * Generates a new "DSA" {@link KeyPair} from the "GNU" security provider.
   *
   * <p>This method generates a unique key-pair each time it is called.</p>
   *
   * @return a new unique {@link KeyPair}.
   * @see #generateKeyPair()
   * @since 1.2
   */
  public final KeyPair genKeyPair()
  {
    try
      {
        return getInstance("DSA", "GNU").generateKeyPair();
      }
    catch (Exception e)
      {
        System.err.println("genKeyPair failed: " + e);
        e.printStackTrace();
        return null;
      }
  }

  /**
   * Generates a new "DSA" {@link KeyPair} from the "GNU" security provider.
   *
   * <p>This method generates a unique key pair each time it is called.</p>
   *
   * @return a new unique {@link KeyPair}.
   * @see #genKeyPair()
   */
  public KeyPair generateKeyPair()
  {
    return genKeyPair();
  }
}
