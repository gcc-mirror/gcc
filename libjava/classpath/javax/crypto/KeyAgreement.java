/* KeyAgreement.java -- Engine for key agreement methods.
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
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Provider;
import java.security.SecureRandom;
import java.security.Security;
import java.security.spec.AlgorithmParameterSpec;

/**
 * Key agreement is a method in which two or more parties may agree on a
 * secret key for symmetric cryptography or message authentication
 * without transmitting any secrets in the clear. Key agreement
 * algorithms typically use a public/private <i>key pair</i>, and the
 * public key (along with some additional information) is sent across
 * untrusted networks.
 *
 * <p>The most common form of key agreement used today is the
 * <i>Diffie-Hellman key exchange algorithm</i>, described in <a
 * href="http://www.rsasecurity.com/rsalabs/pkcs/pkcs-3/">PKCS #3 -
 * Diffie Hellman Key Agreement Standard</a>.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 * @see KeyGenerator
 * @see SecretKey
 */
public class KeyAgreement
{

  // Fields.
  // ------------------------------------------------------------------------

  private static final String SERVICE = "KeyAgreement";

  /** The underlying key agreement implementation. */
  private KeyAgreementSpi kaSpi;

  /** The provider of this implementation. */
  private Provider provider;

  /** The name of this instance's algorithm. */
  private String algorithm;

  /** Singnals whether or not this instance has been initialized. */
  private boolean virgin;

  // Constructor.
  // ------------------------------------------------------------------------

  protected KeyAgreement(KeyAgreementSpi kaSpi, Provider provider,
                         String algorithm)
  {
    this.kaSpi = kaSpi;
    this.provider = provider;
    this.algorithm = algorithm;
    virgin = true;
  }

  /**
   * Get an implementation of an algorithm from the first provider that
   * implements it.
   * 
   * @param algorithm The name of the algorithm to get.
   * @return The proper KeyAgreement instacne, if found.
   * @throws NoSuchAlgorithmException If the specified algorithm is not
   *           implemented by any installed provider.
   * @throws IllegalArgumentException if <code>algorithm</code> is
   *           <code>null</code> or is an empty string.
   */
  public static final KeyAgreement getInstance(String algorithm)
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
   * Return an implementation of an algorithm from a named provider.
   * 
   * @param algorithm The name of the algorithm to create.
   * @param provider The name of the provider from which to get the
   *          implementation.
   * @return The proper KeyAgreement instance, if found.
   * @throws NoSuchAlgorithmException If the named provider does not implement
   *           the algorithm.
   * @throws NoSuchProviderException If the named provider does not exist.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   */
  public static final KeyAgreement getInstance(String algorithm, String provider)
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
   * Return an implementation of an algorithm from a specific provider.
   * 
   * @param algorithm The name of the algorithm to get.
   * @param provider The provider from which to get the implementation.
   * @return The proper KeyAgreement instance, if found.
   * @throws NoSuchAlgorithmException If this provider does not implement the
   *           algorithm.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   */
  public static final KeyAgreement getInstance(String algorithm,
                                               Provider provider)
    throws NoSuchAlgorithmException
  {
    StringBuilder sb = new StringBuilder("KeyAgreement algorithm [")
        .append(algorithm).append("] from provider[")
        .append(provider).append("] could not be created");
    Throwable cause;
    try
      {
        Object spi = Engine.getInstance(SERVICE, algorithm, provider);
        return new KeyAgreement((KeyAgreementSpi) spi, provider, algorithm);
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
   * Do a phase in the key agreement. The number of times this method is
   * called depends upon the algorithm and the number of parties
   * involved, but must be called at least once with the
   * <code>lastPhase</code> flag set to <code>true</code>.
   *
   * @param key       The key for this phase.
   * @param lastPhase Should be <code>true</code> if this will be the
   *        last phase before generating the shared secret.
   * @return The intermediate result, or <code>null</code> if there is
   *         no intermediate result.
   * @throws java.lang.IllegalStateException If this instance has not
   *         been initialized.
   * @throws java.security.InvalidKeyException If the key is
   *         inappropriate for this algorithm.
   */
  public final Key doPhase(Key key, boolean lastPhase)
    throws IllegalStateException, InvalidKeyException
  {
    if (virgin)
      {
        throw new IllegalStateException("not initialized");
      }
    return kaSpi.engineDoPhase(key, lastPhase);
  }

  /**
   * Generate the shared secret in a new byte array.
   *
   * @return The shared secret.
   * @throws java.lang.IllegalStateException If this instnace has not
   *         been initialized, or if not enough calls to
   *         <code>doPhase</code> have been made.
   */
  public final byte[] generateSecret() throws IllegalStateException
  {
    if (virgin)
      {
        throw new IllegalStateException("not initialized");
      }
    return kaSpi.engineGenerateSecret();
  }

  /**
   * Generate the shared secret and store it into the supplied array.
   *
   * @param sharedSecret The array in which to store the secret.
   * @param offset       The index in <code>sharedSecret</code> to start
   *                     storing data.
   * @return The length of the shared secret, in bytes.
   * @throws java.lang.IllegalStateException If this instnace has not
   *         been initialized, or if not enough calls to
   *         <code>doPhase</code> have been made.
   * @throws javax.crypto.ShortBufferException If the supplied array is
   *         not large enough to store the result.
   */
  public final int generateSecret(byte[] sharedSecret, int offset)
  throws IllegalStateException, ShortBufferException
  {
    if (virgin)
      {
        throw new IllegalStateException("not initialized");
      }
    return kaSpi.engineGenerateSecret(sharedSecret, offset);
  }

  /**
   * Generate the shared secret and return it as an appropriate {@link
   * SecretKey}.
   *
   * @param algorithm The secret key's algorithm.
   * @return The shared secret as a secret key.
   * @throws java.lang.IllegalStateException If this instnace has not
   *         been initialized, or if not enough calls to
   *         <code>doPhase</code> have been made.
   * @throws java.security.InvalidKeyException If the shared secret
   *         cannot be used to make a {@link SecretKey}.
   * @throws java.security.NoSuchAlgorithmException If the specified
   *         algorithm does not exist.
   */
  public final SecretKey generateSecret(String algorithm)
  throws IllegalStateException, InvalidKeyException, NoSuchAlgorithmException
  {
    if (virgin)
      {
        throw new IllegalStateException("not initialized");
      }
    return kaSpi.engineGenerateSecret(algorithm);
  }

  /**
   * Return the name of this key-agreement algorithm.
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
   * Initialize this key agreement with a key. This method will use the
   * highest-priority {@link java.security.SecureRandom} as its source
   * of randomness.
   *
   * @param key The key, usually the user's private key.
   * @throws java.security.InvalidKeyException If the supplied key is
   *         not appropriate.
   */
  public final void init(Key key) throws InvalidKeyException
  {
    init(key, new SecureRandom());
  }

  /**
   * Initialize this key agreement with a key and a source of
   * randomness.
   *
   * @param key    The key, usually the user's private key.
   * @param random The source of randomness.
   * @throws java.security.InvalidKeyException If the supplied key is
   *         not appropriate.
   */
  public final void init(Key key, SecureRandom random)
    throws InvalidKeyException
  {
    kaSpi.engineInit(key, random);
    virgin = false; // w00t!
  }

  /**
   * Initialize this key agreement with a key and parameters. This
   * method will use the highest-priority {@link
   * java.security.SecureRandom} as its source of randomness.
   *
   * @param key    The key, usually the user's private key.
   * @param params The algorithm parameters.
   * @throws java.security.InvalidAlgorithmParameterException If the
   *         supplied parameters are not appropriate.
   * @throws java.security.InvalidKeyException If the supplied key is
   *         not appropriate.
   */
  public final void init(Key key, AlgorithmParameterSpec params)
    throws InvalidAlgorithmParameterException, InvalidKeyException
  {
    init(key, params, new SecureRandom());
  }

  /**
   * Initialize this key agreement with a key, parameters, and source of
   * randomness.
   *
   * @param key    The key, usually the user's private key.
   * @param params The algorithm parameters.
   * @param random The source of randomness.
   * @throws java.security.InvalidAlgorithmParameterException If the
   *         supplied parameters are not appropriate.
   * @throws java.security.InvalidKeyException If the supplied key is
   *         not appropriate.
   */
  public final void init(Key key, AlgorithmParameterSpec params,
                         SecureRandom random)
    throws InvalidAlgorithmParameterException, InvalidKeyException
  {
    kaSpi.engineInit(key, params, random);
    virgin = false; // w00t!
  }
}
