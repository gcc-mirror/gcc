/* KeyFactory.java --- Key Factory Class
   Copyright (C) 1999, 2003, 2004  Free Software Foundation, Inc.

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

import gnu.java.security.Engine;

import java.lang.reflect.InvocationTargetException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;

/**
 * Key factories are used to convert keys (opaque cryptographic keys of type
 * {@link Key}) into key specifications (transparent representations of the
 * underlying key material).
 * 
 * <p>Key factories are bi-directional. They allow a key class to be converted
 * into a key specification (key material) and back again. For example DSA
 * public keys can be specified as <code>DSAPublicKeySpec</code> or
 * <code>X509EncodedKeySpec</code>. A key factory translates these key
 * specifications.</p>
 *
 * @since 1.2
 * @see Key
 * @see KeySpec
 * @see java.security.spec.DSAPublicKeySpec
 * @see java.security.spec.X509EncodedKeySpec
   @author Mark Benvenuto
 */
public class KeyFactory
{
  /** The service name for key factories. */
  private static final String KEY_FACTORY = "KeyFactory";

  private KeyFactorySpi keyFacSpi;
  private Provider provider;
  private String algorithm;

  /**
   * Constructs a new instance of <code>KeyFactory</code> with the specified
   * parameters.
   * 
   * @param keyFacSpi
   *          the key factory to use.
   * @param provider
   *          the provider to use.
   * @param algorithm
   *          the name of the key algorithm to use.
   */
  protected KeyFactory(KeyFactorySpi keyFacSpi, Provider provider,
		       String algorithm)
  {
    this.keyFacSpi = keyFacSpi;
    this.provider = provider;
    this.algorithm = algorithm;
  }

  /**
   * Returns a new instance of <code>KeyFactory</code> representing the
   * specified key factory.
   * 
   * @param algorithm the name of algorithm to use.
   * @return a new instance repesenting the desired algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by any
   *           provider.
   * @throws IllegalArgumentException if <code>algorithm</code> is
   *           <code>null</code> or is an empty string.
   */
  public static KeyFactory getInstance(String algorithm)
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
   * Returns a new instance of <code>KeyFactory</code> representing the
   * specified key factory from the specified provider.
   * 
   * @param algorithm the name of algorithm to use.
   * @param provider the name of the provider to use.
   * @return a new instance repesenting the desired algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by the
   *           named provider.
   * @throws NoSuchProviderException if the named provider was not found.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code> or empty.
   */
  public static KeyFactory getInstance(String algorithm, String provider)
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
   * Returns a new instance of <code>KeyFactory</code> representing the
   * specified key factory from the designated {@link Provider}.
   * 
   * @param algorithm the name of algorithm to use.
   * @param provider the {@link Provider} to use.
   * @return a new instance repesenting the desired algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by
   *           {@link Provider}.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   * @since 1.4
   * @see Provider
   */
  public static KeyFactory getInstance(String algorithm, Provider provider)
      throws NoSuchAlgorithmException
  {
    StringBuilder sb = new StringBuilder("KeyFactory for algorithm [")
        .append(algorithm).append("] from provider[")
        .append(provider).append("] could not be created");
    Throwable cause;
    try
      {
        Object spi = Engine.getInstance(KEY_FACTORY, algorithm, provider);
        return new KeyFactory((KeyFactorySpi) spi, provider, algorithm);
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
   * Returns the {@link Provider} of this instance.
   * 
   * @return the {@link Provider} of this instance.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Returns the name of the algorithm used.
   * 
   * @return the name of the algorithm used.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Generates a public key from the provided key specification.
   * 
   * @param keySpec
   *          the key specification.
   * @return the public key.
   * @throws InvalidKeySpecException
   *           if the key specification is invalid.
   */
  public final PublicKey generatePublic(KeySpec keySpec)
    throws InvalidKeySpecException
  {
    return keyFacSpi.engineGeneratePublic(keySpec);
  }

  /**
   * Generates a private key from the provided key specification.
   * 
   * @param keySpec
   *          the key specification.
   * @return the private key.
   * @throws InvalidKeySpecException
   *           if the key specification is invalid.
   */
  public final PrivateKey generatePrivate(KeySpec keySpec)
    throws InvalidKeySpecException
  {
    return keyFacSpi.engineGeneratePrivate(keySpec);
  }

  /**
   * Returns a key specification for the given key. <code>keySpec</code>
   * identifies the specification class to return the key material in.
   * 
   * @param key
   *          the key to use.
   * @param keySpec
   *          the specification class to use.
   * @return the key specification in an instance of the requested specification
   *         class.
   * @throws InvalidKeySpecException
   *           the requested key specification is inappropriate for this key or
   *           the key is unrecognized.
   */
  public final <T extends KeySpec> T getKeySpec(Key key, Class<T> keySpec)
    throws InvalidKeySpecException
  {
    return keyFacSpi.engineGetKeySpec(key, keySpec);
  }

  /**
   * Translates the key from an unknown or untrusted provider into a key from
   * this key factory.
   * 
   * @param key
   *          the key to translate from.
   * @return the translated key.
   * @throws InvalidKeyException
   *           if the key cannot be processed by this key factory.
   */
  public final Key translateKey(Key key) throws InvalidKeyException
  {
    return keyFacSpi.engineTranslateKey(key);
  }
}
