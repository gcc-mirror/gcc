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

import gnu.java.security.Engine;

import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;

/**
 * <p>Key factories are used to convert keys (opaque cryptographic keys of type
 * {@link Key}) into key specifications (transparent representations of the
 * underlying key material), and vice versa.</p>
 *
 * <p>Key factories are bi-directional. That is, they allow you to build an
 * opaque key object from a given key specification (key material), or to
 * retrieve the underlying key material of a key object in a suitable format.</p>
 *
 * <p>Multiple compatible key specifications may exist for the same key. For
 * example, a <i>DSA</i> public key may be specified using {@link
 * java.security.spec.DSAPublicKeySpec} or {@link
 * java.security.spec.X509EncodedKeySpec}. A key factory can be used to
 * translate between compatible key specifications.</p>
 *
 * <p>The following is an example of how to use a key factory in order to
 * instantiate a <i>DSA</i> public key from its encoding. Assume Alice has
 * received a digital signature from Bob. Bob also sent her his public key (in
 * encoded format) to verify his signature. Alice then performs the following
 * actions:
 *
 * <pre>
 *  X509EncodedKeySpec bobPubKeySpec = new X509EncodedKeySpec(bobEncodedPubKey);
 *  KeyFactory keyFactory = KeyFactory.getInstance("DSA");
 *  PublicKey bobPubKey = keyFactory.generatePublic(bobPubKeySpec);
 *  Signature sig = Signature.getInstance("DSA");
 *  sig.initVerify(bobPubKey);
 *  sig.update(data);
 *  sig.verify(signature);
 * </pre>
 *
 * @since 1.2
 * @see Key
 * @see PublicKey
 * @see PrivateKey
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
   * Creates a <code>KeyFactory</code> object.
   *
   * @param keyFacSpi the delegate.
   * @param provider the provider.
   * @param algorithm the name of the algorithm to associate with this
   * <code>KeyFactory</code>.
   */
  protected KeyFactory(KeyFactorySpi keyFacSpi, Provider provider,
		       String algorithm)
  {
    this.keyFacSpi = keyFacSpi;
    this.provider = provider;
    this.algorithm = algorithm;
  }

  /**
   * Generates a <code>KeyFactory</code> object that implements the specified
   * algorithm. If the default provider package provides an implementation of
   * the requested algorithm, an instance of <code>KeyFactory</code> containing
   * that implementation is returned. If the algorithm is not available in the
   * default package, other packages are searched.
   *
   * @param algorithm the name of the requested key algorithm. See Appendix A
   * in the Java Cryptography Architecture API Specification &amp; Reference
   * for information about standard algorithm names.
   * @return a <code>KeyFactory</code> object for the specified algorithm.
   * @throws NoSuchAlgorithmException if the requested algorithm is not
   * available in the default provider package or any of the other provider
   * packages that were searched.
   */
  public static KeyFactory getInstance(String algorithm)
    throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();
    for (int i = 0; i < p.length; i++)
      try
        {
          return getInstance(algorithm, p[i]);
        }
      catch (NoSuchAlgorithmException e)
	{
	  // Ignore.
	}

    throw new NoSuchAlgorithmException(algorithm);
  }

  /**
   * Generates a <code>KeyFactory</code> object for the specified algorithm
   * from the specified provider.
   *
   * @param algorithm the name of the requested key algorithm. See Appendix A
   * in the Java Cryptography Architecture API Specification &amp; Reference
   * for information about standard algorithm names.
   * @param provider the name of the provider.
   * @return a <code>KeyFactory</code> object for the specified algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not available from
   * the specified provider.
   * @throws NoSuchProviderException if the provider has not been configured.
   * @throws IllegalArgumentException if the provider name is null or empty.
   * @see Provider
   */
  public static KeyFactory getInstance(String algorithm, String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    if (provider == null || provider.length() == 0)
      throw new IllegalArgumentException("Illegal provider");

    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);

    return getInstance(algorithm, p);
  }

  /**
   * Generates a <code>KeyFactory</code> object for the specified algorithm from
   * the specified provider. Note: the <code>provider</code> doesn't have to be
   * registered.
   *
   * @param algorithm the name of the requested key algorithm. See Appendix A
   * in the Java Cryptography Architecture API Specification &amp; Reference for
   * information about standard algorithm names.
   * @param provider the provider.
   * @return a <code>KeyFactory</code> object for the specified algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not available from
   * the specified provider.
   * @throws IllegalArgumentException if the <code>provider</code> is
   * <code>null</code>.
   * @since 1.4
   * @see Provider
   */
  public static KeyFactory getInstance(String algorithm, Provider provider)
    throws NoSuchAlgorithmException
  {
    if (provider == null)
      throw new IllegalArgumentException("Illegal provider");

    try
      {
	return new KeyFactory((KeyFactorySpi)
	  Engine.getInstance(KEY_FACTORY, algorithm, provider),
          provider, algorithm);
      }
    catch (java.lang.reflect.InvocationTargetException ite)
      {
	throw new NoSuchAlgorithmException(algorithm);
      }
    catch (ClassCastException cce)
      {
	throw new NoSuchAlgorithmException(algorithm);
      } 
  }

  /**
   * Returns the provider of this key factory object.
   *
   * @return the provider of this key factory object.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Gets the name of the algorithm associated with this <code>KeyFactory</code>.
   *
   * @return the name of the algorithm associated with this
   * <code>KeyFactory</code>.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Generates a public key object from the provided key specification (key
   * material).
   *
   * @param keySpec the specification (key material) of the public key.
   * @return the public key.
   * @throws InvalidKeySpecException if the given key specification is
   * inappropriate for this key factory to produce a public key.
   */
  public final PublicKey generatePublic(KeySpec keySpec)
    throws InvalidKeySpecException
  {
    return keyFacSpi.engineGeneratePublic(keySpec);
  }

  /**
   * Generates a private key object from the provided key specification (key
   * material).
   *
   * @param keySpec the specification (key material) of the private key.
   * @return the private key.
   * @throws InvalidKeySpecException if the given key specification is
   * inappropriate for this key factory to produce a private key.
   */
  public final PrivateKey generatePrivate(KeySpec keySpec)
    throws InvalidKeySpecException
  {
    return keyFacSpi.engineGeneratePrivate(keySpec);
  }

  /**
   * Returns a specification (key material) of the given key object.
   * <code>keySpec</code> identifies the specification class in which the key
   * material should be returned. It could, for example, be
   * <code>DSAPublicKeySpec.class</code>, to indicate that the key material
   * should be returned in an instance of the {@link
   * java.security.spec.DSAPublicKeySpec} class.
   *
   * @param key the key.
   * @param keySpec the specification class in which the key material should be
   * returned.
   * @return the underlying key specification (key material) in an instance of
   * the requested specification class.
   * @throws InvalidKeySpecException if the requested key specification is
   * inappropriate for the given key, or the given key cannot be processed
   * (e.g., the given key has an unrecognized algorithm or format).
   */
  public final KeySpec getKeySpec(Key key, Class keySpec)
    throws InvalidKeySpecException
  {
    return keyFacSpi.engineGetKeySpec(key, keySpec);
  }

  /**
   * Translates a key object, whose provider may be unknown or potentially
   * untrusted, into a corresponding key object of this key factory.
   *
   * @param key the key whose provider is unknown or untrusted.
   * @return the translated key.
   * @throws InvalidKeyException if the given key cannot be processed by this
   * key factory.
   */
  public final Key translateKey(Key key) throws InvalidKeyException
  {
    return keyFacSpi.engineTranslateKey(key);
  }
}
