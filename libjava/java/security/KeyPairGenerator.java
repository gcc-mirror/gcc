/* KeyPairGenerator.java --- Key Pair Generator Class
   Copyright (C) 1999, 2002, 2003, 2004  Free Software Foundation, Inc.

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

import java.security.spec.AlgorithmParameterSpec;

/**
 * <p>The <code>KeyPairGenerator</code> class is used to generate pairs of
 * public and private keys. Key pair generators are constructed using the
 * <code>getInstance()</code> factory methods (static methods that return
 * instances of a given class).</p>
 *
 * <p>A Key pair generator for a particular algorithm creates a public/private
 * key pair that can be used with this algorithm. It also associates
 * algorithm-specific parameters with each of the generated keys.</p>
 *
 * <p>There are two ways to generate a key pair: in an algorithm-independent
 * manner, and in an algorithm-specific manner. The only difference between the
 * two is the initialization of the object:</p>
 *
 * <ul>
 * <li><b>Algorithm-Independent Initialization</b><br/>
 *     All key pair generators share the concepts of a <i>keysize</i> and a
 *     <i>source of randomness</i>. The <i>keysize</i> is interpreted differently
 *     for different algorithms (e.g., in the case of the <i>DSA</i> algorithm,
 *     the <i>keysize</i> corresponds to the length of the modulus). There is an
 *     <code>initialize()</code> method in this <code>KeyPairGenerator</code>
 *     class that takes these two universally shared types of arguments. There
 *     is also one that takes just a <i>keysize</i> argument, and uses the
 *     {@link SecureRandom} implementation of the highest-priority installed
 *     provider as the <i>source of randomness</i>. (If none of the installed
 *     providers supply an implementation of {@link SecureRandom}, a
 *     system-provided source of randomness is used.)
 *
 *     <p>Since no other parameters are specified when you call the above
 *     algorithm-independent initialize methods, it is up to the provider what
 *     to do about the algorithm-specific parameters (if any) to be associated
 *     with each of the keys.</p>
 *
 *     <p>If the algorithm is the <i>DSA</i> algorithm, and the <i>keysize</i>
 *     (modulus size) is <code>512</code>, <code>768</code>, or <code>1024</code>,
 *     then the <b>GNU</b> provider uses a set of precomputed values for the
 *     <code>p</code>, <code>q</code>, and <code>g</code> parameters. If the
 *     <i>modulus size</i> is not one of the above values, the <b>GNU</b>
 *     provider creates a new set of parameters. Other providers might have
 *     precomputed parameter sets for more than just the three modulus sizes
 *     mentioned above. Still others might not have a list of precomputed
 *     parameters at all and instead always create new parameter sets.</p></li>
 * <li><b>Algorithm-Specific Initialization</b><br/>
 *     For situations where a set of algorithm-specific parameters already
 *     exists (e.g., so-called <i>community parameters</i> in <i>DSA</i>), there
 *     are two initialize methods that have an {@link AlgorithmParameterSpec}
 *     argument. One also has a {@link SecureRandom} argument, while the the
 *     other uses the {@link SecureRandom} implementation of the highest-priority
 *     installed provider as the source of randomness. (If none of the installed
 *     providers supply an implementation of {@link SecureRandom}, a
 *     system-provided source of randomness is used.)</li>
 * </ul>
 *
 * <p>In case the client does not explicitly initialize the
 * <code>KeyPairGenerator</code> (via a call to an initialize method), each
 * provider must supply (and document) a default initialization. For example,
 * the <b>GNU</b> provider uses a default modulus size (keysize) of
 * <code>1024</code> bits.</p>
 *
 * <p>Note that this class is abstract and extends from {@link
 * KeyPairGeneratorSpi} for historical reasons. Application developers should
 * only take notice of the methods defined in this <code>KeyPairGenerator</code>
 * class; all the methods in the superclass are intended for cryptographic
 * service providers who wish to supply their own implementations of key pair
 * generators.</p>
 *
 * @see Signature
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
   * Creates a <code>KeyPairGenerator</code> object for the specified 
   * algorithm.
   *
   * @param algorithm the standard string name of the algorithm. 
   * See Appendix A in the Java Cryptography Architecture API 
   * Specification &amp; Reference for information about standard 
   * algorithm names.
   */
  protected KeyPairGenerator(String algorithm)
  {
    this.algorithm = algorithm;
    this.provider = null;
  }

  /**
   * Returns the standard name of the algorithm for this key pair generator.
   * See Appendix A in the Java Cryptography Architecture API Specification
   * &amp; Reference for information about standard algorithm names.
   *
   * @return the standard string name of the algorithm.
   */
  public String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Generates a <code>KeyPairGenerator</code> object that implements the
   * specified digest algorithm. If the default provider package provides an
   * implementation of the requested digest algorithm, an instance of
   * <code>KeyPairGenerator</code> containing that implementation is returned.
   * If the algorithm is not available in the default package, other packages
   * are searched.
   *
   * @param algorithm the standard string name of the algorithm. See Appendix A
   * in the Java Cryptography Architecture API Specification &amp; Reference for
   * information about standard algorithm names.
   * @return the new <code>KeyPairGenerator</code> object.
   * @throws NoSuchAlgorithmException if the algorithm is not available in the
   * environment.
   */
  public static KeyPairGenerator getInstance(String algorithm)
    throws NoSuchAlgorithmException
  {
    Provider[] p = Security.getProviders();
    for (int i = 0; i < p.length; i++)
      {
        try
          {
            return getInstance(algorithm, p[i]);
	  }
	catch (NoSuchAlgorithmException e)
	  {
	    // Ignored.
	  }
      }

    throw new NoSuchAlgorithmException(algorithm);
  }

  /**
   * Generates a <code>KeyPairGenerator</code> object implementing the 
   * specified algorithm, as supplied from the specified provider, if 
   * such an algorithm is available from the provider.
   *
   * @param algorithm the standard string name of the algorithm. See 
   * Appendix A in the Java Cryptography Architecture API Specification 
   * &amp; Reference for information about standard algorithm names.
   * @param provider the string name of the provider.
   * @return the new <code>KeyPairGenerator</code> object.
   * @throws NoSuchAlgorithmException if the algorithm is not available 
   * from the provider.
   * @throws NoSuchProviderException if the provider is not available in the
   * environment.
   * @throws IllegalArgumentException if the provider name is <code>null</code>
   * or empty.
   * @see Provider
   */
  public static KeyPairGenerator getInstance(String algorithm, String provider)
    throws NoSuchAlgorithmException, NoSuchProviderException
  {
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);

    return getInstance(algorithm, p);
  }

  /**
   * Generates a <code>KeyPairGenerator</code> object implementing the specified
   * algorithm, as supplied from the specified provider, if such an algorithm is
   * available from the provider. Note: the provider doesn't have to be
   * registered.
   *
   * @param algorithm the standard string name of the algorithm. See Appendix A
   * in the Java Cryptography Architecture API Specification &amp; Reference for
   * information about standard algorithm names.
   * @param provider the provider.
   * @return the new <code>KeyPairGenerator</code> object.
   * @throws NoSuchAlgorithmException if the <code>algorithm</code> is not
   * available from the <code>provider</code>.
   * @throws IllegalArgumentException if the <code>provider</code> is
   * <code>null</code>.
   * @since 1.4
   * @see Provider
   */
  public static KeyPairGenerator getInstance(String algorithm, 
					     Provider provider)
    throws NoSuchAlgorithmException
  {
    if (provider == null)
      throw new IllegalArgumentException("Illegal provider");

    Object o = null;
    try
      {
        o = Engine.getInstance(KEY_PAIR_GENERATOR, algorithm, provider);
      }
    catch (java.lang.reflect.InvocationTargetException ite)
      {
	throw new NoSuchAlgorithmException(algorithm);
      }

    KeyPairGenerator result = null;
    if (o instanceof KeyPairGeneratorSpi)
      {
	result = new DummyKeyPairGenerator((KeyPairGeneratorSpi) o, algorithm);
      }
    else if (o instanceof KeyPairGenerator)
      {
        result = (KeyPairGenerator) o;
        result.algorithm = algorithm;
      }
    result.provider = provider;
    return result;
  }

  /**
   * Returns the provider of this key pair generator object.
   *
   * @return the provider of this key pair generator object.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Initializes the key pair generator for a certain keysize using a default
   * parameter set and the {@link SecureRandom} implementation of the
   * highest-priority installed provider as the source of randomness. (If none
   * of the installed providers supply an implementation of {@link SecureRandom},
   * a system-provided source of randomness is used.)
   *
   * @param keysize the keysize. This is an algorithm-specific metric, such as
   * modulus length, specified in number of bits.
   * @throws InvalidParameterException if the keysize is not supported by this
   * <code>KeyPairGenerator</code> object.
   */
  public void initialize(int keysize)
  {
    initialize(keysize, new SecureRandom());
  }

  /**
   * Initializes the key pair generator for a certain keysize with the given
   * source of randomness (and a default parameter set).
   *
   * @param keysize the keysize. This is an algorithm-specific metric, such as
   * modulus length, specified in number of bits.
   * @param random the source of randomness.
   * @throws InvalidParameterException if the <code>keysize</code> is not
   * supported by this <code>KeyPairGenerator</code> object.
   * @since 1.2
   */
  public void initialize(int keysize, SecureRandom random)
  {
    initialize(keysize, random);
  }

  /**
   * <p>Initializes the key pair generator using the specified parameter set and
   * the {@link SecureRandom} implementation of the highest-priority installed
   * provider as the source of randomness. (If none of the installed providers
   * supply an implementation of {@link SecureRandom}, a system-provided source
   * of randomness is used.)</p>
   *
   * <p>This concrete method has been added to this previously-defined abstract
   * class. This method calls the
   * {@link KeyPairGeneratorSpi#initialize(AlgorithmParameterSpec, SecureRandom)}
   * initialize method, passing it <code>params</code> and a source of
   * randomness (obtained from the highest-priority installed provider or
   * system-provided if none of the installed providers supply one). That
   * initialize method always throws an {@link UnsupportedOperationException}
   * if it is not overridden by the provider.</p>
   *
   * @param params the parameter set used to generate the keys.
   * @throws InvalidAlgorithmParameterException if the given parameters are
   * inappropriate for this key pair generator.
   * @since 1.2
   */
  public void initialize(AlgorithmParameterSpec params)
    throws InvalidAlgorithmParameterException
  {
    initialize(params, new SecureRandom());
  }

  /**
   * <p>Initializes the key pair generator with the given parameter set and
   * source of randomness.</p>
   *
   * <p>This concrete method has been added to this previously-defined abstract
   * class. This method calls the
   * {@link KeyPairGeneratorSpi#initialize(AlgorithmParameterSpec, SecureRandom)}
   * initialize method, passing it <code>params</code> and <code>random</code>.
   * That initialize method always throws an {@link UnsupportedOperationException}
   * if it is not overridden by the provider.</p>
   *
   * @param params the parameter set used to generate the keys.
   * @param random the source of randomness.
   * @throws InvalidAlgorithmParameterException if the given parameters are
   * inappropriate for this key pair generator.
   * @since 1.2
   */
  public void initialize(AlgorithmParameterSpec params, SecureRandom random)
    throws InvalidAlgorithmParameterException
  {
    super.initialize(params, random);
  }

  /**
   * <p>Generates a key pair.</p>
   *
   * <p>If this <code>KeyPairGenerator</code> has not been initialized
   * explicitly, provider-specific defaults will be used for the size and other
   * (algorithm-specific) values of the generated keys.</p>
   *
   * <p>This will generate a new key pair every time it is called.</p>
   *
   * <p>This method is functionally equivalent to {@link #generateKeyPair()}.</p>
   *
   * @return the generated key pair.
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
   * <p>Generates a key pair.</p>
   *
   * <p>If this <code>KeyPairGenerator</code> has not been initialized
   * explicitly, provider-specific defaults will be used for the size and other
   * (algorithm-specific) values of the generated keys.</p>
   *
   * <p>This will generate a new key pair every time it is called.</p>
   *
   * <p>This method is functionally equivalent to {@link #genKeyPair()}.</p>
   *
   * @return the generated key pair.
   */
  public KeyPair generateKeyPair()
  {
    return genKeyPair();
  }
}
