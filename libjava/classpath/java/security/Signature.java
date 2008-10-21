/* Signature.java --- Signature Class
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
import java.nio.ByteBuffer;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.security.spec.AlgorithmParameterSpec;

/**
 * <code>Signature</code> is used to provide an interface to digital signature
 * algorithms. Digital signatures provide authentication and data integrity of
 * digital data.
 * 
 * <p>The GNU provider provides the NIST standard DSA which uses DSA and SHA-1.
 * It can be specified by SHA/DSA, SHA-1/DSA or its OID. If the RSA signature
 * algorithm is provided then it could be MD2/RSA. MD5/RSA, or SHA-1/RSA. The
 * algorithm must be specified because there is no default.</p>
 * 
 * <p>Signature provides implementation-independent algorithms which are
 * requested by the user through the <code>getInstance()<?code> methods. It can
 * be requested by specifying just the algorithm name or by specifying both the
 * algorithm name and provider name.</p>
 * 
 * <p>The three phases of using <code>Signature</code> are:</p>
 * 
 * <ol>
 *   <li>Initializing:
 *     <ul>
 *       <li>It must be initialized with a private key for signing.</li>
 *       <li>It must be initialized with a public key for verifying.</li>
 *   </li>
 *   
 *   <li>Updating:
 *   <p>Update the bytes for signing or verifying with calls to update.</p>
 *   </li>
 *   
 *   <li>Signing or Verify the signature on the currently stored bytes by
 *   calling sign or verify.</li>
 * </ol>
 *
 * @author Mark Benvenuto  (ivymccough@worldnet.att.net)
 */
public abstract class Signature extends SignatureSpi
{
  /** Service name for signatures. */
  private static final String SIGNATURE = "Signature";

  /**
   * Possible state value which signifies that this instance has not yet been
   * initialized.
   */
  protected static final int UNINITIALIZED = 0;

  /**
   * Possible state value which signifies that this instance has been
   * initialized for signing purposes.
   */
  protected static final int SIGN = 2;

  /**
   * Possible state value which signifies that this instance has been
   * initialized for verification purposes.
   */
  protected static final int VERIFY = 3;

  /** Current sate of this instance. */
  protected int state = UNINITIALIZED;

  private String algorithm;
  Provider provider;

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Constructs a new <code>Signature</code> instance for a designated digital
   * signature algorithm.
   * 
   * @param algorithm
   *          the algorithm to use.
   */
  protected Signature(String algorithm)
  {
    this.algorithm = algorithm;
    state = UNINITIALIZED;
  }

  /**
   * Returns an instance of <code>Signature</code> representing the specified
   * signature.
   * 
   * @param algorithm the algorithm to use.
   * @return a new instance repesenting the desired algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by any
   *           provider.
   * @throws IllegalArgumentException if <code>algorithm</code> is
   *           <code>null</code> or is an empty string.
   */
  public static Signature getInstance(String algorithm)
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
   * Returns an instance of <code>Signature</code> representing the specified
   * signature from the named provider.
   * 
   * @param algorithm the algorithm to use.
   * @param provider the name of the provider to use.
   * @return a new instance repesenting the desired algorithm.
   * @throws NoSuchProviderException if the named provider was not found.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by the
   *           named provider.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code> or empty.
   */
  public static Signature getInstance(String algorithm, String provider)
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
   * Returns an instance of <code>Signature</code> representing the specified
   * signature from the specified {@link Provider}.
   * 
   * @param algorithm the algorithm to use.
   * @param provider the {@link Provider} to use.
   * @return a new instance repesenting the desired algorithm.
   * @throws NoSuchAlgorithmException if the algorithm is not implemented by the
   *           {@link Provider}.
   * @throws IllegalArgumentException if either <code>algorithm</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>algorithm</code> is an empty string.
   */
  public static Signature getInstance(String algorithm, Provider provider)
    throws NoSuchAlgorithmException
  {
    CPStringBuilder sb = new CPStringBuilder("Signature algorithm [")
        .append(algorithm).append("] from provider[")
        .append(provider).append("] ");
    Object o;
    try
      {
        o = Engine.getInstance(SIGNATURE, algorithm, provider);
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
    Signature result;
    if (o instanceof SignatureSpi)
      result = new DummySignature((SignatureSpi) o, algorithm);
    else if (o instanceof Signature)
      {
        result = (Signature) o;
        result.algorithm = algorithm;
      }
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
   * Initializes this instance with the public key for verification purposes.
   * 
   * @param publicKey
   *          the public key to verify with.
   * @throws InvalidKeyException
   *           if the key is invalid.
   */
  public final void initVerify(PublicKey publicKey) throws InvalidKeyException
  {
    state = VERIFY;
    engineInitVerify(publicKey);
  }

  /**
   * Verify a signature with a designated {@link Certificate}. This is a FIPS
   * 140-1 compatible method since it verifies a signature with a certificate.
   * 
   * <p>If the {@link Certificate} is an X.509 one, has a <i>KeyUsage</i>
   * parameter and that parameter indicates this key is not to be used for
   * signing then an exception is thrown.</p>
   * 
   * @param certificate
   *          a {@link Certificate} containing a public key to verify with.
   * @throws InvalidKeyException if the key is invalid.
   */
  public final void initVerify(Certificate certificate)
    throws InvalidKeyException
  {
    state = VERIFY;
    if (certificate.getType().equals("X509"))
      {
        X509Certificate cert = (X509Certificate) certificate;
        boolean[]array = cert.getKeyUsage();
        if (array != null && array[0] == false)
          throw new InvalidKeyException(
              "KeyUsage of this Certificate indicates it cannot be used for digital signing");
      }
    this.initVerify(certificate.getPublicKey());
  }

  /**
   * Initializes this class with the private key for signing purposes.
   * 
   * @param privateKey
   *          the private key to sign with.
   * @throws InvalidKeyException
   *           if the key is invalid.
   */
  public final void initSign(PrivateKey privateKey) throws InvalidKeyException
  {
    state = SIGN;
    engineInitSign(privateKey);
  }

  /**
   * Initializes this class with the private key and source of randomness for
   * signing purposes.
   * 
   * @param privateKey
   *          the private key to sign with.
   * @param random
   *          the {@link SecureRandom} to use.
   * @throws InvalidKeyException
   *           if the key is invalid.
   */
  public final void initSign(PrivateKey privateKey, SecureRandom random)
    throws InvalidKeyException
  {
    state = SIGN;
    engineInitSign(privateKey, random);
  }

  /**
   * Returns the signature bytes of all the data fed to this instance. The
   * format of the output depends on the underlying signature algorithm.
   * 
   * @return the signature bytes.
   * @throws SignatureException
   *           if the engine is not properly initialized.
   */
  public final byte[] sign() throws SignatureException
  {
    if (state == SIGN)
      return engineSign();
    else
      throw new SignatureException();
  }

  /**
   * Generates signature bytes of all the data fed to this instance and stores
   * it in the designated array. The format of the result depends on the
   * underlying signature algorithm.
   * 
   * <p>After calling this method, the instance is reset to its initial state
   * and can then be used to generate additional signatures.</p>
   * 
   * <p><b>IMPLEMENTATION NOTE:</b> Neither this method nor the GNU provider
   * will return partial digests. If <code>len</code> is less than the
   * signature length, this method will throw a {@link SignatureException}. If
   * it is greater than or equal then it is ignored.</p>
   * 
   * @param outbuf
   *          array of bytes of where to store the resulting signature bytes.
   * @param offset
   *          the offset to start at in the array.
   * @param len
   *          the number of the bytes to use in the array.
   * @return the real number of bytes used.
   * @throws SignatureException
   *           if the engine is not properly initialized.
   * @since 1.2
   */
  public final int sign(byte[] outbuf, int offset, int len)
    throws SignatureException
  {
    if (state == SIGN)
      return engineSign(outbuf, offset, len);
    else
      throw new SignatureException();
  }

  /**
   * Verifies a designated signature.
   * 
   * @param signature
   *          the signature bytes to verify.
   * @return <code>true</code> if verified, <code>false</code> otherwise.
   * @throws SignatureException
   *           if the engine is not properly initialized or the signature does
   *           not check.
   */
  public final boolean verify(byte[]signature) throws SignatureException
  {
    if (state == VERIFY)
      return engineVerify(signature);
    else
      throw new SignatureException();
  }

  /**
   * Verifies a designated signature.
   * 
   * @param signature
   *          the signature bytes to verify.
   * @param offset
   *          the offset to start at in the array.
   * @param length
   *          the number of the bytes to use from the array.
   * @return <code>true</code> if verified, <code>false</code> otherwise.
   * @throws IllegalArgumentException
   *           if the <code>signature</code> byte array is <code>null</code>,
   *           or the <code>offset</code> or <code>length</code> is less
   *           than <code>0</code>, or the sum of the <code>offset</code>
   *           and <code>length</code> is greater than the length of the
   *           <code>signature</code> byte array.
   * @throws SignatureException
   *           if the engine is not properly initialized or the signature does
   *           not check.
   */
  public final boolean verify(byte[] signature, int offset, int length)
    throws SignatureException
  {
    if (state != VERIFY)
      throw new SignatureException("illegal state");

    if (signature == null)
      throw new IllegalArgumentException("signature is null");
    if (offset < 0)
      throw new IllegalArgumentException("offset is less than 0");
    if (length < 0)
      throw new IllegalArgumentException("length is less than 0");
    if (offset + length < signature.length)
      throw new IllegalArgumentException("range is out of bounds");

    return engineVerify(signature, offset, length);
  }

  /**
   * Updates the data to be signed or verified with the specified byte.
   * 
   * @param b
   *          the byte to update with.
   * @throws SignatureException
   *           if the engine is not properly initialized.
   */
  public final void update(byte b) throws SignatureException
  {
    if (state != UNINITIALIZED)
      engineUpdate(b);
    else
      throw new SignatureException();
  }

  /**
   * Updates the data to be signed or verified with the specified bytes.
   * 
   * @param data
   *          the array of bytes to use.
   * @throws SignatureException
   *           if the engine is not properly initialized.
   */
  public final void update(byte[]data) throws SignatureException
  {
    if (state != UNINITIALIZED)
      engineUpdate(data, 0, data.length);
    else
      throw new SignatureException();
  }

  /**
   * Updates the data to be signed or verified with the specified bytes.
   * 
   * @param data
   *          an array of bytes to use.
   * @param off
   *          the offset to start at in the array.
   * @param len
   *          the number of bytes to use from the array.
   * @throws SignatureException
   *           if the engine is not properly initialized.
   */
  public final void update(byte[]data, int off, int len)
    throws SignatureException
  {
    if (state != UNINITIALIZED)
      engineUpdate(data, off, len);
    else
      throw new SignatureException();
  }
  
  /**
   * Update this signature with the {@link java.nio.Buffer#remaining()}
   * bytes of the input buffer.
   * 
   * @param input The input buffer.
   * @throws SignatureException If this instance was not properly
   *  initialized.
   */
  public final void update(ByteBuffer input) throws SignatureException
  {
    if (state != UNINITIALIZED)
      engineUpdate(input);
    else
      throw new SignatureException("not initialized");
  }

  /**
   * Returns the name of the algorithm currently used. The names of algorithms
   * are usually SHA/DSA or SHA/RSA.
   * 
   * @return name of algorithm.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Returns a rstring representation of this instance.
   * 
   * @return a rstring representation of this instance.
   */
  public String toString()
  {
    return (algorithm + " Signature");
  }

  /**
   * Sets the specified algorithm parameter to the specified value.
   * 
   * @param param
   *          the parameter name.
   * @param value
   *          the parameter value.
   * @throws InvalidParameterException
   *           if the parameter is invalid, the parameter is already set and
   *           can not be changed, a security exception occured, etc.
   * @deprecated use the other setParameter
   */
  public final void setParameter(String param, Object value)
    throws InvalidParameterException
  {
    engineSetParameter(param, value);
  }

  /**
   * Sets the signature engine with the specified {@link AlgorithmParameterSpec}.
   * 
   * <p>By default, and unless overriden by the concrete SPI, this method always
   * throws an {@link UnsupportedOperationException}.</p>
   * 
   * @param params
   *          the parameters to use for intializing this instance.
   * @throws InvalidParameterException
   *           if the parameter is invalid, the parameter is already set and
   *           cannot be changed, a security exception occured, etc.
   */
  public final void setParameter(AlgorithmParameterSpec params)
    throws InvalidAlgorithmParameterException
  {
    engineSetParameter(params);
  }

  /**
   * Return the parameters of the algorithm used in this instance as an
   * {@link AlgorithmParameters}.
   * 
   * @return the parameters used with this instance, or <code>null</code> if
   *         this instance does not use any parameters.
   */
  public final AlgorithmParameters getParameters()
  {
    return engineGetParameters();
  }

  /**
   * Returns the value for the specified algorithm parameter.
   * 
   * @param param
   *          the parameter name.
   * @return the parameter value.
   * @throws InvalidParameterException
   *           if the parameter is invalid.
   * @deprecated use the other getParameter
   */
  public final Object getParameter(String param)
    throws InvalidParameterException
  {
    return engineGetParameter(param);
  }

  /**
   * Returns a clone of this instance.
   * 
   * @return a clone of this instace.
   * @throws CloneNotSupportedException
   *           if the implementation does not support cloning.
   */
  public Object clone() throws CloneNotSupportedException
  {
    return super.clone();
  }
}
