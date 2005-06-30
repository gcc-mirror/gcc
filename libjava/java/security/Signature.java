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

import gnu.java.security.Engine;

import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.security.spec.AlgorithmParameterSpec;

/**
 * <p>This <code>Signature</code> class is used to provide applications the
 * functionality of a digital signature algorithm. Digital signatures are used
 * for authentication and integrity assurance of digital data.</p>
 *
 * <p>The signature algorithm can be, among others, the NIST standard <i>DSS</i>,
 * using <i>DSA</i> and <i>SHA-1</i>. The <i>DSA</i> algorithm using the
 * <i>SHA-1</i> message digest algorithm can be specified as <code>SHA1withDSA
 * </code>. In the case of <i>RSA</i>, there are multiple choices for the
 * message digest algorithm, so the signing algorithm could be specified as, for
 * example, <code>MD2withRSA</code>, <code>MD5withRSA</code>, or
 * <code>SHA1withRSA</code>. The algorithm name must be specified, as there is
 * no default.</p>
 *
 * <p>Like other algorithm-based classes in Java Security, <code>Signature</code>
 * provides implementation-independent algorithms, whereby a caller (application
 * code) requests a particular signature algorithm and is handed back a properly
 * initialized <code>Signature</code> object. It is also possible, if desired,
 * to request a particular algorithm from a particular provider. See the
 * <code>getInstance()</code> methods.</p>
 *
 * <p>Thus, there are two ways to request a <code>Signature</code> algorithm
 * object: by specifying either just an algorithm name, or both an algorithm
 * name and a package provider.</p>
 *
 * <p>If just an algorithm name is specified, the system will determine if there
 * is an implementation of the algorithm requested available in the environment,
 * and if there is more than one, if there is a preferred one.</p>
 *
 * <p>If both an algorithm name and a package provider are specified, the system
 * will determine if there is an implementation of the algorithm in the package
 * requested, and throw an exception if there is not.</p>
 *
 * <p>A <code>Signature</code> object can be used to generate and verify digital
 * signatures.</p>
 *
 * <p>There are three phases to the use of a <code>Signature</code> object for
 * either signing data or verifying a signature:</p>
 *
 * <ol>
 * <li>Initialization, with either
 *     <ul>
 *     <li>a public key, which initializes the signature for verification
 *         (see <code>initVerify()</code>), or</li>
 *     <li>a private key (and optionally a Secure Random Number Generator),
 *         which initializes the signature for signing (see
 *         {@link #initSign(PrivateKey)} and {@link #initSign(PrivateKey, SecureRandom)}
 *         ).</li>
 *     </ul></li>
 * <li>Updating<br/>
 *     Depending on the type of initialization, this will update the bytes to
 *     be signed or verified. See the update methods.<br/></li>
 * <li>Signing or Verifying a signature on all updated bytes. See the
 *     <code>sign()</code> methods and the <code>verify()</code> method.</li>
 *  </ol>
 *
 * <p>Note that this class is abstract and extends from {@link SignatureSpi} for
 * historical reasons. Application developers should only take notice of the
 * methods defined in this <code>Signature</code> class; all the methods in the
 * superclass are intended for cryptographic service providers who wish to
 * supply their own implementations of digital signature algorithms.
 *
 * @author Mark Benvenuto  (ivymccough@worldnet.att.net)
 */
public abstract class Signature extends SignatureSpi
{
  /** Service name for signatures. */
  private static final String SIGNATURE = "Signature";

  /**
   * Possible <code>state</code> value, signifying that this signature object
   * has not yet been initialized.
   */
  protected static final int UNINITIALIZED = 0;

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Possible <code>state</code> value, signifying that this signature object
   * has been initialized for signing.
   */
  protected static final int SIGN = 2;

  /**
   * Possible <code>state</code> value, signifying that this signature object
   * has been initialized for verification.
   */
  protected static final int VERIFY = 3;

  /** Current state of this signature object. */
  protected int state = UNINITIALIZED;

  private String algorithm;
  Provider provider;

  /**
   * Creates a <code>Signature</code> object for the specified algorithm.
   *
   * @param algorithm the standard string name of the algorithm. See Appendix A
   * in the Java Cryptography Architecture API Specification &amp; Reference for
   * information about standard algorithm names.
   */
  protected Signature(String algorithm)
  {
    this.algorithm = algorithm;
    state = UNINITIALIZED;
  }

  /**
   * Generates a <code>Signature</code> object that implements the specified
   * digest algorithm. If the default provider package provides an
   * implementation of the requested digest algorithm, an instance of
   * <code>Signature</code> containing that implementation is returned. If the
   * algorithm is not available in the default package, other packages are
   * searched.
   *
   * @param algorithm the standard name of the algorithm requested. See Appendix
   * A in the Java Cryptography Architecture API Specification &amp; Reference
   * for information about standard algorithm names.
   * @return the new Signature object.
   * @throws NoSuchAlgorithmException if the algorithm is not available in the
   * environment.
   */
  public static Signature getInstance(String algorithm)
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
   * Generates a <code>Signature</code> object implementing the specified
   * algorithm, as supplied from the specified provider, if such an algorithm
   * is available from the provider.
   *
   * @param algorithm the name of the algorithm requested. See Appendix A in
   * the Java Cryptography Architecture API Specification &amp; Reference for
   * information about standard algorithm names.
   * @param provider the name of the provider.
   * @return the new <code>Signature</code> object.
   * @throws NoSuchAlgorithmException if the algorithm is not available in the
   * package supplied by the requested provider.
   * @throws NoSuchProviderException if the provider is not available in the
   * environment.
   * @throws IllegalArgumentException if the provider name is <code>null</code>
   * or empty.
   * @see Provider
   */
  public static Signature getInstance(String algorithm, String provider)
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
   * Generates a <code>Signature</code> object implementing the specified
   * algorithm, as supplied from the specified provider, if such an algorithm
   * is available from the provider. Note: the provider doesn't have to be
   * registered.
   *
   * @param algorithm the name of the algorithm requested. See Appendix A in
   * the Java Cryptography Architecture API Specification &amp; Reference for
   * information about standard algorithm names.
   * @param provider the provider.
   * @return the new <code>Signature</code> object.
   * @throws NoSuchAlgorithmException if the <code>algorithm</code> is not
   * available in the package supplied by the requested <code>provider</code>.
   * @throws IllegalArgumentException if the <code>provider</code> is
   * <code>null</code>.
   * @since 1.4
   * @see Provider
   */
  public static Signature getInstance(String algorithm, Provider provider)
    throws NoSuchAlgorithmException
  {
    if (provider == null)
      throw new IllegalArgumentException("Illegal provider");

    Signature result = null;
    Object o = null;
    try
      {
        o = Engine.getInstance(SIGNATURE, algorithm, provider);
      }
    catch (java.lang.reflect.InvocationTargetException ite)
      {
        throw new NoSuchAlgorithmException(algorithm);
      }

    if (o instanceof SignatureSpi)
      {
        result = new DummySignature((SignatureSpi) o, algorithm);
      }
    else if (o instanceof Signature)
      {
        result = (Signature) o;
        result.algorithm = algorithm;
      }
    else
      {
        throw new NoSuchAlgorithmException(algorithm);
      }
    result.provider = provider;
    return result;
  }

  /**
   * Returns the provider of this signature object.
   *
   * @return the provider of this signature object.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Initializes this object for verification. If this method is called again
   * with a different argument, it negates the effect of this call.
   *
   * @param publicKey the public key of the identity whose signature is going
   * to be verified.
   * @throws InvalidKeyException if the key is invalid.
   */
  public final void initVerify(PublicKey publicKey) throws InvalidKeyException
  {
    state = VERIFY;
    engineInitVerify(publicKey);
  }

  /**
   * <p>Initializes this object for verification, using the public key from the
   * given certificate.</p>
   *
   * <p>If the certificate is of type <i>X.509</i> and has a <i>key usage</i>
   * extension field marked as <i>critical</i>, and the value of the <i>key
   * usage</i> extension field implies that the public key in the certificate
   * and its corresponding private key are not supposed to be used for digital
   * signatures, an {@link InvalidKeyException} is thrown.</p>
   *
   * @param certificate the certificate of the identity whose signature is
   * going to be verified.
   * @throws InvalidKeyException if the public key in the certificate is not
   * encoded properly or does not include required parameter information or
   * cannot be used for digital signature purposes.
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
   * Initialize this object for signing. If this method is called again with a
   * different argument, it negates the effect of this call.
   *
   * @param privateKey the private key of the identity whose signature is going
   * to be generated.
   * @throws InvalidKeyException if the key is invalid.
   */
  public final void initSign(PrivateKey privateKey) throws InvalidKeyException
  {
    state = SIGN;
    engineInitSign(privateKey);
  }

  /**
   * Initialize this object for signing. If this method is called again with a
   * different argument, it negates the effect of this call.
   *
   * @param privateKey the private key of the identity whose signature is going
   * to be generated.
   * @param random the source of randomness for this signature.
   * @throws InvalidKeyException if the key is invalid.
   */
  public final void initSign(PrivateKey privateKey, SecureRandom random)
    throws InvalidKeyException
  {
    state = SIGN;
    engineInitSign(privateKey, random);
  }

  /**
   * <p>Returns the signature bytes of all the data updated. The format of the
   * signature depends on the underlying signature scheme.</p>
   *
   * <p>A call to this method resets this signature object to the state it was
   * in when previously initialized for signing via a call to
   * <code>initSign(PrivateKey)</code>. That is, the object is reset and
   * available to generate another signature from the same signer, if desired,
   * via new calls to <code>update()</code> and <code>sign()</code>.</p>
   *
   * @return the signature bytes of the signing operation's result.
   * @throws SignatureException if this signature object is not initialized
   * properly.
   */
  public final byte[] sign() throws SignatureException
  {
    if (state == SIGN)
      return engineSign();
    else
      throw new SignatureException();
  }

  /**
   * <p>Finishes the signature operation and stores the resulting signature
   * bytes in the provided buffer <code>outbuf</code>, starting at <code>offset
   * </code>. The format of the signature depends on the underlying signature
   * scheme.</p>
   *
   * <p>This signature object is reset to its initial state (the state it was
   * in after a call to one of the <code>initSign()</code> methods) and can be
   * reused to generate further signatures with the same private key.</p>
   *
   * @param outbuf buffer for the signature result.
   * @param offset offset into outbuf where the signature is stored.
   * @param len number of bytes within outbuf allotted for the signature.
   * @return the number of bytes placed into outbuf.
   * @throws SignatureException if an error occurs or len is less than the
   * actual signature length.
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
   * <p>Verifies the passed-in signature.</p>
   *
   * <p>A call to this method resets this signature object to the state it was
   * in when previously initialized for verification via a call to
   * <code>initVerify(PublicKey)</code>. That is, the object is reset and
   * available to verify another signature from the identity whose public key
   * was specified in the call to <code>initVerify()</code>.</p>
   *
   * @param signature the signature bytes to be verified.
   * @return <code>true</code> if the signature was verified, <code>false</code>
   * if not.
   * @throws SignatureException if this signature object is not initialized
   * properly, or the passed-in signature is improperly encoded or of the wrong
   * type, etc.
   */
  public final boolean verify(byte[]signature) throws SignatureException
  {
    if (state == VERIFY)
      return engineVerify(signature);
    else
      throw new SignatureException();
  }

  /**
   * <p>Verifies the passed-in <code>signature</code> in the specified array of
   * bytes, starting at the specified <code>offset</code>.</p>
   *
   * <p>A call to this method resets this signature object to the state it was
   * in when previously initialized for verification via a call to
   * <code>initVerify(PublicKey)</code>. That is, the object is reset and
   * available to verify another signature from the identity whose public key
   * was specified in the call to <code>initVerify()</code>.</p>
   *
   * @param signature the signature bytes to be verified.
   * @param offset the offset to start from in the array of bytes.
   * @param length the number of bytes to use, starting at offset.
   * @return <code>true</code> if the signature was verified, <code>false</code>
   * if not.
   * @throws SignatureException if this signature object is not initialized
   * properly, or the passed-in <code>signature</code> is improperly encoded or
   * of the wrong type, etc.
   * @throws IllegalArgumentException if the <code>signature</code> byte array
   * is <code>null</code>, or the <code>offset</code> or <code>length</code> is
   * less than <code>0</code>, or the sum of the <code>offset</code> and
   * <code>length</code> is greater than the length of the <code>signature</code>
   * byte array.
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
   * Updates the data to be signed or verified by a byte.
   *
   * @param b the byte to use for the update.
   * @throws SignatureException if this signature object is not initialized
   * properly.
   */
  public final void update(byte b) throws SignatureException
  {
    if (state != UNINITIALIZED)
      engineUpdate(b);
    else
      throw new SignatureException();
  }

  /**
   * Updates the data to be signed or verified, using the specified array of
   * bytes.
   *
   * @param data the byte array to use for the update.
   * @throws SignatureException if this signature object is not initialized
   * properly.
   */
  public final void update(byte[]data) throws SignatureException
  {
    if (state != UNINITIALIZED)
      engineUpdate(data, 0, data.length);
    else
      throw new SignatureException();
  }

  /**
   * Updates the data to be signed or verified, using the specified array of
   * bytes, starting at the specified offset.
   *
   * @param data the array of bytes.
   * @param off the offset to start from in the array of bytes.
   * @param len the number of bytes to use, starting at offset.
   * @throws SignatureException if this signature object is not initialized
   * properly.
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
   * Returns the name of the algorithm for this signature object.
   *
   * @return the name of the algorithm for this signature object.
   */
  public final String getAlgorithm()
  {
    return algorithm;
  }

  /**
   * Returns a string representation of this signature object, providing
   * information that includes the state of the object and the name of the
   * algorithm used.
   *
   * @return a string representation of this signature object.
   */
  public String toString()
  {
    return (algorithm + " Signature");
  }

  /**
   * Sets the specified algorithm parameter to the specified value. This method
   * supplies a general-purpose mechanism through which it is possible to set
   * the various parameters of this object. A parameter may be any settable
   * parameter for the algorithm, such as a parameter size, or a source of
   * random bits for signature generation (if appropriate), or an indication of
   * whether or not to perform a specific but optional computation. A uniform
   * algorithm-specific naming scheme for each parameter is desirable but left
   * unspecified at this time.
   *
   * @param param the string identifier of the parameter.
   * @param value the parameter value.
   * @throws InvalidParameterException if param is an invalid parameter for this
   * signature algorithm engine, the parameter is already set and cannot be set
   * again, a security exception occurs, and so on.
   * @see #getParameter(String)
   * @deprecated Use setParameter(AlgorithmParameterSpec).
   */
  public final void setParameter(String param, Object value)
    throws InvalidParameterException
  {
    engineSetParameter(param, value);
  }

  /**
   * Initializes this signature engine with the specified parameter set.
   *
   * @param params the parameters.
   * @throws InvalidAlgorithmParameterException if the given parameters are
   * inappropriate for this signature engine.
   * @see #getParameters()
   */
  public final void setParameter(AlgorithmParameterSpec params)
    throws InvalidAlgorithmParameterException
  {
    engineSetParameter(params);
  }

  /**
   * <p>Returns the parameters used with this signature object.</p>
   *
   * <p>The returned parameters may be the same that were used to initialize
   * this signature, or may contain a combination of default and randomly
   * generated parameter values used by the underlying signature implementation
   * if this signature requires algorithm parameters but was not initialized
   * with any.
   *
   * @return the parameters used with this signature, or <code>null</code> if
   * this signature does not use any parameters.
   * @see #setParameter(AlgorithmParameterSpec)
   */
  public final AlgorithmParameters getParameters()
  {
    return engineGetParameters();
  }

  /**
   * Gets the value of the specified algorithm parameter. This method supplies
   * a general-purpose mechanism through which it is possible to get the various
   * parameters of this object. A parameter may be any settable parameter for
   * the algorithm, such as a parameter size, or a source of random bits for
   * signature generation (if appropriate), or an indication of whether or not
   * to perform a specific but optional computation. A uniform
   * algorithm-specific naming scheme for each parameter is desirable but left
   * unspecified at this time.
   *
   * @param param the string name of the parameter.
   * @return the object that represents the parameter value, or null if there
   * is none.
   * @throws InvalidParameterException if param is an invalid parameter for this
   * engine, or another exception occurs while trying to get this parameter.
   * @see #setParameter(String, Object)
   * @deprecated
   */
  public final Object getParameter(String param)
    throws InvalidParameterException
  {
    return engineGetParameter(param);
  }

  /**
   * Returns a clone if the implementation is cloneable.
   *
   * @return a clone if the implementation is cloneable.
   * @throws CloneNotSupportedException if this is called on an implementation
   * that does not support {@link Cloneable}.
   */
  public Object clone() throws CloneNotSupportedException
  {
    return super.clone();
  }
}
