/* SignatureSpi.java --- Signature Service Provider Interface
   Copyright (C) 1999, 2003, Free Software Foundation, Inc.

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

import java.security.spec.AlgorithmParameterSpec;

/**
 * <p>This class defines the <i>Service Provider Interface (SPI)</i> for the
 * {@link Signature} class, which is used to provide the functionality of a
 * digital signature algorithm. Digital signatures are used for authentication
 * and integrity assurance of digital data.</p>
 *
 * <p>All the abstract methods in this class must be implemented by each
 * cryptographic service provider who wishes to supply the implementation of a
 * particular signature algorithm.
 *
 * @author Mark Benvenuto <ivymccough@worldnet.att.net>
 * @since 1.2
 * @see Signature
 */
public abstract class SignatureSpi
{
  /** Application-specified source of randomness. */
  protected SecureRandom appRandom;

  public SignatureSpi()
  {
    appRandom = null;
  }

  /**
   * Initializes this signature object with the specified public key for
   * verification operations.
   *
   * @param publicKey the public key of the identity whose signature is going
   * to be verified.
   * @throws InvalidKeyException if the key is improperly encoded, parameters
   * are missing, and so on.
   */
  protected abstract void engineInitVerify(PublicKey publicKey)
    throws InvalidKeyException;

  /**
   * Initializes this signature object with the specified private key for
   * signing operations.
   *
   * @param privateKey the private key of the identity whose signature will be
   * generated.
   * @throws InvalidKeyException if the key is improperly encoded, parameters
   * are missing, and so on.
   */
  protected abstract void engineInitSign(PrivateKey privateKey)
    throws InvalidKeyException;

  /**
   * <p>Initializes this signature object with the specified private key and
   * source of randomness for signing operations.</p>
   *
   * <p>This concrete method has been added to this previously-defined abstract
   * class. (For backwards compatibility, it cannot be abstract.)</p>
   *
   * @param privateKey the private key of the identity whose signature will be
   * generated.
   * @param random the source of randomness.
   * @throws InvalidKeyException if the key is improperly encoded, parameters
   * are missing, and so on.
   * @since 1.2
   */
  protected void engineInitSign(PrivateKey privateKey, SecureRandom random)
    throws InvalidKeyException
  {
    appRandom = random;
    engineInitSign(privateKey);
  }

  /**
   * Updates the data to be signed or verified using the specified byte.
   *
   * @param b the byte to use for the update.
   * @throws SignatureException if the engine is not initialized properly.
   */
  protected abstract void engineUpdate(byte b) throws SignatureException;

  /**
   * Updates the data to be signed or verified, using the specified array of
   * bytes, starting at the specified offset.
   *
   * @param b the array of bytes.
   * @param off the offset to start from in the array of bytes.
   * @param len the number of bytes to use, starting at offset.
   * @throws SignatureException if the engine is not initialized properly.
   */
  protected abstract void engineUpdate(byte[] b, int off, int len)
    throws SignatureException;

  /**
   * Returns the signature bytes of all the data updated so far. The format of
   * the signature depends on the underlying signature scheme.
   *
   * @return the signature bytes of the signing operation's result.
   * @throws SignatureException if the engine is not initialized properly.
   */
  protected abstract byte[] engineSign() throws SignatureException;

  /**
   * <p>Finishes this signature operation and stores the resulting signature
   * bytes in the provided buffer <code>outbuf</code>, starting at <code>offset
   * </code>. The format of the signature depends on the underlying signature
   * scheme.</p>
   *
   * <p>The signature implementation is reset to its initial state (the state it
   * was in after a call to one of the <code>engineInitSign()</code> methods)
   * and can be reused to generate further signatures with the same private key.
   * This method should be abstract, but we leave it concrete for binary
   * compatibility. Knowledgeable providers should override this method.</p>
   *
   * @param outbuf buffer for the signature result.
   * @param offset offset into outbuf where the signature is stored.
   * @param len number of bytes within outbuf allotted for the signature. Both
   * this default implementation and the <b>GNU</b> provider do not return
   * partial digests. If the value of this parameter is less than the actual
   * signature length, this method will throw a {@link SignatureException}. This
   * parameter is ignored if its value is greater than or equal to the actual
   * signature length.
   * @return the number of bytes placed into <code>outbuf</code>.
   * @throws SignatureException if an error occurs or len is less than the
   * actual signature length.
   * @since 1.2
   */
  protected int engineSign(byte[] outbuf, int offset, int len)
    throws SignatureException
  {
    byte tmp[] = engineSign();
    if (tmp.length > len)
      throw new SignatureException("Invalid Length");

    System.arraycopy(outbuf, offset, tmp, 0, tmp.length);
    return tmp.length;
  }

  /**
   * Verifies the passed-in signature.
   *
   * @param sigBytes the signature bytes to be verified.
   * @return <code>true</code> if the signature was verified, <code>false</code>
   * if not.
   * @throws SignatureException if the engine is not initialized properly, or
   * the passed-in signature is improperly encoded or of the wrong type, etc.
   */
  protected abstract boolean engineVerify(byte[] sigBytes)
    throws SignatureException;

  /**
   * <p>Verifies the passed-in <code>signature</code> in the specified array of
   * bytes, starting at the specified <code>offset</code>.</p>
   *
   * <p>Note: Subclasses should overwrite the default implementation.</p>
   *
   * @param sigBytes the signature bytes to be verified.
   * @param offset the offset to start from in the array of bytes.
   * @param length the number of bytes to use, starting at offset.
   * @return <code>true</code> if the signature was verified, <code>false</code>
   * if not.
   * @throws SignatureException if the engine is not initialized properly, or
   * the passed-in <code>signature</code> is improperly encoded or of the wrong
   * type, etc.
   */
  protected boolean engineVerify(byte[] sigBytes, int offset, int length)
    throws SignatureException
  {
    byte[] tmp = new byte[length];
    System.arraycopy(sigBytes, offset, tmp, 0, length);
    return engineVerify(tmp);
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
   * @throws InvalidParameterException if <code>param</code> is an invalid
   * parameter for this signature algorithm engine, the parameter is already set
   * and cannot be set again, a security exception occurs, and so on.
   * @deprecated Replaced by engineSetParameter(AlgorithmParameterSpec).
   */
  protected abstract void engineSetParameter(String param, Object value)
    throws InvalidParameterException;

  /**
   * This method is overridden by providers to initialize this signature engine
   * with the specified parameter set.
   *
   * @param params the parameters.
   * @throws UnsupportedOperationException if this method is not overridden by
   * a provider.
   * @throws InvalidAlgorithmParameterException if this method is overridden by
   * a provider and the the given parameters are inappropriate for this
   * signature engine.
   */
  protected void engineSetParameter(AlgorithmParameterSpec params)
    throws InvalidAlgorithmParameterException
  {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>This method is overridden by providers to return the parameters used
   * with this signature engine, or <code>null</code> if this signature engine
   * does not use any parameters.</p>
   *
   * <p>The returned parameters may be the same that were used to initialize
   * this signature engine, or may contain a combination of default and randomly
   * generated parameter values used by the underlying signature implementation
   * if this signature engine requires algorithm parameters but was not
   * initialized with any.</p>
   *
   * @return the parameters used with this signature engine, or <code>null</code>
   * if this signature engine does not use any parameters.
   * @throws UnsupportedOperationException if this method is not overridden by
   * a provider.
   */
  protected AlgorithmParameters engineGetParameters()
  {
    throw new UnsupportedOperationException();    
  }

  /**
   * Gets the value of the specified algorithm parameter. This method supplies
   * a general-purpose mechanism through which it is possible to get the various
   * parameters of this object. A parameter may be any settable parameter for
   * the algorithm, such as a parameter size, or a source of random bits for
   * signature generation (if appropriate), or an indication of whether or not
   * to perform a specific but optional computation. A uniform algorithm-specific
   * naming scheme for each parameter is desirable but left unspecified at this
   * time.
   *
   * @param param the string name of the parameter.
   * @return the object that represents the parameter value, or <code>null</code>
   * if there is none.
   * @throws InvalidParameterException if <code>param<?code> is an invalid
   * parameter for this engine, or another exception occurs while trying to get
   * this parameter.
   * @deprecated
   */
  protected abstract Object engineGetParameter(String param)
    throws InvalidParameterException;

  /**
   * Returns a clone if the implementation is cloneable.
   *
   * @return a clone if the implementation is cloneable.
   * @throws CloneNotSupportedException if this is called on an implementation
   * that does not support {@link Cloneable}.
   * @see Cloneable
   */
  public Object clone() throws CloneNotSupportedException
  {
    throw new CloneNotSupportedException();
  }
}
