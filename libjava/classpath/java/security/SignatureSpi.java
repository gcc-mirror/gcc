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

import java.nio.ByteBuffer;
import java.security.spec.AlgorithmParameterSpec;

/**
 * <code>SignatureSpi</code> defines the Service Provider Interface (SPI) for
 * the {@link Signature} class. The signature class provides an interface to a
 * digital signature algorithm. Digital signatures are used for authentication
 * and integrity of data.
 *
 * @author Mark Benvenuto (ivymccough@worldnet.att.net)
 * @since 1.2
 * @see Signature
 */
public abstract class SignatureSpi
{
  /** Source of randomness. */
  protected SecureRandom appRandom;

  /**
   * Creates a new instance of <code>SignatureSpi</code>.
   */
  public SignatureSpi()
  {
    appRandom = null;
  }

  /**
   * Initializes this instance with the public key for verification purposes.
   *
   * @param publicKey
   *          the public key to verify with.
   * @throws InvalidKeyException
   *           if the key is invalid.
   */
  protected abstract void engineInitVerify(PublicKey publicKey)
    throws InvalidKeyException;

  /**
   * Initializes this instance with the private key for signing purposes.
   *
   * @param privateKey
   *          the private key to sign with.
   * @throws InvalidKeyException
   *           if the key is invalid.
   */
  protected abstract void engineInitSign(PrivateKey privateKey)
    throws InvalidKeyException;

  /**
   * Initializes this instance with the private key and source of randomness for
   * signing purposes.
   *
   * <p>This method cannot be abstract for backward compatibility reasons.</p>
   *
   * @param privateKey
   *          the private key to sign with.
   * @param random
   *          the {@link SecureRandom} to use.
   * @throws InvalidKeyException
   *           if the key is invalid.
   * @since 1.2
   */
  protected void engineInitSign(PrivateKey privateKey, SecureRandom random)
    throws InvalidKeyException
  {
    appRandom = random;
    engineInitSign(privateKey);
  }

  /**
   * Updates the data to be signed or verified with the specified byte.
   *
   * @param b
   *          byte to update with.
   * @throws SignatureException
   *           if the engine is not properly initialized.
   */
  protected abstract void engineUpdate(byte b) throws SignatureException;

  /**
   * Updates the data to be signed or verified with the specified bytes.
   *
   * @param b
   *          the array of bytes to use.
   * @param off
   *          the offset to start at in the array.
   * @param len
   *          the number of the bytes to use from the array.
   * @throws SignatureException
   *           if the engine is not properly initialized.
   */
  protected abstract void engineUpdate(byte[] b, int off, int len)
    throws SignatureException;

  /**
   * Update this signature with the {@link java.nio.Buffer#remaining()}
   * bytes of the given buffer.
   *
   * @param input The input buffer.
   * @throws IllegalStateException if the engine is not properly initialized.
   */
  protected void engineUpdate(ByteBuffer input)
  {
    byte[] buf = new byte[4096];
    while (input.hasRemaining())
      {
        int l = Math.min(input.remaining(), buf.length);
        input.get(buf, 0, l);
        try
          {
            engineUpdate(buf, 0, l);
          }
        catch (SignatureException se)
          {
            throw new IllegalStateException(se);
          }
      }
  }

  /**
   * Returns the signature bytes of all the data fed to this instance. The
   * format of the output depends on the underlying signature algorithm.
   *
   * @return the signature bytes.
   * @throws SignatureException
   *           if the engine is not properly initialized.
   */
  protected abstract byte[] engineSign() throws SignatureException;

  /**
   * Generates signature bytes of all the data fed to this instance and stores
   * the result in the designated array. The format of the output depends on
   * the underlying signature algorithm.
   *
   * <p>This method cannot be abstract for backward compatibility reasons.
   * After calling this method, the signature is reset to its initial state and
   * can be used to generate additional signatures.</p>
   *
   * <p><b>IMPLEMENTATION NOTE:</b>: Neither this method nor the GNU provider
   * will return partial digests. If <code>len</code> is less than the
   * signature length, this method will throw a {@link SignatureException}. If
   * it is greater than or equal then it is ignored.</p>
   *
   * @param outbuf
   *          the array of bytes to store the result in.
   * @param offset
   *          the offset to start at in the array.
   * @param len
   *          the number of the bytes to use in the array.
   * @return the real number of bytes used.
   * @throws SignatureException
   *           if the engine is not properly initialized.
   * @since 1.2
   */
  protected int engineSign(byte[] outbuf, int offset, int len)
    throws SignatureException
  {
    byte[] tmp = engineSign();
    if (tmp.length > len)
      throw new SignatureException("Invalid Length");

    System.arraycopy(outbuf, offset, tmp, 0, tmp.length);
    return tmp.length;
  }

  /**
   * Verifies a designated signature.
   *
   * @param sigBytes
   *          the signature bytes to verify.
   * @return <code>true</code> if verified, <code>false</code> otherwise.
   * @throws SignatureException
   *           if the engine is not properly initialized or if it is the wrong
   *           signature.
   */
  protected abstract boolean engineVerify(byte[] sigBytes)
    throws SignatureException;

  /**
   * Convenience method which calls the method with the same name and one
   * argument after copying the designated bytes into a temporary byte array.
   * Subclasses may override this method for performance reasons.
   *
   * @param sigBytes
   *          the array of bytes to use.
   * @param offset
   *          the offset to start from in the array of bytes.
   * @param length
   *          the number of bytes to use, starting at offset.
   * @return <code>true</code> if verified, <code>false</code> otherwise.
   * @throws SignatureException
   *           if the engine is not properly initialized.
   */
  protected boolean engineVerify(byte[] sigBytes, int offset, int length)
    throws SignatureException
  {
    byte[] tmp = new byte[length];
    System.arraycopy(sigBytes, offset, tmp, 0, length);
    return engineVerify(tmp);
  }

  /**
   * Sets the specified algorithm parameter to the specified value.
   *
   * @param param
   *          the parameter name.
   * @param value
   *          the parameter value.
   * @throws InvalidParameterException
   *           if the parameter invalid, the parameter is already set and
   *           cannot be changed, a security exception occured, etc.
   * @deprecated use the other setParameter.
   */
  protected abstract void engineSetParameter(String param, Object value)
    throws InvalidParameterException;

  /**
   * Sets the signature engine with the specified {@link AlgorithmParameterSpec}.
   *
   * <p>This method cannot be abstract for backward compatibility reasons. By
   * default it always throws {@link UnsupportedOperationException} unless
   * overridden.</p>
   *
   * @param params
   *          the parameters.
   * @throws InvalidParameterException
   *           if the parameter is invalid, the parameter is already set and
   *           cannot be changed, a security exception occured, etc.
   */
  protected void engineSetParameter(AlgorithmParameterSpec params)
    throws InvalidAlgorithmParameterException
  {
    throw new UnsupportedOperationException();
  }

  /**
   * The default implementaion of this method always throws a
   * {@link UnsupportedOperationException}. It MUST be overridden by concrete
   * implementations to return the appropriate {@link AlgorithmParameters} for
   * this signature engine (or <code>null</code> when that engine does not use
   * any parameters.
   *
   * @return the parameters used with this signature engine, or
   *         <code>null</code> if it does not use any parameters.
   * @throws UnsupportedOperationException
   *           always.
   */
  protected AlgorithmParameters engineGetParameters()
  {
    throw new UnsupportedOperationException();
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
  protected abstract Object engineGetParameter(String param)
    throws InvalidParameterException;

  /**
   * Returns a clone of this instance.
   *
   * @return a clone of this instance.
   * @throws CloneNotSupportedException
   *           if the implementation does not support cloning.
   */
  public Object clone() throws CloneNotSupportedException
  {
    return super.clone();
  }
}
