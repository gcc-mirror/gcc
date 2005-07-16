/* CipherSpi.java -- The cipher service provider interface.
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

import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;

/**
 * <p>This class represents the <i>Service Provider Interface</i>
 * (<b>SPI</b>) for cryptographic ciphers.</p>
 *
 * <p>Providers of cryptographic ciphers must subclass this for every
 * cipher they implement, implementing the abstract methods as
 * appropriate, then provide an entry that points to the subclass in
 * their implementation of {@link java.security.Provider}.</p>
 *
 * <p>CipherSpi objects are instantiated along with {@link Cipher}s when
 * the {@link Cipher#getInstance(java.lang.String)} methods are invoked.
 * Particular ciphers are referenced by a <i>transformation</i>, which
 * is a String consisting of the cipher's name or the ciper's name
 * followed by a mode and a padding. Transformations all follow the
 * general form:</p>
 *
 * <ul>
 * <li><i>algorithm</i>, or</li>
 * <li><i>algorithm</i>/<i>mode</i>/<i>padding</i>
 * </ul>
 *
 * <p>Cipher names in the master {@link java.security.Provider} class
 * may be:</p>
 *
 * <ol>
 * <li>The algorithm's name, which uses a pluggable mode and padding:
 * <code>Cipher.<i>algorithm</i></code></li>
 * <li>The algorithm's name and the mode, which uses pluggable padding:
 * <code>Cipher.<i>algorithm</i>/<i>mode</i></code></li>
 * <li>The algorithm's name and the padding, which uses a pluggable
 * mode: <code>Cipher.<i>algorithm</i>//<i>padding</i></code></li>
 * <li>The algorihtm's name, the mode, and the padding:
 * <code>Cipher.<i>algorithm</i>/<i>mode</i>/<i>padding</i></code></li>
 * </ol>
 *
 * <p>When any {@link Cipher#getInstance(java.lang.String)} method is
 * invoked, the following happens if the transformation is simply
 * <i>algorithm</i>:</p>
 *
 * <ol>
 * <li>If the provider defines a <code>CipherSpi</code> implementation
 * for "<i>algorithm</i>", return it. Otherwise throw a {@link
 * java.security.NoSuchAlgorithmException}.</li>
 * </ol>
 *
 * <p>If the transformation is of the form
 * <i>algorithm</i>/<i>mode</i>/<i>padding</i>:</p>
 *
 * <ol>
 * <li>If the provider defines a <code>CipherSpi</code> subclass for
 * "<i>algorithm</i>/<i>mode</i>/<i>padding</i>", return it. Otherwise
 * go to step 2.</li>
 *
 * <li>If the provider defines a <code>CipherSpi</code> subclass for
 * "<i>algorithm</i>/<i>mode</i>", instatiate it, call {@link
 * #engineSetPadding(java.lang.String)} for the padding name, and return
 * it. Otherwise go to step 3.</li>
 *
 * <li>If the provider defines a <code>CipherSpi</code> subclass for
 * "<i>algorithm</i>//<i>padding</i>", instatiate it, call {@link
 * #engineSetMode(java.lang.String)} for the mode name, and return
 * it. Otherwise go to step 4.</li>
 *
 * <li>If the provider defines a <code>CipherSpi</code> subclass for
 * "<i>algorithm</i>", instatiate it, call {@link
 * #engineSetMode(java.lang.String)} for the mode name, call {@link
 * #engineSetPadding(java.lang.String)} for the padding name, and return
 * it. Otherwise throw a {@link java.security.NoSuchAlgorithmException}.</li>
 * </ol>
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 */
public abstract class CipherSpi
{

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new CipherSpi.
   */
  public CipherSpi()
  {
  }

  // Abstract methods to be implemented by providers.
  // ------------------------------------------------------------------------

  /**
   * Finishes a multi-part transformation or transforms a portion of a
   * byte array, and returns the transformed bytes.
   *
   * @param input       The input bytes.
   * @param inputOffset The index in the input at which to start.
   * @param inputLength The number of bytes to transform.
   * @return The transformed bytes in a new array.
   * @throws javax.crypto.IllegalBlockSizeException If this instance has
   *         no padding and the input size is not a multiple of the
   *         block size.
   * @throws javax.crypto.BadPaddingException If this instance is being
   *         used for decryption and the padding is not appropriate for
   *         this instance's padding scheme.
   */
  protected abstract byte[]
  engineDoFinal(byte[] input, int inputOffset, int inputLength)
  throws IllegalBlockSizeException, BadPaddingException;

  /**
   * Finishes a multi-part transformation or transforms a portion of a
   * byte array, and stores the transformed bytes in the supplied array.
   *
   * @param input        The input bytes.
   * @param inputOffset  The index in the input at which to start.
   * @param inputLength  The number of bytes to transform.
   * @param output       The output byte array.
   * @param outputOffset The index in the output array at which to start.
   * @return The number of transformed bytes stored in the output array.
   * @throws javax.crypto.IllegalBlockSizeException If this instance has
   *         no padding and the input size is not a multiple of the
   *         block size.
   * @throws javax.crypto.BadPaddingException If this instance is being
   *         used for decryption and the padding is not appropriate for
   *         this instance's padding scheme.
   * @throws javax.crypto.ShortBufferException If there is not enough
   *         space in the output array for the transformed bytes.
   */
  protected abstract int
  engineDoFinal(byte[] input, int inputOffset, int inputLength,
                byte[] output, int outputOffset)
  throws IllegalBlockSizeException, BadPaddingException, ShortBufferException;

  /**
   * Returns the block size of the underlying cipher.
   *
   * @return The block size.
   */
  protected abstract int engineGetBlockSize();

  /**
   * Returns the initializaiton vector this cipher was initialized with,
   * if any.
   *
   * @return The IV, or null if this cipher uses no IV or if this
   *         instance has not been initialized yet.
   */
  protected abstract byte[] engineGetIV();

  /**
   * <p>Return the length of the given key in bits.</p>
   *
   * <p>For compatibility this method is not declared
   * <code>abstract</code>, and the default implementation will throw an
   * {@link java.lang.UnsupportedOperationException}. Concrete
   * subclasses should override this method to return the correct
   * value.</p>
   *
   * @param key The key to get the size for.
   * @return The size of the key, in bits.
   * @throws java.security.InvalidKeyException If the key's length
   *         cannot be determined by this implementation.
   */
  protected int engineGetKeySize(Key key) throws InvalidKeyException
  {
    throw new UnsupportedOperationException();
  }

  /**
   * <p>Returns the size, in bytes, an output buffer must be for a call
   * to {@link #engineUpdate(byte[],int,int,byte[],int)} or {@link
   * #engineDoFinal(byte[],int,int,byte[],int)} to succeed.</p>
   *
   * <p>The actual output length may be smaller than the value returned
   * by this method, as it considers the padding length as well. The
   * length considered is the argument plus the length of any buffered,
   * unprocessed bytes.</p>
   *
   * @param inputLength The input length, in bytes.
   * @return The size an output buffer must be.
   */
  protected abstract int engineGetOutputSize(int inputLength);

  /**
   * Returns the parameters that this cipher is using. This may be the
   * parameters used to initialize this cipher, or it may be parameters
   * that have been initialized with random values.
   *
   * @return This cipher's parameters, or <code>null</code> if this
   *         cipher does not use parameters.
   */
  protected abstract AlgorithmParameters engineGetParameters();

  /**
   * Initializes this cipher with an operation mode, key, and source of
   * randomness. If this cipher requires any other initializing data,
   * for example an initialization vector, then it should generate it
   * from the provided source of randomness.
   *
   * @param opmode The operation mode, one of {@link
   *        Cipher#DECRYPT_MODE}, {@link Cipher#ENCRYPT_MODE}, {@link
   *        Cipher#UNWRAP_MODE}, or {@link Cipher#WRAP_MODE}.
   * @param key    The key to initialize this cipher with.
   * @param random The source of random bytes to use.
   * @throws java.security.InvalidKeyException If the given key is not
   *         acceptable for this implementation.
   */
  protected abstract void engineInit(int opmode, Key key, SecureRandom random)
  throws InvalidKeyException;

  /**
   * Initializes this cipher with an operation mode, key, parameters,
   * and source of randomness. If this cipher requires any other
   * initializing data, for example an initialization vector, then it should
   * generate it from the provided source of randomness.
   *
   * @param opmode The operation mode, one of {@link
   *        Cipher#DECRYPT_MODE}, {@link Cipher#ENCRYPT_MODE}, {@link
   *        Cipher#UNWRAP_MODE}, or {@link Cipher#WRAP_MODE}.
   * @param key    The key to initialize this cipher with.
   * @param params The algorithm parameters to initialize with.
   * @param random The source of random bytes to use.
   * @throws java.security.InvalidAlgorithmParameterException If the
   *         given parameters are not appropriate for this
   *         implementation.
   * @throws java.security.InvalidKeyException If the given key is not
   *         acceptable for this implementation.
   */
  protected abstract void
  engineInit(int opmode, Key key, AlgorithmParameters params,
             SecureRandom random)
  throws InvalidAlgorithmParameterException, InvalidKeyException;

  /**
   * Initializes this cipher with an operation mode, key, parameters,
   * and source of randomness. If this cipher requires any other
   * initializing data, for example an initialization vector, then it should
   * generate it from the provided source of randomness.
   *
   * @param opmode The operation mode, one of {@link
   *        Cipher#DECRYPT_MODE}, {@link Cipher#ENCRYPT_MODE}, {@link
   *        Cipher#UNWRAP_MODE}, or {@link Cipher#WRAP_MODE}.
   * @param key    The key to initialize this cipher with.
   * @param params The algorithm parameters to initialize with.
   * @param random The source of random bytes to use.
   * @throws java.security.InvalidAlgorithmParameterException If the
   *         given parameters are not appropriate for this
   *         implementation.
   * @throws java.security.InvalidKeyException If the given key is not
   *         acceptable for this implementation.
   */
  protected abstract void
  engineInit(int opmode, Key key, AlgorithmParameterSpec params,
             SecureRandom random)
  throws InvalidAlgorithmParameterException, InvalidKeyException;

  /**
   * Set the mode in which this cipher is to run.
   *
   * @param mode The name of the mode to use.
   * @throws java.security.NoSuchAlgorithmException If the mode is
   *         not supported by this cipher's provider.
   */
  protected abstract void engineSetMode(String mode)
  throws NoSuchAlgorithmException;

  /**
   * Set the method with which the input is to be padded.
   *
   * @param padding The name of the padding to use.
   * @throws javax.crypto.NoSuchPaddingException If the padding is not
   *         supported by this cipher's provider.
   */
  protected abstract void engineSetPadding(String padding)
  throws NoSuchPaddingException;

  /**
   * <p>Unwraps a previously-wrapped key.</p>
   *
   * <p>For compatibility this method is not declared
   * <code>abstract</code>, and the default implementation will throw an
   * {@link java.lang.UnsupportedOperationException}.</p>
   *
   * @param wrappedKey          The wrapped key.
   * @param wrappedKeyAlgorithm The name of the algorithm used to wrap
   *                            this key.
   * @param wrappedKeyType      The type of wrapped key; one of
   *                            {@link Cipher#PRIVATE_KEY},
   *                            {@link Cipher#PUBLIC_KEY}, or
   *                            {@link Cipher#SECRET_KEY}.
   * @return The unwrapped key.
   * @throws java.security.InvalidKeyException If the key cannot be
   *         unwrapped, or if <code>wrappedKeyType</code> is an
   *         inappropriate type for the unwrapped key.
   * @throws java.security.NoSuchAlgorithmException If the
   *         <code>wrappedKeyAlgorithm</code> is unknown.
   */
  protected Key engineUnwrap(byte[] wrappedKey, String wrappedKeyAlgorithm,
                             int wrappedKeyType)
  throws InvalidKeyException, NoSuchAlgorithmException
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Continue with a multi-part transformation, returning a new array of
   * the transformed bytes.
   *
   * @param input       The next input bytes.
   * @param inputOffset The index in the input array from which to start.
   * @param inputLength The number of bytes to input.
   * @return The transformed bytes.
   */
  protected abstract byte[]
  engineUpdate(byte[] input, int inputOffset, int inputLength);

  /**
   * Continue with a multi-part transformation, storing the transformed
   * bytes into the specified array.
   *
   * @param input        The next input bytes.
   * @param inputOffset  The index in the input from which to start.
   * @param inputLength  The number of bytes to input.
   * @param output       The output buffer.
   * @param outputOffset The index in the output array from which to start.
   * @return The transformed bytes.
   * @throws javax.crypto.ShortBufferException If there is not enough
   *         space in the output array to store the transformed bytes.
   */
  protected abstract int
  engineUpdate(byte[] input, int inputOffset, int inputLength,
               byte[] output, int outputOffset)
  throws ShortBufferException;

  /**
   * <p>Wrap a key.</p>
   *
   * <p>For compatibility this method is not declared
   * <code>abstract</code>, and the default implementation will throw an
   * {@link java.lang.UnsupportedOperationException}.</p>
   *
   * @param key The key to wrap.
   * @return The wrapped key.
   * @throws java.security.InvalidKeyException If the key cannot be
   *         wrapped.
   */
  protected byte[] engineWrap(Key key) throws InvalidKeyException, IllegalBlockSizeException
  {
    throw new UnsupportedOperationException();
  }
}
