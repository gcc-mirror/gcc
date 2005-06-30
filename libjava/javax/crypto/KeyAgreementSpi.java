/* KeyAgreementSpi.java -- The key agreement service provider interface.
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

import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;

/**
 * This is the <i>Service Provider Interface</i> (<b>SPI</b>) for the
 * {@link javax.crypto.KeyAgreement} class.
 *
 * <p>Providers wishing to implement a key agreement algorithm must
 * subclass this and provide an appropriate implementation for all the
 * abstract methods below, and provide an appropriate entry in the
 * master {@link java.security.Provider} class (the service name for key
 * agreement algorithms is <code>"KeyAgreement"</code>).
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 * @see KeyAgreement
 * @see SecretKey
 */
public abstract class KeyAgreementSpi
{

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Create a new KeyAgreementSpi instance.
   */
  public KeyAgreementSpi()
  {
  }

  // Abstract instance methods.
  // ------------------------------------------------------------------------

  /**
   * Do a phase in the key agreement.
   *
   * @param key The key to use for this phase.
   * @param lastPhase <code>true</code> if this call should be the last
   *        phase.
   * @return The intermediate result, or <code>null</code> if there is
   *         no intermediate result.
   * @throws java.lang.IllegalStateException If this instance has not
   *         been initialized.
   * @throws java.security.InvalidKeyException If the supplied key is
   *         not appropriate.
   */
  protected abstract Key engineDoPhase(Key key, boolean lastPhase)
    throws IllegalStateException, InvalidKeyException;

  /**
   * Generate the shared secret in a new byte array.
   *
   * @return The shared secret in a new byte array.
   * @throws java.lang.IllegalStateException If this key agreement is
   *         not ready to generate the secret.
   */
  protected abstract byte[] engineGenerateSecret()
    throws IllegalStateException;

  /**
   * Generate the shared secret, storing it into the specified array.
   *
   * @param sharedSecret The byte array in which to store the secret.
   * @param offset       The offset into the byte array to start.
   * @return The size of the shared secret.
   * @throws java.lang.IllegalStateException If this key agreement is
   *         not ready to generate the secret.
   * @throws javax.crypto.ShortBufferException If there is not enough
   *         space in the supplied array for the shared secret.
   */
  protected abstract int engineGenerateSecret(byte[] sharedSecret, int offset)
    throws IllegalStateException, ShortBufferException;

  /**
   * Generate the shared secret and return it as a {@link SecretKey}.
   *
   * @param algorithm The algorithm with which to generate the secret key.
   * @return The shared secret as a secret key.
   * @throws java.lang.IllegalStateException If this key agreement is
   *         not ready to generate the secret.
   * @throws java.security.InvalidKeyException If the shared secret
   *         cannot be made into a {@link SecretKey}.
   * @throws java.security.NoSuchAlgorithmException If
   *         <code>algorithm</code> cannot be found.
   */
  protected abstract SecretKey engineGenerateSecret(String algorithm)
    throws IllegalStateException, InvalidKeyException, NoSuchAlgorithmException;

  /**
   * Initialize this key agreement with a key, parameters, and source of
   * randomness.
   *
   * @param key    The key to initialize with, usually a private key.
   * @param params The parameters to initialize with.
   * @param random The source of randomness to use.
   * @throws java.security.InvalidAlgorithmParameterException If the
   *         supplied parameters are inappropriate.
   * @throws java.security.InvalidKeyException If the supplied key is
   *         inappropriate.
   */
  protected abstract void engineInit(Key key, AlgorithmParameterSpec params,
                                     SecureRandom random)
    throws InvalidAlgorithmParameterException, InvalidKeyException;

  /**
   * Initialize this key agreement with a key and source of randomness.
   *
   * @param key    The key to initialize with, usually a private key.
   * @param random The source of randomness to use.
   * @throws java.security.InvalidKeyException If the supplied key is
   *         inappropriate.
   */
  protected abstract void engineInit(Key key, SecureRandom random)
    throws InvalidKeyException;
}
