/* ExemptionMechanismSpi.java -- Exemption mechanism service provider interface.
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
import java.security.spec.AlgorithmParameterSpec;

/**
 * The <i>Service Provider Interface</i> (<b>SPI</b>) for the {@link
 * ExemptionMechanism} class.
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 */
public abstract class ExemptionMechanismSpi
{

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Create a new exemption mechanism SPI.
   */
  public ExemptionMechanismSpi()
  {
  }

  // Abstract instance methods.
  // ------------------------------------------------------------------------

  /**
   * Return a key blob for the key that this mechanism was initialized
   * with.
   *
   * @return The key blob.
   * @throws javax.crypto.ExemptionMechanismException If generating the
   *         blob fails.
   */
  protected abstract byte[] engineGenExemptionBlob()
    throws ExemptionMechanismException;

  /**
   * Generate a key blob for the key that this mechanism was initialized
   * with, storing it into the given byte array.
   *
   * @param output       The destination for the key blob.
   * @param outputOffset The index in the output array to start.
   * @return The size of the key blob.
   * @throws javax.crypto.ExemptionMechanismException If generating the
   *         blob fails.
   * @throws javax.crypto.ShortBufferException If the output array is
   *         not large enough for the key blob.
   */
  protected abstract int engineGenExemptionBlob(byte[] output, int outputOffset)
    throws ExemptionMechanismException, ShortBufferException;

  /**
   * Get the size of the output blob given an input key size. The actual
   * blob may be shorter than the value returned by this method. Both
   * values are in bytes.
   *
   * @param inputLength The input size.
   * @return The output size.
   */
  protected abstract int engineGetOutputSize(int inputLength);

  /**
   * Initialize this mechanism with a key.
   *
   * @param key The key.
   * @throws javax.crypto.ExemptionMechanismException If generating the
   *         blob fails.
   * @throws java.security.InvalidKeyException If the supplied key
   *         cannot be used.
   */
  protected abstract void engineInit(Key key)
    throws ExemptionMechanismException, InvalidKeyException;

  /**
   * Initialize this mechanism with a key and parameters.
   *
   * @param key    The key.
   * @param params The parameters.
   * @throws javax.crypto.ExemptionMechanismException If generating the
   *         blob fails.
   * @throws java.security.InvalidAlgorithmParameterExceptin If the
   *         supplied parameters are inappropriate.
   * @throws java.security.InvalidKeyException If the supplied key
   *         cannot be used.
   */
  protected abstract void engineInit(Key key, AlgorithmParameters params)
    throws ExemptionMechanismException, InvalidAlgorithmParameterException,
           InvalidKeyException;

  /**
   * Initialize this mechanism with a key and parameters.
   *
   * @param key    The key.
   * @param params The parameters.
   * @throws javax.crypto.ExemptionMechanismException If generating the
   *         blob fails.
   * @throws java.security.InvalidAlgorithmParameterExceptin If the
   *         supplied parameters are inappropriate.
   * @throws java.security.InvalidKeyException If the supplied key
   *         cannot be used.
   */
  protected abstract void engineInit(Key key, AlgorithmParameterSpec params)
    throws ExemptionMechanismException, InvalidAlgorithmParameterException,
           InvalidKeyException;
}
