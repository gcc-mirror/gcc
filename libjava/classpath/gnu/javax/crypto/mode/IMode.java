/* IMode.java -- 
   Copyright (C) 2001, 2002, 2006 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.crypto.mode;

import gnu.javax.crypto.cipher.IBlockCipher;

/**
 * The basic visible methods of any block cipher mode.
 * <p>
 * Block ciphers encrypt plaintext in fixed size n-bit blocks. For messages
 * larger than n bits, the simplest approach is to segment the message into
 * n-bit blocks and process (encrypt and/or decrypt) each one separately
 * (Electronic Codebook or ECB mode). But this approach has disadvantages in
 * most applications. The block cipher modes of operations are one way of
 * working around those disadvantages.
 * <p>
 * A <i>Mode</i> always employs an underlying block cipher for processing its
 * input. For all intents and purposes, a <i>Mode</i> appears to behave as any
 * other block cipher with the following differences:
 * <ul>
 * <li>Depending on the specifications of the mode, the block size may be
 * different that that of the underlying cipher.</li>
 * <li>While some modes of operations allow operations on block sizes that can
 * be 1-bit long, this library will only deal with sizes that are multiple of 8
 * bits. This is because the <tt>byte</tt> is the smallest, easy to handle,
 * primitive type in Java.</li>
 * <li>Some modes need an <i>Initialisation Vector</i> (IV) to be properly
 * initialised.</li>
 * </ul>
 * <p>
 * Possible additional initialisation values for an instance of that type are:
 * <ul>
 * <li>The block size in which to operate this mode instance. This value is
 * <b>optional</b>, if unspecified, the underlying block cipher's configured
 * block size shall be used.</li>
 * <li>Whether this mode will be used for encryption or decryption. This value
 * is <b>mandatory</b> and should be included in the initialisation parameters.
 * If it isn't, a {@link java.lang.IllegalStateException} will be thrown if any
 * method, other than <code>reset()</code> is invoked on the instance.</li>
 * <li>The byte array containing the <i>initialisation vector</i>, if required
 * by this type of mode.</li>
 * </ul>
 */
public interface IMode
    extends IBlockCipher
{
  /**
   * Property name of the state in which to operate this mode. The value
   * associated to this property name is taken to be an {@link Integer} which
   * value is either <code>ENCRYPTION</code> or <code>DECRYPTION</code>.
   */
  String STATE = "gnu.crypto.mode.state";
  /**
   * Property name of the block size in which to operate this mode. The value
   * associated with this property name is taken to be an {@link Integer}. If
   * it is not specified, the value of the block size of the underlying block
   * cipher, used to construct the mode instance, shall be used.
   */
  String MODE_BLOCK_SIZE = "gnu.crypto.mode.block.size";
  /**
   * Property name of the initialisation vector to use, if required, with this
   * instance. The value associated with this property name is taken to be a
   * byte array. If the concrete instance needs such a parameter, and it has not
   * been specified as part of the initialissation parameters, an all-zero byte
   * array of the appropriate size shall be used.
   */
  String IV = "gnu.crypto.mode.iv";
  /** Constant indicating the instance is being used for <i>encryption</i>. */
  int ENCRYPTION = 1;
  /** Constant indicating the instance is being used for <i>decryption</i>. */
  int DECRYPTION = 2;

  /**
   * A convenience method. Effectively invokes the <code>encryptBlock()</code>
   * or <code>decryptBlock()</code> method depending on the operational state
   * of the instance.
   * 
   * @param in the plaintext.
   * @param inOffset index of <code>in</code> from which to start considering
   *          data.
   * @param out the ciphertext.
   * @param outOffset index of <code>out</code> from which to store result.
   * @exception IllegalStateException if the instance is not initialised.
   */
  void update(byte[] in, int inOffset, byte[] out, int outOffset)
      throws IllegalStateException;
}
