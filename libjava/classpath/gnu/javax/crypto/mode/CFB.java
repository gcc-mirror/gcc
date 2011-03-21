/* CFB.java --
   Copyright (C) 2002, 2006 Free Software Foundation, Inc.

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

import gnu.java.security.Registry;
import gnu.javax.crypto.cipher.IBlockCipher;

/**
 * The cipher feedback mode. CFB mode is a stream mode that operates on <i>s</i>
 * bit blocks, where 1 &lt;= <i>s</i> &lt;= <i>b</i>, if <i>b</i> is the
 * underlying cipher's block size. Encryption is:
 * <pre>
 *  I[1] = IV
 *  I[j] = LSB(b-s, I[j-1]) | C[j-1]   for j = 2...n
 *  O[j] = CIPH(K, I[j])               for j = 1,2...n
 *  C[j] = P[j] &circ; MSB(s, O[j])         for j = 1,2...n
 * </pre>
 * <p>
 * And decryption is:
 * <pre>
 *  I[1] = IV
 *  I[j] = LSB(b-s, I[j-1]) | C[j-1]   for j = 2...n
 *  O[j] = CIPH(K, I[j])               for j = 1,2...n
 *  P[j] = C[j] &circ; MSB(s, O[j])         for j = 1,2...n
 * </pre>
 * <p>
 * CFB mode requires an initialization vector, which need not be kept secret.
 * <p>
 * References:
 * <ol>
 * <li>Bruce Schneier, <i>Applied Cryptography: Protocols, Algorithms, and
 * Source Code in C, Second Edition</i>. (1996 John Wiley and Sons) ISBN
 * 0-471-11709-9.</li>
 * <li><a
 * href="http://csrc.nist.gov/encryption/modes/Recommendation/Modes01.pdf">
 * Recommendation for Block Cipher Modes of Operation Methods and Techniques</a>,
 * Morris Dworkin.</li>
 * </ol>
 */
public class CFB
    extends BaseMode
{
  /** The shift register, the input block to the block cipher. */
  private byte[] shiftRegister;
  /** The output block from the block cipher. */
  private byte[] scratch;

  /**
   * Package-private constructor for the factory class.
   *
   * @param underlyingCipher The cipher implementation.
   * @param cipherBlockSize The cipher's block size.
   */
  CFB(IBlockCipher underlyingCipher, int cipherBlockSize)
  {
    super(Registry.CFB_MODE, underlyingCipher, cipherBlockSize);
  }

  /**
   * Cloneing constructor.
   *
   * @param that The instance being cloned.
   */
  private CFB(CFB that)
  {
    this((IBlockCipher) that.cipher.clone(), that.cipherBlockSize);
  }

  public Object clone()
  {
    return new CFB(this);
  }

  public void setup()
  {
    if (modeBlockSize > cipherBlockSize)
      throw new IllegalArgumentException(
          "CFB block size cannot be larger than the cipher block size");
    shiftRegister = new byte[cipherBlockSize];
    scratch = new byte[cipherBlockSize];
    System.arraycopy(iv, 0,
                     shiftRegister, 0,
                     Math.min(iv.length, cipherBlockSize));
  }

  public void teardown()
  {
    if (shiftRegister != null)
      for (int i = 0; i < shiftRegister.length; i++)
        shiftRegister[i] = 0;
    shiftRegister = null;
  }

  public void encryptBlock(byte[] in, int inOffset, byte[] out, int outOffset)
  {
    cipher.encryptBlock(shiftRegister, 0, scratch, 0);
    for (int i = 0; i < modeBlockSize; i++)
      out[outOffset + i] = (byte)(in[inOffset + i] ^ scratch[i]);
    System.arraycopy(shiftRegister, modeBlockSize,
                     shiftRegister, 0,
                     cipherBlockSize - modeBlockSize);
    System.arraycopy(out, outOffset,
                     shiftRegister, cipherBlockSize - modeBlockSize,
                     modeBlockSize);
  }

  public void decryptBlock(byte[] in, int inOffset, byte[] out, int outOffset)
  {
    cipher.encryptBlock(shiftRegister, 0, scratch, 0);
    for (int i = 0; i < modeBlockSize; i++)
      out[outOffset + i] = (byte)(in[inOffset + i] ^ scratch[i]);
    System.arraycopy(shiftRegister, modeBlockSize,
                     shiftRegister, 0,
                     cipherBlockSize - modeBlockSize);
    System.arraycopy(in, inOffset,
                     shiftRegister, cipherBlockSize - modeBlockSize,
                     modeBlockSize);
  }
}
