/* ECB.java -- 
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

import gnu.java.security.Registry;
import gnu.javax.crypto.cipher.IBlockCipher;

/**
 * The implementation of the Electronic Codebook mode.
 * <p>
 * The Electronic Codebook (ECB) mode is a confidentiality mode that is defined
 * as follows:
 * <ul>
 * <li>ECB Encryption: C<sub>j</sub> = CIPH<sub>K</sub>(P<sub>j</sub>)
 * for j = 1...n</li>
 * <li>ECB Decryption: P<sub>j</sub> = CIPH<sup>-1</sup><sub>K</sub>(C<sub>j</sub>)
 * for j = 1...n</li>
 * </ul>
 * <p>
 * In ECB encryption, the forward cipher function is applied directly, and
 * independently, to each block of the plaintext. The resulting sequence of
 * output blocks is the ciphertext.
 * <p>
 * In ECB decryption, the inverse cipher function is applied directly, and
 * independently, to each block of the ciphertext. The resulting sequence of
 * output blocks is the plaintext.
 * <p>
 * References:
 * <ol>
 * <li><a
 * href="http://csrc.nist.gov/encryption/modes/Recommendation/Modes01.pdf">
 * Recommendation for Block Cipher Modes of Operation Methods and Techniques</a>,
 * Morris Dworkin.</li>
 * </ol>
 */
public class ECB
    extends BaseMode
    implements Cloneable
{
  /**
   * Trivial package-private constructor for use by the Factory class.
   * 
   * @param underlyingCipher the underlying cipher implementation.
   * @param cipherBlockSize the underlying cipher block size to use.
   */
  ECB(IBlockCipher underlyingCipher, int cipherBlockSize)
  {
    super(Registry.ECB_MODE, underlyingCipher, cipherBlockSize);
  }

  /**
   * Private constructor for cloning purposes.
   * 
   * @param that the mode to clone.
   */
  private ECB(ECB that)
  {
    this((IBlockCipher) that.cipher.clone(), that.cipherBlockSize);
  }

  public Object clone()
  {
    return new ECB(this);
  }

  public void setup()
  {
    if (modeBlockSize != cipherBlockSize)
      throw new IllegalArgumentException(IMode.MODE_BLOCK_SIZE);
  }

  public void teardown()
  {
  }

  public void encryptBlock(byte[] in, int i, byte[] out, int o)
  {
    cipher.encryptBlock(in, i, out, o);
  }

  public void decryptBlock(byte[] in, int i, byte[] out, int o)
  {
    cipher.decryptBlock(in, i, out, o);
  }
}
