/* OFB.java -- 
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
 * The Output Feedback (OFB) mode is a confidentiality mode that requires a
 * unique <code>IV</code> for every message that is ever encrypted under the
 * given key. The OFB mode is defined as follows:
 * <ul>
 * <li>OFB Encryption:
 * <ul>
 * <li>I<sub>1</sub> = IV;</li>
 * <li>I<sub>j</sub> = O<sub>j -1</sub> for j = 2...n;</li>
 * <li>O<sub>j</sub> = CIPH<sub>K</sub>(I<sub>j</sub>) for j = 1, 2...n;</li>
 * <li>C<sub>j</sub> = P<sub>j</sub> XOR O<sub>j</sub> for j = 1, 2...n.</li>
 * </ul>
 * </li>
 * <li>OFB Decryption:
 * <ul>
 * <li>I<sub>1</sub> = IV;</li>
 * <li>I<sub>j</sub> = O<sub>j -1</sub> for j = 2...n;</li>
 * <li>O<sub>j</sub> = CIPH<sub>K</sub>(I<sub>j</sub>) for j = 1, 2...n;</li>
 * <li>P<sub>j</sub> = C<sub>j</sub> XOR O<sub>j</sub> for j = 1, 2...n.</li>
 * </ul>
 * </li>
 * </ul>
 * <p>
 * In OFB encryption, the <code>IV</code> is transformed by the forward cipher
 * function to produce the first output block. The first output block is
 * exclusive-ORed with the first plaintext block to produce the first ciphertext
 * block. The first output block is then transformed by the forward cipher
 * function to produce the second output block. The second output block is
 * exclusive-ORed with the second plaintext block to produce the second
 * ciphertext block, and the second output block is transformed by the forward
 * cipher function to produce the third output block. Thus, the successive
 * output blocks are produced from enciphering the previous output blocks, and
 * the output blocks are exclusive-ORed with the corresponding plaintext blocks
 * to produce the ciphertext blocks.
 * <p>
 * In OFB decryption, the <code>IV</code> is transformed by the forward cipher
 * function to produce the first output block. The first output block is
 * exclusive-ORed with the first ciphertext block to recover the first plaintext
 * block. The first output block is then transformed by the forward cipher
 * function to produce the second output block. The second output block is
 * exclusive-ORed with the second ciphertext block to produce the second
 * plaintext block, and the second output block is also transformed by the
 * forward cipher function to produce the third output block. Thus, the
 * successive output blocks are produced from enciphering the previous output
 * blocks, and the output blocks are exclusive-ORed with the corresponding
 * ciphertext blocks to recover the plaintext blocks.
 * <p>
 * In both OFB encryption and OFB decryption, each forward cipher function
 * (except the first) depends on the results of the previous forward cipher
 * function; therefore, multiple forward cipher functions cannot be performed in
 * parallel. However, if the <code>IV</code> is known, the output blocks can
 * be generated prior to the availability of the plaintext or ciphertext data.
 * <p>
 * The OFB mode requires a unique <code>IV</code> for every message that is
 * ever encrypted under the given key. If, contrary to this requirement, the
 * same <code>IV</code> is used for the encryption of more than one message,
 * then the confidentiality of those messages may be compromised. In particular,
 * if a plaintext block of any of these messages is known, say, the j<sup>th</sup>
 * plaintext block, then the j<sup>th</sup> output of the forward cipher
 * function can be determined easily from the j<sup>th</sup> ciphertext block
 * of the message. This information allows the j<sup>th</sup> plaintext block
 * of any other message that is encrypted using the same <code>IV</code> to be
 * easily recovered from the jth ciphertext block of that message.
 * <p>
 * Confidentiality may similarly be compromised if any of the input blocks to
 * the forward cipher function for the encryption of a message is used as the
 * <code>IV</code> for the encryption of another message under the given key.
 * <p>
 * References:
 * <ol>
 * <li><a
 * href="http://csrc.nist.gov/encryption/modes/Recommendation/Modes01.pdf">
 * Recommendation for Block Cipher Modes of Operation Methods and Techniques</a>,
 * Morris Dworkin.</li>
 * </ol>
 */
public class OFB
    extends BaseMode
    implements Cloneable
{
  private byte[] outputBlock;

  /**
   * Trivial package-private constructor for use by the Factory class.
   * 
   * @param underlyingCipher the underlying cipher implementation.
   * @param cipherBlockSize the underlying cipher block size to use.
   */
  OFB(IBlockCipher underlyingCipher, int cipherBlockSize)
  {
    super(Registry.OFB_MODE, underlyingCipher, cipherBlockSize);
  }

  /**
   * Private constructor for cloning purposes.
   * 
   * @param that the mode to clone.
   */
  private OFB(OFB that)
  {
    this((IBlockCipher) that.cipher.clone(), that.cipherBlockSize);
  }

  public Object clone()
  {
    return new OFB(this);
  }

  public void setup()
  {
    if (modeBlockSize != cipherBlockSize)
      throw new IllegalArgumentException(IMode.MODE_BLOCK_SIZE);
    outputBlock = (byte[]) iv.clone();
  }

  public void teardown()
  {
  }

  public void encryptBlock(byte[] in, int i, byte[] out, int o)
  {
    cipher.encryptBlock(outputBlock, 0, outputBlock, 0);
    for (int j = 0; j < cipherBlockSize;)
      out[o++] = (byte)(in[i++] ^ outputBlock[j++]);
  }

  public void decryptBlock(byte[] in, int i, byte[] out, int o)
  {
    this.encryptBlock(in, i, out, o);
  }
}
