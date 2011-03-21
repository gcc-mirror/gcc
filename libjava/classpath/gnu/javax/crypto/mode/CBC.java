/* CBC.java --
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
 * The Cipher Block Chaining mode. This mode introduces feedback into the cipher
 * by XORing the previous ciphertext block with the plaintext block before
 * encipherment. That is, encrypting looks like this:
 *
 * <pre>
 *  C<sub>i</sub> = E<sub>K</sub>(P<sub>i</sub>&circ; C<sub>i-1</sub>)
 * </pre>
 * <p>
 * Similarly, decrypting is:
 * <pre>
 *  P<sub>i</sub> = C<sub>i-1</sub> &circ; D<sub>K</sub>(C<sub>i</sub>)
 * </pre>
 */
public class CBC
    extends BaseMode
    implements Cloneable
{
  /** The last (de|en)crypted block */
  private byte[] lastBlock;
  /** An intermediate buffer. */
  private byte[] scratch;

  /**
   * Package-private constructor for the factory class.
   *
   * @param underlyingCipher The cipher implementation.
   * @param cipherBlockSize The cipher's block size.
   */
  CBC(IBlockCipher underlyingCipher, int cipherBlockSize)
  {
    super(Registry.CBC_MODE, underlyingCipher, cipherBlockSize);
  }

  /** Our constructor for cloning. */
  private CBC(CBC that)
  {
    this((IBlockCipher) that.cipher.clone(), that.cipherBlockSize);
  }

  public Object clone()
  {
    return new CBC(this);
  }

  public void setup()
  {
    if (modeBlockSize != cipherBlockSize)
      throw new IllegalArgumentException();
    scratch = new byte[cipherBlockSize];
    lastBlock = new byte[cipherBlockSize];
    // lastBlock gets initialized to the initialization vector.
    for (int i = 0; i < lastBlock.length && i < iv.length; i++)
      lastBlock[i] = iv[i];
  }

  public void teardown()
  {
    lastBlock = null;
    scratch = null;
  }

  public void encryptBlock(byte[] in, int i, byte[] out, int o)
  {
    for (int k = 0; k < scratch.length; k++)
      scratch[k] = (byte)(lastBlock[k] ^ in[k + i]);
    cipher.encryptBlock(scratch, 0, out, o);
    System.arraycopy(out, o, lastBlock, 0, cipherBlockSize);
  }

  public void decryptBlock(byte[] in, int i, byte[] out, int o)
  {
    byte[] buf = new byte[cipherBlockSize];
    System.arraycopy(in, i, buf, 0, cipherBlockSize);
    cipher.decryptBlock(in, i, scratch, 0);
    for (int k = 0; k < scratch.length; k++)
      out[o + k] = (byte)(lastBlock[k] ^ scratch[k]);
    System.arraycopy(buf, 0, lastBlock, 0, cipherBlockSize);
  }
}
