/* CTR.java -- 
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
import gnu.java.security.util.Sequence;
import gnu.javax.crypto.cipher.IBlockCipher;

import java.util.Arrays;
import java.util.Iterator;

/**
 * The implementation of the Counter Mode.
 * <p>
 * The algorithm steps are formally described as follows:
 * 
 * <pre>
 *     CTR Encryption: O[j] = E(K)(T[j]); for j = 1, 2...n;
 *                     C[j] = P[j] &circ; O[j]; for j = 1, 2...n.
 *     CTR Decryption: O[j] = E(K)(T[j]); for j = 1, 2...n;
 *                     P[j] = C[j] &circ; O[j]; for j = 1, 2...n.
 * </pre>
 * 
 * <p>
 * where <code>P</code> is the plaintext, <code>C</code> is the ciphertext,
 * <code>E(K)</code> is the underlying block cipher encryption function
 * parametrised with the session key <code>K</code>, and <code>T</code> is
 * the <i>Counter</i>.
 * <p>
 * This implementation, uses a standard incrementing function with a step of 1,
 * and an initial value similar to that described in the NIST document.
 * <p>
 * References:
 * <ol>
 * <li><a
 * href="http://csrc.nist.gov/encryption/modes/Recommendation/Modes01.pdf">
 * Recommendation for Block Cipher Modes of Operation Methods and Techniques</a>,
 * Morris Dworkin.</li>
 * </ol>
 */
public class CTR
    extends BaseMode
    implements Cloneable
{
  private int off;
  private byte[] counter, enc;

  /**
   * Trivial package-private constructor for use by the Factory class.
   * 
   * @param underlyingCipher the underlying cipher implementation.
   * @param cipherBlockSize the underlying cipher block size to use.
   */
  CTR(IBlockCipher underlyingCipher, int cipherBlockSize)
  {
    super(Registry.CTR_MODE, underlyingCipher, cipherBlockSize);
  }

  /**
   * Private constructor for cloning purposes.
   * 
   * @param that the instance to clone.
   */
  private CTR(CTR that)
  {
    this((IBlockCipher) that.cipher.clone(), that.cipherBlockSize);
  }

  public Object clone()
  {
    return new CTR(this);
  }

  public void setup()
  {
    if (modeBlockSize > cipherBlockSize)
      throw new IllegalArgumentException("mode size exceeds cipher block size");
    off = 0;
    counter = new byte[cipherBlockSize];
    int i = cipherBlockSize - 1;
    int j = iv.length - 1;
    while (i >= 0 && j >= 0)
      counter[i--] = iv[j--];
    enc = new byte[cipherBlockSize];
    cipher.encryptBlock(counter, 0, enc, 0);
  }

  public void teardown()
  {
    if (counter != null)
      Arrays.fill(counter, (byte) 0);
    if (enc != null)
      Arrays.fill(enc, (byte) 0);
  }

  public void encryptBlock(byte[] in, int i, byte[] out, int o)
  {
    ctr(in, i, out, o);
  }

  public void decryptBlock(byte[] in, int i, byte[] out, int o)
  {
    ctr(in, i, out, o);
  }

  public Iterator blockSizes()
  {
    return new Sequence(1, cipherBlockSize).iterator();
  }

  private void ctr(byte[] in, int inOffset, byte[] out, int outOffset)
  {
    for (int i = 0; i < modeBlockSize; i++)
      {
        out[outOffset++] = (byte)(in[inOffset++] ^ enc[off++]);
        if (off == cipherBlockSize)
          {
            int j;
            for (j = cipherBlockSize - 1; j >= 0; j--)
              {
                counter[j]++;
                if ((counter[j] & 0xFF) != 0)
                  break;
              }
            if (j == 0)
              counter[cipherBlockSize - 1]++;
            off = 0;
            cipher.encryptBlock(counter, 0, enc, 0);
          }
      }
  }
}
