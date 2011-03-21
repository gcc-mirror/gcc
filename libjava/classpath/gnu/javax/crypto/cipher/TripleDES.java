/* TripleDES.java --
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


package gnu.javax.crypto.cipher;

import gnu.java.security.Registry;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.security.InvalidKeyException;

/**
 * Triple-DES, 3DES, or DESede is a <i>combined cipher</i> that uses three
 * iterations of the Data Encryption Standard cipher to theoretically improve
 * the security of plain DES, at the cost of speed.
 * <p>
 * Triple-DES runs the DES algorithm three times with one, two or three
 * independent 56-bit (DES) keys. When used with one DES key, the cipher behaves
 * exactly like a (slower) DES.
 * <p>
 * To encrypt:
 * <blockquote><i>C<sub>i</sub> = E<sub>k3</sub> ( E<sub>k2</sub><sup>-1</sup> (
 * E<sub>k1</sub> ( P<sub>i</sub> )))</i>
 * </blockquote>
 * <p>
 * And to decrypt:
 * <blockquote><i>P<sub>i</sub> = E<sub>k1</sub><sup>-1</sup> (
 * E<sub>k2</sub> ( E<sub>k3</sub><sup>-1</sup> ( C<sub>i</sub> )))</i>
 * </blockquote>
 * <p>
 * (The "ede" comes from the encryption operation, which runs
 * Encrypt-Decrypt-Encrypt)
 * <p>
 * References:
 * <ol>
 * <li>Bruce Schneier, <i>Applied Cryptography: Protocols, Algorithms, and
 * Source Code in C, Second Edition</i>. (1996 John Wiley and Sons) ISBN
 * 0-471-11709-9. Page 294--295.</li>
 * </ol>
 */
public class TripleDES
    extends BaseCipher
{
  /** Triple-DES only operates on 64 bit blocks. */
  public static final int BLOCK_SIZE = 8;
  /** By default, Triple-DES uses 168 bits of a parity-adjusted 192 bit key. */
  public static final int KEY_SIZE = 24;
  /** The underlying DES instance. */
  private DES des;

  /**
   * Default 0-arguments constructor.
   */
  public TripleDES()
  {
    super(Registry.TRIPLEDES_CIPHER, BLOCK_SIZE, KEY_SIZE);
    des = new DES();
  }

  /**
   * Convenience method which calls the method with same name and three
   * arguments, passing <code>3</code> as the value of the first parameter.
   *
   * @param kb The key bytes to adjust.
   * @param offset The starting offset into the key bytes.
   */
  public static void adjustParity(byte[] kb, int offset)
  {
    adjustParity(3, kb, offset);
  }

  /**
   * Adjusts, in-situ, the parity of the designated bytes, so they can be used
   * as DES keys for a 3-DES 1-, 2- or 3-key cipher.
   *
   * @param keyCount the number of independent DES keys. Can be either
   *          <code>1</code>, <code>2</code> or <code>3</code>. Any other value
   *          will cause an {@link IllegalArgumentException} to be raised.
   * @param kb the array containing the key bytes to adjust. MUST have at least
   *          <code>8 * keyCount</code> bytes starting at offset position
   *          <code>offset</code>, otherwise an
   *          {@link ArrayIndexOutOfBoundsException} will be raised.
   * @param offset the starting offset into the array.
   * @see DES#adjustParity(byte[],int)
   */
  public static void adjustParity(int keyCount, byte[] kb, int offset)
  {
    if (keyCount < 1 || keyCount > 3)
      throw new IllegalArgumentException("Invalid keyCount value: " + keyCount);
    DES.adjustParity(kb, offset);
    if (keyCount > 1)
      DES.adjustParity(kb, offset + 8);
    if (keyCount > 2)
      DES.adjustParity(kb, offset + 16);
  }

  /**
   * Convenience method which calls the method with same name and three
   * arguments, passing <code>3</code> as the value of the first parameter.
   *
   * @param kb The key bytes to test.
   * @param offset The starting offset into the key bytes.
   * @return <code>true</code> if the bytes in <i>kb</i> starting at
   *         <i>offset</i> are parity adjusted.
   * @see DES#isParityAdjusted(byte[],int)
   * @see #adjustParity(byte[],int)
   */
  public static boolean isParityAdjusted(byte[] kb, int offset)
  {
    return isParityAdjusted(3, kb, offset);
  }

  /**
   * Tests if enough bytes, expected to be used as DES keys for a 3-DES 1-, 2-
   * or 3-key cipher, located in a designated byte array, has already been
   * parity adjusted.
   *
   * @param keyCount the number of independent DES keys. Can be either
   *          <code>1</code>, <code>2</code> or <code>3</code>. Any other value
   *          will cause an {@link IllegalArgumentException} to be raised.
   * @param kb the array containing the key bytes to test. MUST have at least
   *          <code>8 * keyCount</code> bytes starting at offset position
   *          <code>offset</code>, otherwise an
   *          {@link ArrayIndexOutOfBoundsException} will be raised.
   * @param offset the starting offset into the array.
   * @return <code>true</code> if the bytes in <i>kb</i> starting at
   *         <i>offset</i> are parity adjusted.
   * @see DES#isParityAdjusted(byte[],int)
   * @see #adjustParity(int,byte[],int)
   */
  public static boolean isParityAdjusted(int keyCount, byte[] kb, int offset)
  {
    if (keyCount < 1 || keyCount > 3)
      throw new IllegalArgumentException("Invalid keyCount value: " + keyCount);
    boolean result = DES.isParityAdjusted(kb, offset);
    if (keyCount > 1)
      result = result && DES.isParityAdjusted(kb, offset + 8);
    if (keyCount > 2)
      result = result && DES.isParityAdjusted(kb, offset + 16);
    return result;
  }

  public Object clone()
  {
    return new TripleDES();
  }

  public Iterator blockSizes()
  {
    return Collections.singleton(Integer.valueOf(BLOCK_SIZE)).iterator();
  }

  public Iterator keySizes()
  {
    ArrayList al = new ArrayList();
    al.add(Integer.valueOf(8));
    al.add(Integer.valueOf(16));
    al.add(Integer.valueOf(24));
    return Collections.unmodifiableList(al).iterator();
  }

  public Object makeKey(byte[] kb, int bs) throws InvalidKeyException
  {
    if (kb.length != 8 && kb.length != 16 && kb.length != 24)
      throw new InvalidKeyException("TripleDES key must be 8, 16 or 24 bytes: "
                                    + kb.length);
    Context ctx = new Context();
    byte[] k1 = new byte[DES.KEY_SIZE];
    System.arraycopy(kb, 0, k1, 0, DES.KEY_SIZE);
    if (! DES.isParityAdjusted(k1, 0))
      DES.adjustParity(k1, 0);
    ctx.k1 = (DES.Context) des.makeKey(k1, bs);

    if (kb.length == 8)
      {
        ctx.k2 = (DES.Context) des.makeKey(k1, bs);
        ctx.k3 = (DES.Context) des.makeKey(k1, bs);
      }
    else
      {
        byte[] k2 = new byte[DES.KEY_SIZE];
        System.arraycopy(kb, DES.KEY_SIZE, k2, 0, DES.KEY_SIZE);
        if (! DES.isParityAdjusted(k2, 0))
          DES.adjustParity(k2, 0);
        ctx.k2 = (DES.Context) des.makeKey(k2, bs);

        byte[] k3 = new byte[DES.KEY_SIZE];
        if (kb.length == 16)
          ctx.k3 = (DES.Context) des.makeKey(k1, bs);
        else
          {
            System.arraycopy(kb, 2 * DES.KEY_SIZE, k3, 0, DES.KEY_SIZE);
            if (! DES.isParityAdjusted(k3, 0))
              DES.adjustParity(k3, 0);
            ctx.k3 = (DES.Context) des.makeKey(k3, bs);
          }
      }
    return ctx;
  }

  public void encrypt(byte[] in, int i, byte[] out, int o, Object K, int bs)
  {
    byte[] temp = new byte[BLOCK_SIZE];
    des.encrypt(in, i, temp, 0, ((Context) K).k1, bs);
    des.decrypt(temp, 0, temp, 0, ((Context) K).k2, bs);
    des.encrypt(temp, 0, out, o, ((Context) K).k3, bs);
  }

  public void decrypt(byte[] in, int i, byte[] out, int o, Object K, int bs)
  {
    byte[] temp = new byte[BLOCK_SIZE];
    des.decrypt(in, i, temp, 0, ((Context) K).k3, bs);
    des.encrypt(temp, 0, temp, 0, ((Context) K).k2, bs);
    des.decrypt(temp, 0, out, o, ((Context) K).k1, bs);
  }

  private final class Context
  {
    DES.Context k1, k2, k3;
  }
}
