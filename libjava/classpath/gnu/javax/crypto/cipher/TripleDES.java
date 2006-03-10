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

import java.util.Collections;
import java.util.Iterator;
import java.security.InvalidKeyException;

/**
 * Triple-DES, 3DES, or DESede is a <i>combined cipher</i> that uses
 * three iterations of the Data Encryption Standard cipher to improve
 * the security (at the cost of speed) of plain DES.
 *
 * <p>Triple-DES runs the DES algorithm three times with three
 * independent 56 bit keys. To encrypt:</p>
 *
 * <blockquote><i>C<sub>i</sub> =
 * E<sub>k3</sub> ( E<sub>k2</sub><sup>-1</sup> ( E<sub>k1</sub> ( P<sub>i</sub> )))</i></blockquote>
 *
 * <p>And to decrypt:</p>
 *
 * <blockquote><i>P<sub>i</sub> =
 * E<sub>k1</sub><sup>-1</sup> ( E<sub>k2</sub> ( E<sub>k3</sub><sup>-1</sup> ( C<sub>i</sub> )))</i></blockquote>
 *
 * <p>(The "ede" comes from the encryption operation, which runs
 * Encrypt-Decrypt-Encrypt)</p>
 *
 * <p>References:</p>
 * <ol>
 * <li>Bruce Schneier, <i>Applied Cryptography: Protocols, Algorithms,
 * and Source Code in C, Second Edition</i>. (1996 John Wiley and Sons)
 * ISBN 0-471-11709-9. Page 294--295.</li>
 * </ol>
 */
public class TripleDES extends BaseCipher
{

  // Constants and variables.
  // -----------------------------------------------------------------------

  /** Triple-DES only operates on 64 bit blocks. */
  public static final int BLOCK_SIZE = 8;

  /** Triple-DES uses 168 bits of a parity-adjusted 192 bit key. */
  public static final int KEY_SIZE = 24;

  /** The underlying DES instance. */
  private DES des;

  // Constructors.
  // -----------------------------------------------------------------------

  /**
   * Default 0-arguments constructor.
   */
  public TripleDES()
  {
    super(Registry.TRIPLEDES_CIPHER, BLOCK_SIZE, KEY_SIZE);
    des = new DES();
  }

  // Class methods.
  // -----------------------------------------------------------------------

  /**
   * Transform a key so it will be parity adjusted.
   *
   * @param kb     The key bytes to adjust.
   * @param offset The starting offset into the key bytes.
   * @see DES#adjustParity(byte[],int)
   */
  public static void adjustParity(byte[] kb, int offset)
  {
    DES.adjustParity(kb, offset);
    DES.adjustParity(kb, offset + 8);
    DES.adjustParity(kb, offset + 16);
  }

  /**
   * Tests if a byte array has already been parity adjusted.
   *
   * @param kb     The key bytes to test.
   * @param offset The starting offset into the key bytes.
   * @return <code>true</code> if the bytes in <i>kb</i> starting at
   *         <i>offset</i> are parity adjusted.
   * @see DES#isParityAdjusted(byte[],int)
   * @see #adjustParity(byte[],int)
   */
  public static boolean isParityAdjusted(byte[] kb, int offset)
  {
    return DES.isParityAdjusted(kb, offset)
           && DES.isParityAdjusted(kb, offset + 8)
           && DES.isParityAdjusted(kb, offset + 16);
  }

  // Methods implementing BaseCipher.
  // -----------------------------------------------------------------------

  public Object clone()
  {
    return new TripleDES();
  }

  public Iterator blockSizes()
  {
    return Collections.singleton(new Integer(BLOCK_SIZE)).iterator();
  }

  public Iterator keySizes()
  {
    return Collections.singleton(new Integer(KEY_SIZE)).iterator();
  }

  public Object makeKey(byte[] kb, int bs) throws InvalidKeyException
  {
    if (kb.length != KEY_SIZE)
      throw new InvalidKeyException("TripleDES key must be 24 bytes");

    if (!isParityAdjusted(kb, 0))
      adjustParity(kb, 0);

    byte[] k1 = new byte[DES.KEY_SIZE], k2 = new byte[DES.KEY_SIZE], k3 = new byte[DES.KEY_SIZE];
    System.arraycopy(kb, 0, k1, 0, DES.KEY_SIZE);
    System.arraycopy(kb, DES.KEY_SIZE, k2, 0, DES.KEY_SIZE);
    System.arraycopy(kb, 2 * DES.KEY_SIZE, k3, 0, DES.KEY_SIZE);
    Context ctx = new Context();

    ctx.k1 = (DES.Context) des.makeKey(k1, bs);
    ctx.k2 = (DES.Context) des.makeKey(k2, bs);
    ctx.k3 = (DES.Context) des.makeKey(k3, bs);

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

  // Inner classes.
  // -----------------------------------------------------------------------

  private final class Context
  {
    DES.Context k1, k2, k3;
  }
}