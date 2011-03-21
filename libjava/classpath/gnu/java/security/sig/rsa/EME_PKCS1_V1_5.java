/* EME_PKCS1_V1_5.java --
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.java.security.sig.rsa;

import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.LimitReachedException;
import gnu.java.security.util.PRNG;

import java.io.ByteArrayOutputStream;
import java.security.interfaces.RSAKey;
import java.util.Random;

/**
 * An implementation of the EME-PKCS1-V1.5 encoding and decoding methods.
 * <p>
 * EME-PKCS1-V1.5 is parameterised by the entity <code>k</code> which is the
 * byte count of an RSA public shared modulus.
 * <p>
 * References:
 * <ol>
 * <li><a href="http://www.ietf.org/rfc/rfc3447.txt">Public-Key Cryptography
 * Standards (PKCS) #1:</a><br>
 * RSA Cryptography Specifications Version 2.1.<br>
 * Jakob Jonsson and Burt Kaliski.</li>
 * </ol>
 */
public class EME_PKCS1_V1_5
{
  private int k;

  private ByteArrayOutputStream baos = new ByteArrayOutputStream();

  /** Our default source of randomness. */
  private PRNG prng = PRNG.getInstance();

  private EME_PKCS1_V1_5(final int k)
  {
    super();

    this.k = k;
  }

  public static final EME_PKCS1_V1_5 getInstance(final int k)
  {
    if (k < 0)
      throw new IllegalArgumentException("k must be a positive integer");

    return new EME_PKCS1_V1_5(k);
  }

  public static final EME_PKCS1_V1_5 getInstance(final RSAKey key)
  {
    final int modBits = key.getModulus().bitLength();
    final int k = (modBits + 7) / 8;
    return EME_PKCS1_V1_5.getInstance(k);
  }

  /**
   * Generates an octet string <code>PS</code> of length <code>k - mLen -
   * 3</code> consisting of pseudo-randomly generated nonzero octets. The length
   * of <code>PS</code> will be at least eight octets.
   * <p>
   * The method then concatenates <code>PS</code>, the message <code>M</code>,
   * and other padding to form an encoded message <code>EM</code> of length
   * <code>k</code> octets as:
   * <pre>
   *     EM = 0x00 || 0x02 || PS || 0x00 || M.
   * </pre>
   * <p>
   * This method uses a default PRNG to obtain the padding bytes.
   *
   * @param M the message to encode.
   * @return the encoded message <code>EM</code>.
   */
  public byte[] encode(final byte[] M)
  {
    // a. Generate an octet string PS of length k - mLen - 3 consisting
    // of pseudo-randomly generated nonzero octets. The length of PS
    // will be at least eight octets.
    final byte[] PS = new byte[k - M.length - 3];
    // FIXME. This should be configurable, somehow.
    prng.nextBytes(PS);
    int i = 0;
    for (; i < PS.length; i++)
      {
        if (PS[i] == 0)
          PS[i] = 1;
      }
    // b. Concatenate PS, the message M, and other padding to form an
    // encoded message EM of length k octets as
    //
    // EM = 0x00 || 0x02 || PS || 0x00 || M.
    return assembleEM(PS, M);
  }

  /**
   * Similar to {@link #encode(byte[])} method, except that the source of
   * randomness to use for obtaining the padding bytes (an instance of
   * {@link IRandom}) is given as a parameter.
   *
   * @param M the message to encode.
   * @param irnd the {@link IRandom} instance to use as a source of randomness.
   * @return the encoded message <code>EM</code>.
   */
  public byte[] encode(final byte[] M, final IRandom irnd)
  {
    final byte[] PS = new byte[k - M.length - 3];
    try
      {
        irnd.nextBytes(PS, 0, PS.length);
        int i = 0;
        outer: while (true)
          {
            for (; i < PS.length; i++)
              {
                if (PS[i] == 0x00)
                  {
                    System.arraycopy(PS, i + 1, PS, i, PS.length - i - 1);
                    irnd.nextBytes(PS, PS.length - 1, 1);
                    continue outer;
                  }
              }
            break;
          }
      }
    catch (IllegalStateException x)
      {
        throw new RuntimeException("encode(): " + String.valueOf(x));
      }
    catch (LimitReachedException x)
      {
        throw new RuntimeException("encode(): " + String.valueOf(x));
      }
    return assembleEM(PS, M);
  }

  /**
   * Similar to the {@link #encode(byte[], IRandom)} method, except that the
   * source of randmoness is an instance of {@link Random}.
   *
   * @param M the message to encode.
   * @param rnd the {@link Random} instance to use as a source of randomness.
   * @return the encoded message <code>EM</code>.
   */
  public byte[] encode(final byte[] M, final Random rnd)
  {
    final byte[] PS = new byte[k - M.length - 3];
    rnd.nextBytes(PS);
    int i = 0;
    outer: while (true)
      {
        for (; i < PS.length; i++)
          {
            if (PS[i] == 0x00)
              {
                System.arraycopy(PS, i + 1, PS, i, PS.length - i - 1);
                PS[PS.length - 1] = (byte) rnd.nextInt();
                continue outer;
              }
          }
        break;
      }
    return assembleEM(PS, M);
  }

  /**
   * Separate the encoded message <code>EM</code> into an octet string
   * <code>PS</code> consisting of nonzero octets and a message <code>M</code>
   * as:
   * <pre>
   *     EM = 0x00 || 0x02 || PS || 0x00 || M.
   * </pre>
   * <p>
   * If the first octet of <code>EM</code> does not have hexadecimal value
   * <code>0x00</code>, if the second octet of <code>EM</code> does not
   * have hexadecimal value <code>0x02</code>, if there is no octet with
   * hexadecimal value <code>0x00</code> to separate <code>PS</code> from
   * <code>M</code>, or if the length of <code>PS</code> is less than
   * <code>8</code> octets, output "decryption error" and stop.
   *
   * @param EM the designated encoded message.
   * @return the decoded message <code>M</code> framed in the designated
   *         <code>EM</code> value.
   * @throws IllegalArgumentException if the length of the designated entity
   *           <code>EM</code> is different than <code>k</code> (the length
   *           in bytes of the public shared modulus), or if any of the
   *           conditions described above is detected.
   */
  public byte[] decode(final byte[] EM)
  {
    // Separate the encoded message EM into an
    // octet string PS consisting of nonzero octets and a message M as
    //
    // EM = 0x00 || 0x02 || PS || 0x00 || M.
    //
    // If the first octet of EM does not have hexadecimal value 0x00, if
    // the second octet of EM does not have hexadecimal value 0x02, if
    // there is no octet with hexadecimal value 0x00 to separate PS from
    // M, or if the length of PS is less than 8 octets, output
    // "decryption error" and stop. (See the note below.)
    final int emLen = EM.length;
    if (emLen != k)
      throw new IllegalArgumentException("decryption error");
    if (EM[0] != 0x00)
      throw new IllegalArgumentException("decryption error");
    if (EM[1] != 0x02)
      throw new IllegalArgumentException("decryption error");
    int i = 2;
    for (; i < emLen; i++)
      {
        if (EM[i] == 0x00)
          break;
      }
    if (i >= emLen || i < 11)
      throw new IllegalArgumentException("decryption error");
    i++;
    final byte[] result = new byte[emLen - i];
    System.arraycopy(EM, i, result, 0, result.length);
    return result;
  }

  private byte[] assembleEM(final byte[] PS, final byte[] M)
  {
    // b. Concatenate PS, the message M, and other padding to form an
    // encoded message EM of length k octets as
    //
    // EM = 0x00 || 0x02 || PS || 0x00 || M.
    baos.reset();
    baos.write(0x00);
    baos.write(0x02);
    baos.write(PS, 0, PS.length);
    baos.write(0x00);
    baos.write(M, 0, M.length);
    final byte[] result = baos.toByteArray();
    baos.reset();
    return result;
  }
}
