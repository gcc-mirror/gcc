/* EMSA_PSS.java --
   Copyright (C) 2001, 2002, 2003, 2006 Free Software Foundation, Inc.

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

import gnu.java.security.Configuration;
import gnu.java.security.hash.HashFactory;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.util.Util;

import java.util.Arrays;
import java.util.logging.Logger;

/**
 * An implementation of the EMSA-PSS encoding/decoding scheme.
 * <p>
 * EMSA-PSS coincides with EMSA4 in IEEE P1363a D5 except that EMSA-PSS acts on
 * octet strings and not on bit strings. In particular, the bit lengths of the
 * hash and the salt must be multiples of 8 in EMSA-PSS. Moreover, EMSA4 outputs
 * an integer of a desired bit length rather than an octet string.
 * <p>
 * EMSA-PSS is parameterized by the choice of hash function Hash and mask
 * generation function MGF. In this submission, MGF is based on a Hash
 * definition that coincides with the corresponding definitions in IEEE Std
 * 1363-2000, PKCS #1 v2.0, and the draft ANSI X9.44. In PKCS #1 v2.0 and the
 * draft ANSI X9.44, the recommended hash function is SHA-1, while IEEE Std
 * 1363-2000 recommends SHA-1 and RIPEMD-160.
 * <p>
 * References:
 * <ol>
 * <li><a
 * href="http://www.cosic.esat.kuleuven.ac.be/nessie/workshop/submissions/rsa-pss.zip">
 * RSA-PSS Signature Scheme with Appendix, part B.</a><br>
 * Primitive specification and supporting documentation.<br>
 * Jakob Jonsson and Burt Kaliski.</li>
 * </ol>
 */
public class EMSA_PSS
    implements Cloneable
{
  private static final Logger log = Logger.getLogger(EMSA_PSS.class.getName());

  /** The underlying hash function to use with this instance. */
  private IMessageDigest hash;

  /** The output size of the hash function in octets. */
  private int hLen;

  /**
   * Trivial private constructor to enforce use through Factory method.
   *
   * @param hash the message digest instance to use with this scheme instance.
   */
  private EMSA_PSS(IMessageDigest hash)
  {
    super();

    this.hash = hash;
    hLen = hash.hashSize();
  }

  /**
   * Returns an instance of this object given a designated name of a hash
   * function.
   *
   * @param mdName the canonical name of a hash function.
   * @return an instance of this object configured for use with the designated
   *         options.
   */
  public static EMSA_PSS getInstance(String mdName)
  {
    IMessageDigest hash = HashFactory.getInstance(mdName);
    return new EMSA_PSS(hash);
  }

  public Object clone()
  {
    return getInstance(hash.name());
  }

  /**
   * The encoding operation EMSA-PSS-Encode computes the hash of a message
   * <code>M</code> using a hash function and maps the result to an encoded
   * message <code>EM</code> of a specified length using a mask generation
   * function.
   *
   * @param mHash the byte sequence resulting from applying the message digest
   *          algorithm Hash to the message <i>M</i>.
   * @param emBits the maximal bit length of the integer OS2IP(EM), at least
   *          <code>8.hLen + 8.sLen + 9</code>.
   * @param salt the salt to use when encoding the output.
   * @return the encoded message <code>EM</code>, an octet string of length
   *         <code>emLen = CEILING(emBits / 8)</code>.
   * @exception IllegalArgumentException if an exception occurs.
   */
  public byte[] encode(byte[] mHash, int emBits, byte[] salt)
  {
    int sLen = salt.length;
    // 1. If the length of M is greater than the input limitation for the hash
    // function (2**61 - 1 octets for SHA-1) then output "message too long"
    // and stop.
    // 2. Let mHash = Hash(M), an octet string of length hLen.
    if (hLen != mHash.length)
      throw new IllegalArgumentException("wrong hash");
    // 3. If emBits < 8.hLen + 8.sLen + 9, output 'encoding error' and stop.
    if (emBits < (8 * hLen + 8 * sLen + 9))
      throw new IllegalArgumentException("encoding error");
    int emLen = (emBits + 7) / 8;
    // 4. Generate a random octet string salt of length sLen; if sLen = 0,
    // then salt is the empty string.
    // ...passed as argument to accomodate JCE
    // 5. Let M0 = 00 00 00 00 00 00 00 00 || mHash || salt;
    // M0 is an octet string of length 8 + hLen + sLen with eight initial zero
    // octets.
    // 6. Let H = Hash(M0), an octet string of length hLen.
    byte[] H;
    int i;
    synchronized (hash)
      {
        for (i = 0; i < 8; i++)
          hash.update((byte) 0x00);

        hash.update(mHash, 0, hLen);
        hash.update(salt, 0, sLen);
        H = hash.digest();
      }
    // 7. Generate an octet string PS consisting of emLen - sLen - hLen - 2
    // zero octets. The length of PS may be 0.
    // 8. Let DB = PS || 01 || salt.
    byte[] DB = new byte[emLen - sLen - hLen - 2 + 1 + sLen];
    DB[emLen - sLen - hLen - 2] = 0x01;
    System.arraycopy(salt, 0, DB, emLen - sLen - hLen - 1, sLen);
    // 9. Let dbMask = MGF(H, emLen - hLen - 1).
    byte[] dbMask = MGF(H, emLen - hLen - 1);
    if (Configuration.DEBUG)
      {
        log.fine("dbMask (encode): " + Util.toString(dbMask));
        log.fine("DB (encode): " + Util.toString(DB));
      }
    // 10. Let maskedDB = DB XOR dbMask.
    for (i = 0; i < DB.length; i++)
      DB[i] = (byte)(DB[i] ^ dbMask[i]);
    // 11. Set the leftmost 8emLen - emBits bits of the leftmost octet in
    // maskedDB to zero.
    DB[0] &= (0xFF >>> (8 * emLen - emBits));
    // 12. Let EM = maskedDB || H || bc, where bc is the single octet with
    // hexadecimal value 0xBC.
    byte[] result = new byte[emLen];
    System.arraycopy(DB, 0, result, 0, emLen - hLen - 1);
    System.arraycopy(H, 0, result, emLen - hLen - 1, hLen);
    result[emLen - 1] = (byte) 0xBC;
    // 13. Output EM.
    return result;
  }

  /**
   * The decoding operation EMSA-PSS-Decode recovers the message hash from an
   * encoded message <code>EM</code> and compares it to the hash of
   * <code>M</code>.
   *
   * @param mHash the byte sequence resulting from applying the message digest
   *          algorithm Hash to the message <i>M</i>.
   * @param EM the <i>encoded message</i>, an octet string of length
   *          <code>emLen = CEILING(emBits/8).
   * @param emBits the maximal bit length of the integer OS2IP(EM), at least
   * <code>8.hLen + 8.sLen + 9</code>.
   * @param sLen the length, in octets, of the expected salt.
   * @return <code>true</code> if the result of the verification was
   * <i>consistent</i> with the expected reseult; and <code>false</code> if the
   * result was <i>inconsistent</i>.
   * @exception IllegalArgumentException if an exception occurs.
   */
  public boolean decode(byte[] mHash, byte[] EM, int emBits, int sLen)
  {
    if (Configuration.DEBUG)
      {
        log.fine("mHash: " + Util.toString(mHash));
        log.fine("EM: " + Util.toString(EM));
        log.fine("emBits: " + String.valueOf(emBits));
        log.fine("sLen: " + String.valueOf(sLen));
      }
    if (sLen < 0)
      throw new IllegalArgumentException("sLen");
    // 1. If the length of M is greater than the input limitation for the hash
    // function (2**61 ? 1 octets for SHA-1) then output 'inconsistent' and
    // stop.
    // 2. Let mHash = Hash(M), an octet string of length hLen.
    if (hLen != mHash.length)
      {
        if (Configuration.DEBUG)
          log.fine("hLen != mHash.length; hLen: " + String.valueOf(hLen));
        throw new IllegalArgumentException("wrong hash");
      }
    // 3. If emBits < 8.hLen + 8.sLen + 9, output 'decoding error' and stop.
    if (emBits < (8 * hLen + 8 * sLen + 9))
      {
        if (Configuration.DEBUG)
          log.fine("emBits < (8hLen + 8sLen + 9); sLen: "
                   + String.valueOf(sLen));
        throw new IllegalArgumentException("decoding error");
      }
    int emLen = (emBits + 7) / 8;
    // 4. If the rightmost octet of EM does not have hexadecimal value bc,
    // output 'inconsistent' and stop.
    if ((EM[EM.length - 1] & 0xFF) != 0xBC)
      {
        if (Configuration.DEBUG)
          log.fine("EM does not end with 0xBC");
        return false;
      }
    // 5. Let maskedDB be the leftmost emLen ? hLen ? 1 octets of EM, and let
    // H be the next hLen octets.
    // 6. If the leftmost 8.emLen ? emBits bits of the leftmost octet in
    // maskedDB are not all equal to zero, output 'inconsistent' and stop.
    if ((EM[0] & (0xFF << (8 - (8 * emLen - emBits)))) != 0)
      {
        if (Configuration.DEBUG)
          log.fine("Leftmost 8emLen - emBits bits of EM are not 0s");
        return false;
      }
    byte[] DB = new byte[emLen - hLen - 1];
    byte[] H = new byte[hLen];
    System.arraycopy(EM, 0, DB, 0, emLen - hLen - 1);
    System.arraycopy(EM, emLen - hLen - 1, H, 0, hLen);
    // 7. Let dbMask = MGF(H, emLen ? hLen ? 1).
    byte[] dbMask = MGF(H, emLen - hLen - 1);
    // 8. Let DB = maskedDB XOR dbMask.
    int i;
    for (i = 0; i < DB.length; i++)
      DB[i] = (byte)(DB[i] ^ dbMask[i]);
    // 9. Set the leftmost 8.emLen ? emBits bits of DB to zero.
    DB[0] &= (0xFF >>> (8 * emLen - emBits));
    if (Configuration.DEBUG)
      {
        log.fine("dbMask (decode): " + Util.toString(dbMask));
        log.fine("DB (decode): " + Util.toString(DB));
      }
    // 10. If the emLen -hLen -sLen -2 leftmost octets of DB are not zero or
    // if the octet at position emLen -hLen -sLen -1 is not equal to 0x01,
    // output 'inconsistent' and stop.
    // IMPORTANT (rsn): this is an error in the specs, the index of the 0x01
    // byte should be emLen -hLen -sLen -2 and not -1! authors have been advised
    for (i = 0; i < (emLen - hLen - sLen - 2); i++)
      {
        if (DB[i] != 0)
          {
            if (Configuration.DEBUG)
              log.fine("DB[" + String.valueOf(i) + "] != 0x00");
            return false;
          }
      }
    if (DB[i] != 0x01)
      { // i == emLen -hLen -sLen -2
        if (Configuration.DEBUG)
          log.fine("DB's byte at position (emLen -hLen -sLen -2); i.e. "
                   + String.valueOf(i) + " is not 0x01");
        return false;
      }
    // 11. Let salt be the last sLen octets of DB.
    byte[] salt = new byte[sLen];
    System.arraycopy(DB, DB.length - sLen, salt, 0, sLen);
    // 12. Let M0 = 00 00 00 00 00 00 00 00 || mHash || salt;
    // M0 is an octet string of length 8 + hLen + sLen with eight initial
    // zero octets.
    // 13. Let H0 = Hash(M0), an octet string of length hLen.
    byte[] H0;
    synchronized (hash)
      {
        for (i = 0; i < 8; i++)
          hash.update((byte) 0x00);

        hash.update(mHash, 0, hLen);
        hash.update(salt, 0, sLen);
        H0 = hash.digest();
      }
    // 14. If H = H0, output 'consistent.' Otherwise, output 'inconsistent.'
    return Arrays.equals(H, H0);
  }

  /**
   * A mask generation function takes an octet string of variable length and a
   * desired output length as input, and outputs an octet string of the desired
   * length. There may be restrictions on the length of the input and output
   * octet strings, but such bounds are generally very large. Mask generation
   * functions are deterministic; the octet string output is completely
   * determined by the input octet string. The output of a mask generation
   * function should be pseudorandom, that is, it should be infeasible to
   * predict, given one part of the output but not the input, another part of
   * the output. The provable security of RSA-PSS relies on the random nature of
   * the output of the mask generation function, which in turn relies on the
   * random nature of the underlying hash function.
   *
   * @param Z a seed.
   * @param l the desired output length in octets.
   * @return the mask.
   * @exception IllegalArgumentException if the desired output length is too
   *              long.
   */
  private byte[] MGF(byte[] Z, int l)
  {
    // 1. If l > (2**32).hLen, output 'mask too long' and stop.
    if (l < 1 || (l & 0xFFFFFFFFL) > ((hLen & 0xFFFFFFFFL) << 32L))
      throw new IllegalArgumentException("mask too long");
    // 2. Let T be the empty octet string.
    byte[] result = new byte[l];
    // 3. For i = 0 to CEILING(l/hLen) ? 1, do
    int limit = ((l + hLen - 1) / hLen) - 1;
    IMessageDigest hashZ = null;
    hashZ = (IMessageDigest) hash.clone();
    hashZ.digest();
    hashZ.update(Z, 0, Z.length);
    IMessageDigest hashZC = null;
    byte[] t;
    int sofar = 0;
    int length;
    for (int i = 0; i < limit; i++)
      {
        // 3.1 Convert i to an octet string C of length 4 with the primitive
        // I2OSP: C = I2OSP(i, 4).
        // 3.2 Concatenate the hash of the seed Z and C to the octet string T:
        // T = T || Hash(Z || C)
        hashZC = (IMessageDigest) hashZ.clone();
        hashZC.update((byte)(i >>> 24));
        hashZC.update((byte)(i >>> 16));
        hashZC.update((byte)(i >>> 8));
        hashZC.update((byte) i);
        t = hashZC.digest();
        length = l - sofar;
        length = (length > hLen ? hLen : length);
        System.arraycopy(t, 0, result, sofar, length);
        sofar += length;
      }
    // 4. Output the leading l octets of T as the octet string mask.
    return result;
  }
}
