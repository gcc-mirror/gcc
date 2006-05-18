/* RSAPSSSignature.java -- 
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

import gnu.java.security.Registry;
import gnu.java.security.hash.HashFactory;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.sig.BaseSignature;
import gnu.java.security.util.Util;

import java.io.PrintWriter;
import java.math.BigInteger;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;

/**
 * <p>The RSA-PSS signature scheme is a public-key encryption scheme combining
 * the RSA algorithm with the Probabilistic Signature Scheme (PSS) encoding
 * method.</p>
 *
 * <p>The inventors of RSA are Ronald L. Rivest, Adi Shamir, and Leonard Adleman,
 * while the inventors of the PSS encoding method are Mihir Bellare and Phillip
 * Rogaway. During efforts to adopt RSA-PSS into the P1363a standards effort,
 * certain adaptations to the original version of RSA-PSS were made by Mihir
 * Bellare and Phillip Rogaway and also by Burt Kaliski (the editor of IEEE
 * P1363a) to facilitate implementation and integration into existing protocols.</p>
 *
 * <p>References:</pr>
 * <ol>
 *    <li><a href="http://www.cosic.esat.kuleuven.ac.be/nessie/workshop/submissions/rsa-pss.zip">
 *    RSA-PSS Signature Scheme with Appendix, part B.</a><br>
 *    Primitive specification and supporting documentation.<br>
 *    Jakob Jonsson and Burt Kaliski.</li>
 * </ol>
 */
public class RSAPSSSignature extends BaseSignature
{

  // Debugging methods and variables
  // -------------------------------------------------------------------------

  private static final String NAME = "rsa-pss";

  private static final boolean DEBUG = false;

  private static final int debuglevel = 1;

  private static final PrintWriter err = new PrintWriter(System.out, true);

  private static void debug(String s)
  {
    err.println(">>> " + NAME + ": " + s);
  }

  // Constants and variables
  // -------------------------------------------------------------------------

  /** The underlying EMSA-PSS instance for this object. */
  private EMSA_PSS pss;

  /** The desired length in octets of the EMSA-PSS salt. */
  private int sLen;

  // Constructor(s)
  // -------------------------------------------------------------------------

  /**
   * Default 0-arguments constructor. Uses SHA-1 as the default hash and a
   * 0-octet <i>salt</i>.
   */
  public RSAPSSSignature()
  {
    this(Registry.SHA160_HASH, 0);
  }

  /**
   * <p>Constructs an instance of this object using the designated message
   * digest algorithm as its underlying hash function, and having 0-octet
   * <i>salt</i>.</p>
   *
   * @param mdName the canonical name of the underlying hash function.
   */
  public RSAPSSSignature(String mdName)
  {
    this(mdName, 0);
  }

  /**
   * <p>Constructs an instance of this object using the designated message
   * digest algorithm as its underlying hash function.</p>
   *
   * @param mdName the canonical name of the underlying hash function.
   * @param sLen the desired length in octets of the salt to use for encoding /
   * decoding signatures.
   */
  public RSAPSSSignature(String mdName, int sLen)
  {
    this(HashFactory.getInstance(mdName), sLen);
  }

  public RSAPSSSignature(IMessageDigest md, int sLen)
  {
    super(Registry.RSA_PSS_SIG, md);

    pss = EMSA_PSS.getInstance(md.name());
    this.sLen = sLen;
  }

  /** Private constructor for cloning purposes. */
  private RSAPSSSignature(RSAPSSSignature that)
  {
    this(that.md.name(), that.sLen);

    this.publicKey = that.publicKey;
    this.privateKey = that.privateKey;
    this.md = (IMessageDigest) that.md.clone();
    this.pss = (EMSA_PSS) that.pss.clone();
  }

  // Class methods
  // -------------------------------------------------------------------------

  // Instance methods
  // -------------------------------------------------------------------------

  // Implementation of abstract methods in superclass ------------------------

  public Object clone()
  {
    return new RSAPSSSignature(this);
  }

  protected void setupForVerification(PublicKey k)
      throws IllegalArgumentException
  {
    if (!(k instanceof RSAPublicKey))
      {
        throw new IllegalArgumentException();
      }
    publicKey = (RSAPublicKey) k;
  }

  protected void setupForSigning(PrivateKey k) throws IllegalArgumentException
  {
    if (!(k instanceof RSAPrivateKey))
      {
        throw new IllegalArgumentException();
      }
    privateKey = (RSAPrivateKey) k;
  }

  protected Object generateSignature() throws IllegalStateException
  {
    // 1. Apply the EMSA-PSS encoding operation to the message M to produce an
    //    encoded message EM of length CEILING((modBits ? 1)/8) octets such
    //    that the bit length of the integer OS2IP(EM) is at most modBits ? 1:
    //    EM = EMSA-PSS-Encode(M,modBits ? 1).
    //    Note that the octet length of EM will be one less than k if
    //    modBits ? 1 is divisible by 8. If the encoding operation outputs
    //    'message too long' or 'encoding error,' then output 'message too
    //    long' or 'encoding error' and stop.
    int modBits = ((RSAPrivateKey) privateKey).getModulus().bitLength();
    byte[] salt = new byte[sLen];
    this.nextRandomBytes(salt);
    byte[] EM = pss.encode(md.digest(), modBits - 1, salt);
    if (DEBUG && debuglevel > 8)
      {
        debug("EM (sign): " + Util.toString(EM));
      }
    // 2. Convert the encoded message EM to an integer message representative
    //    m (see Section 1.2.2): m = OS2IP(EM).
    BigInteger m = new BigInteger(1, EM);
    // 3. Apply the RSASP signature primitive to the public key K and the
    //    message representative m to produce an integer signature
    //    representative s: s = RSASP(K,m).
    BigInteger s = RSA.sign(privateKey, m);
    // 4. Convert the signature representative s to a signature S of length k
    //    octets (see Section 1.2.1): S = I2OSP(s, k).
    // 5. Output the signature S.
    int k = (modBits + 7) / 8;
    //      return encodeSignature(s, k);
    return RSA.I2OSP(s, k);
  }

  protected boolean verifySignature(Object sig) throws IllegalStateException
  {
    if (publicKey == null)
      {
        throw new IllegalStateException();
      }
    //      byte[] S = decodeSignature(sig);
    byte[] S = (byte[]) sig;
    // 1. If the length of the signature S is not k octets, output 'signature
    //    invalid' and stop.
    int modBits = ((RSAPublicKey) publicKey).getModulus().bitLength();
    int k = (modBits + 7) / 8;
    if (S.length != k)
      {
        return false;
      }
    // 2. Convert the signature S to an integer signature representative s:
    //    s = OS2IP(S).
    BigInteger s = new BigInteger(1, S);
    // 3. Apply the RSAVP verification primitive to the public key (n, e) and
    //    the signature representative s to produce an integer message
    //    representative m: m = RSAVP((n, e), s).
    //    If RSAVP outputs 'signature representative out of range,' then
    //    output 'signature invalid' and stop.
    BigInteger m = null;
    try
      {
        m = RSA.verify(publicKey, s);
      }
    catch (IllegalArgumentException x)
      {
        return false;
      }
    // 4. Convert the message representative m to an encoded message EM of
    //    length emLen = CEILING((modBits - 1)/8) octets, where modBits is
    //    equal to the bit length of the modulus: EM = I2OSP(m, emLen).
    //    Note that emLen will be one less than k if modBits - 1 is divisible
    //    by 8. If I2OSP outputs 'integer too large,' then output 'signature
    //    invalid' and stop.
    int emBits = modBits - 1;
    int emLen = (emBits + 7) / 8;
    byte[] EM = m.toByteArray();
    if (DEBUG && debuglevel > 8)
      {
        debug("EM (verify): " + Util.toString(EM));
      }
    if (EM.length > emLen)
      {
        return false;
      }
    else if (EM.length < emLen)
      {
        byte[] newEM = new byte[emLen];
        System.arraycopy(EM, 0, newEM, emLen - EM.length, EM.length);
        EM = newEM;
      }
    // 5. Apply the EMSA-PSS decoding operation to the message M and the
    //    encoded message EM: Result = EMSA-PSS-Decode(M, EM, emBits). If
    //    Result = 'consistent,' output 'signature verified.' Otherwise,
    //    output 'signature invalid.'
    byte[] mHash = md.digest();
    boolean result = false;
    try
      {
        result = pss.decode(mHash, EM, emBits, sLen);
      }
    catch (IllegalArgumentException x)
      {
        result = false;
      }
    return result;
  }

  // Other instance methods --------------------------------------------------

  /**
   * Converts the <i>signature representative</i> <code>s</code> to a signature
   * <code>S</code> of length <code>k</code> octets; i.e.
   * <code>S = I2OSP(s, k)</code>, where <code>k = CEILING(modBits/8)</code>.
   *
   * @param s the <i>signature representative</i>.
   * @param k the length of the output.
   * @return the signature as an octet sequence.
   * @exception IllegalArgumentException if the length in octets of meaningful
   * bytes of <code>s</code> is greater than <code>k</code>, implying that
   * <code>s</code> is not less than the RSA <i>modulus</i>.
   */
  //   private Object encodeSignature(BigInteger s, int k) {
  //      if (DEBUG && debuglevel > 8) {
  //         debug("s.bitLength(): "+String.valueOf(s.bitLength()));
  //         debug("k: "+String.valueOf(k));
  //      }
  //      byte[] result = s.toByteArray();
  //      if (DEBUG && debuglevel > 8) {
  //         debug("s: "+Util.toString(result));
  //         debug("s (bytes): "+String.valueOf(result.length));
  //      }
  //      if (result.length < k) {
  //         byte[] newResult = new byte[k];
  //         System.arraycopy(result, 0, newResult, k-result.length, result.length);
  //         result = newResult;
  //      } else if (result.length > k) { // leftmost extra bytes should all be 0
  //         int limit = result.length - k;
  //         for (int i = 0; i < limit; i++) {
  //            if (result[i] != 0x00) {
  //               throw new IllegalArgumentException("integer too large");
  //            }
  //         }
  //         byte[] newResult = new byte[k];
  //         System.arraycopy(result, limit, newResult, 0, k);
  //         result = newResult;
  //      }
  //      return result;
  //   }
  /**
   * Returns the output of a previously generated signature object as an octet
   * sequence.<p>
   *
   * @return the octet sequence <code>S</code>.
   */
  //   private byte[] decodeSignature(Object signature) {
  //      return (byte[]) signature;
  //   }
}
