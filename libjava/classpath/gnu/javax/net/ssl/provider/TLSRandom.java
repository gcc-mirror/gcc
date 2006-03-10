/* TLSRandom.java -- The TLS pseudo-random function.
   Copyright (C) 2006  Free Software Foundation, Inc.

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


package gnu.javax.net.ssl.provider;

import java.security.InvalidKeyException;
import java.util.HashMap;
import java.util.Map;

import gnu.java.security.hash.HashFactory;
import gnu.javax.crypto.mac.IMac;
import gnu.java.security.prng.IRandom;

class TLSRandom implements IRandom
{

  // Fields.
  // -------------------------------------------------------------------------

  /**
   * Property name for the secret that will be used to initialize the HMACs.
   */
  static final String SECRET = "jessie.tls.prng.secret";

  /**
   * Property name for the seed.
   */
  static final String SEED = "jessie.tls.prng.seed";

  private final IMac hmac_sha, hmac_md5;
  private byte[] sha_a, md5_a;
  private byte[] seed;
  private final byte[] buffer;
  private int idx;
  private boolean init;

  // Constructors.
  // -------------------------------------------------------------------------

  TLSRandom()
  {
    hmac_sha = new TLSHMac(HashFactory.getInstance("SHA1"));
    hmac_md5 = new TLSHMac(HashFactory.getInstance("MD5"));
    buffer = new byte[80];   // 80 == LCM of 16 and 20.
    idx = 0;
    init = false;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException shouldNotHappen)
      {
        throw new Error();
      }
  }

  public void init(Map attributes)
  {
    HashMap sha_attr = new HashMap();
    HashMap md5_attr = new HashMap();
    byte[] secret = (byte[]) attributes.get(SECRET);
    if (secret != null)
      {
        int l = (secret.length >>> 1) + (secret.length & 1);
        byte[] s1 = Util.trim(secret, 0, l);
        byte[] s2 = Util.trim(secret, secret.length - l, l);
        md5_attr.put(IMac.MAC_KEY_MATERIAL, s1);
        sha_attr.put(IMac.MAC_KEY_MATERIAL, s2);
        try
          {
            hmac_md5.init(md5_attr);
            hmac_sha.init(sha_attr);
          }
        catch (InvalidKeyException ike)
          {
            throw new Error(ike.toString());
          }
      }
    else if (!init)
      {
        throw new IllegalArgumentException("no secret supplied");
      }
    // else re-use

    byte[] seeed = (byte[]) attributes.get(SEED);
    if (seeed != null)
      {
        seed = (byte[]) seeed.clone();
      }
    else if (!init)
      {
        throw new IllegalArgumentException("no seed supplied");
      }
    // else re-use

    // A(0) is the seed, A(1) = HMAC_hash(secret, A(0)).
    hmac_md5.update(seed, 0, seed.length);
    md5_a = hmac_md5.digest();
    hmac_md5.reset();
    hmac_sha.update(seed, 0, seed.length);
    sha_a = hmac_sha.digest();
    hmac_sha.reset();
    fillBuffer();
    init = true;
  }

  public String name()
  {
    return "TLSRandom";
  }

  public byte nextByte()
  {
    if (!init)
      throw new IllegalStateException();
    if (idx >= buffer.length)
      fillBuffer();
    return buffer[idx++];
  }

  public void nextBytes(byte[] buf, int off, int len)
  {
    if (!init)
      throw new IllegalStateException();
    if (buf == null)
      throw new NullPointerException();
    if (off < 0 || off > buf.length || off + len > buf.length)
      throw new ArrayIndexOutOfBoundsException();
    int count = 0;
    if (idx >= buffer.length)
      fillBuffer();
    while (count < len)
      {
        int l = Math.min(buffer.length-idx, len-count);
        System.arraycopy(buffer, idx, buf, off+count, l);
        idx += l;
        count += l;
        if (count < len && idx >= buffer.length)
          fillBuffer();
      }
  }

  // For future versions of GNU Crypto. No-ops.
  public void addRandomByte (byte b)
  {
  }

  public void addRandomBytes(byte[] buffer) {
    addRandomBytes(buffer, 0, buffer.length);
  }

  public void addRandomBytes (byte[] b, int i, int j)
  {
  }

  // Own methods.
  // -------------------------------------------------------------------------

  /*
   * The PRF is defined as:
   *
   *   PRF(secret, label, seed) = P_MD5(S1, label + seed) XOR
   *                              P_SHA-1(S2, label + seed);
   *
   * P_hash is defined as:
   *
   *   P_hash(secret, seed) = HMAC_hash(secret, A(1) + seed) +
   *                          HMAC_hash(secret, A(2) + seed) +
   *                          HMAC_hash(secret, A(3) + seed) + ...
   *
   * And A() is defined as:
   *
   *   A(0) = seed
   *   A(i) = HMAC_hash(secret, A(i-1))
   *
   * For simplicity, we compute an 80-byte block on each call, which
   * corresponds to five iterations of MD5, and four of SHA-1.
   */
  private synchronized void fillBuffer()
  {
    int len = hmac_md5.macSize();
    for (int i = 0; i < buffer.length; i += len)
      {
        hmac_md5.update(md5_a, 0, md5_a.length);
        hmac_md5.update(seed, 0, seed.length);
        byte[] b = hmac_md5.digest();
        hmac_md5.reset();
        System.arraycopy(b, 0, buffer, i, len);
        hmac_md5.update(md5_a, 0, md5_a.length);
        md5_a = hmac_md5.digest();
        hmac_md5.reset();
      }
    len = hmac_sha.macSize();
    for (int i = 0; i < buffer.length; i += len)
      {
        hmac_sha.update(sha_a, 0, sha_a.length);
        hmac_sha.update(seed, 0, seed.length);
        byte[] b = hmac_sha.digest();
        hmac_sha.reset();
        for (int j = 0; j < len; j++)
          {
            buffer[j + i] ^= b[j];
          }
        hmac_sha.update(sha_a, 0, sha_a.length);
        sha_a = hmac_sha.digest();
        hmac_sha.reset();
      }
    idx = 0;
  }
}
