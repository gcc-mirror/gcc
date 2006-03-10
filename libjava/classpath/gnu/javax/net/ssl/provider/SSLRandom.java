/* SSLRandom.java -- SSLv3 pseudo-random function.
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

import java.util.Map;
import gnu.java.security.hash.HashFactory;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.LimitReachedException;

class SSLRandom implements IRandom
{

  // Fields.
  // -------------------------------------------------------------------------

  static final String SECRET = "jessie.sslprng.secret";
  static final String SEED = "jessie.sslprng.seed";

  private final IMessageDigest md5, sha;
  private byte[] secret;
  private byte[] buffer;
  private byte pad;
  private byte[] seed;
  private int idx;

  // Constructor.
  // -------------------------------------------------------------------------

  SSLRandom()
  {
    md5 = HashFactory.getInstance("MD5");
    sha = HashFactory.getInstance("SHA-1");
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void init(Map attrib)
  {
    secret = (byte[]) attrib.get(SECRET);
    seed = (byte[]) attrib.get(SEED);

    if (secret == null || seed == null)
      throw new NullPointerException();

    pad = (byte) 'A';
    try { buffer = nextBlock(); }
    catch (LimitReachedException cantHappen) { }
  }

  public String name()
  {
    return "SSLRandom";
  }

  public Object clone()
  {
    throw new UnsupportedOperationException();
  }

  public byte nextByte() throws LimitReachedException
  {
    if (buffer == null)
      throw new IllegalStateException();
    if (idx >= buffer.length)
      buffer = nextBlock();
    return buffer[idx++];
  }

  public void nextBytes(byte[] buf, int off, int len)
    throws LimitReachedException
  {
    if (buffer == null)
      throw new IllegalStateException();
    if (buf == null)
      throw new NullPointerException();
    if (off < 0 || len < 0 || off+len > buf.length)
      throw new IndexOutOfBoundsException();
    int count = 0;
    while (count < len)
      {
        if (idx >= buffer.length)
          buffer = nextBlock();
        int l = Math.min(buffer.length-idx, len-count);
        System.arraycopy(buffer, idx, buf, off+count, l);
        count += l;
        idx += l;
      }
  }

  public boolean selfTest()
  {
    return true; // XXX
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

  private byte[] nextBlock() throws LimitReachedException
  {
    int count = pad - 'A' + 1;
    if (count > 26)
      throw new LimitReachedException();
    for (int i = 0; i < count; i++)
      sha.update(pad);
    sha.update(secret, 0, secret.length);
    sha.update(seed, 0, seed.length);
    byte[] b = sha.digest();
    md5.update(secret, 0, secret.length);
    md5.update(b, 0, b.length);
    idx = 0;
    pad++;
    return md5.digest();
  }
}
