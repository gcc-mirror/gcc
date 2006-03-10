/* SSLHMac.java -- SSLv3's MAC algorithm.
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

import java.util.Arrays;
import java.util.Map;

import gnu.java.security.hash.HashFactory;
import gnu.java.security.hash.IMessageDigest;
import gnu.javax.crypto.mac.IMac;

/**
 * The MAC function in SSLv3. This mac is defined as:
 *
 * <pre>
 * hash(MAC_write_secret, pad_2 +
 *      hash(MAC_write_secret + pad_1 + data));</pre>
 *
 * <p><tt>hash</tt> is e.g. MD5 or SHA-1, <tt>pad_1</tt> is the value
 * 0x36 48 times for MD5 and 40 times for SHA-1, and <tt>pad_2</tt> is
 * the value 0x5c repeated similarly.
 */
class SSLHMac implements IMac, Cloneable
{

  // Fields.
  // -------------------------------------------------------------------------

  static final byte PAD1 = 0x36;
  static final byte PAD2 = 0x5c;

  protected IMessageDigest md;
  protected byte[] key;
  protected final byte[] pad1, pad2;

  // Constructors.
  // -------------------------------------------------------------------------

  SSLHMac(String mdName)
  {
    super();
    this.md = HashFactory.getInstance(mdName);
    if (mdName.equalsIgnoreCase("MD5"))
      {
        pad1 = new byte[48];
        pad2 = new byte[48];
      }
    else
      {
        pad1 = new byte[40];
        pad2 = new byte[40];
      }
    Arrays.fill(pad1, PAD1);
    Arrays.fill(pad2, PAD2);
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException cnse)
      {
        throw new Error();
      }
  }

  public String name()
  {
    return "SSLHMac-" + md.name();
  }

  public int macSize()
  {
    return md.hashSize();
  }

  public void init(Map attributes)
  {
    key = (byte[]) attributes.get(MAC_KEY_MATERIAL);
    if (key == null)
      throw new NullPointerException();
    reset();
  }

  public void reset()
  {
    md.reset();
    md.update(key, 0, key.length);
    md.update(pad1, 0, pad1.length);
  }

  public byte[] digest()
  {
    byte[] h1 = md.digest();
    md.update(key, 0, key.length);
    md.update(pad2, 0, pad2.length);
    md.update(h1, 0, h1.length);
    byte[] result = md.digest();
    reset();
    return result;
  }

  public void update(byte b)
  {
    md.update(b);
  }

  public void update(byte[] buf, int off, int len)
  {
    md.update(buf, off, len);
  }

  public boolean selfTest()
  {
    return true; // XXX
  }
}
