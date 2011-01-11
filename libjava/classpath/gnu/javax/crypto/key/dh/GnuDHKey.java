/* GnuDHKey.java --
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


package gnu.javax.crypto.key.dh;

import gnu.java.security.Registry;
import gnu.java.security.action.GetPropertyAction;
import gnu.java.security.util.FormatUtil;

import java.math.BigInteger;
import java.security.AccessController;
import java.security.Key;

import javax.crypto.interfaces.DHKey;
import javax.crypto.spec.DHParameterSpec;

/**
 * A base asbtract class for both public and private Diffie-Hellman keys. It
 * encapsulates the two DH numbers: <code>p</code>, and <code>g</code>.
 * <p>
 * According to the JDK, cryptographic <i>Keys</i> all have a <i>format</i>.
 * The format used in this implementation is called <i>Raw</i>, and basically
 * consists of the raw byte sequences of algorithm parameters. The exact order
 * of the byte sequences and the implementation details are given in each of the
 * relevant <code>getEncoded()</code> methods of each of the private and
 * public keys.
 * <p>
 * Reference:
 * <ol>
 * <li><a href="http://www.ietf.org/rfc/rfc2631.txt">Diffie-Hellman Key
 * Agreement Method</a><br>
 * Eric Rescorla.</li>
 * </ol>
 */
public abstract class GnuDHKey
    implements Key, DHKey
{
  /** The public prime q. A prime divisor of p-1. */
  protected BigInteger q;
  /** The public prime p. */
  protected BigInteger p;
  /** The generator g. */
  protected BigInteger g;
  /**
   * Identifier of the default encoding format to use when externalizing the key
   * material.
   */
  protected final int defaultFormat;
  /** String representation of this key. Cached for speed. */
  private transient String str;

  /**
   * Trivial protected constructor.
   *
   * @param defaultFormat the identifier of the encoding format to use by
   *          default when externalizing the key.
   * @param q a prime divisor of p-1.
   * @param p the public prime.
   * @param g the generator of the group.
   */
  protected GnuDHKey(int defaultFormat, BigInteger q, BigInteger p, BigInteger g)
  {
    super();

    this.defaultFormat = defaultFormat <= 0 ? Registry.RAW_ENCODING_ID
                                            : defaultFormat;
    this.q = q;
    this.p = p;
    this.g = g;
  }

  public DHParameterSpec getParams()
  {
    if (q == null)
      return new DHParameterSpec(p, g);
    return new DHParameterSpec(p, g, q.bitLength());
  }

  public String getAlgorithm()
  {
    return Registry.DH_KPG;
  }

  /** @deprecated see getEncoded(int). */
  public byte[] getEncoded()
  {
    return getEncoded(defaultFormat);
  }

  public String getFormat()
  {
    return FormatUtil.getEncodingShortName(defaultFormat);
  }

  public BigInteger getQ()
  {
    return q;
  }

  /**
   * Returns <code>true</code> if the designated object is an instance of
   * {@link DHKey} and has the same Diffie-Hellman parameter values as this one.
   *
   * @param obj the other non-null DH key to compare to.
   * @return <code>true</code> if the designated object is of the same type
   *         and value as this one.
   */
  public boolean equals(Object obj)
  {
    if (obj == null)
      return false;
    if (! (obj instanceof DHKey))
      return false;
    DHKey that = (DHKey) obj;
    return p.equals(that.getParams().getP())
           && g.equals(that.getParams().getG());
  }

  public String toString()
  {
    if (str == null)
      {
        String ls = (String) AccessController.doPrivileged
            (new GetPropertyAction("line.separator"));
        StringBuilder sb = new StringBuilder(ls)
            .append("defaultFormat=").append(defaultFormat).append(",").append(ls);
        if (q == null)
          sb.append("q=null,");
        else
          sb.append("q=0x").append(q.toString(16)).append(",");
        sb.append(ls).append("p=0x").append(p.toString(16)).append(",").append(ls)
            .append("g=0x").append(g.toString(16));
        str = sb.toString();
      }
    return str;
  }

  public abstract byte[] getEncoded(int format);
}
