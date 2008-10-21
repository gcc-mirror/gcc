/* GnuRSAKey.java -- 
   Copyright 2001, 2002, 2003, 2006 Free Software Foundation, Inc.

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


package gnu.java.security.key.rsa;

import gnu.java.lang.CPStringBuilder;

import gnu.java.security.Registry;
import gnu.java.security.action.GetPropertyAction;
import gnu.java.security.util.FormatUtil;

import java.math.BigInteger;
import java.security.AccessController;
import java.security.Key;
import java.security.interfaces.RSAKey;

/**
 * A base asbtract class for both public and private RSA keys.
 */
public abstract class GnuRSAKey
    implements Key, RSAKey
{
  /** The public modulus of an RSA key pair. */
  private final BigInteger n;

  /** The public exponent of an RSA key pair. */
  private final BigInteger e;

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
   * @param n the public modulus <code>n</code>.
   * @param e the public exponent <code>e</code>.
   */
  protected GnuRSAKey(int defaultFormat, BigInteger n, BigInteger e)
  {
    super();

    this.defaultFormat = defaultFormat <= 0 ? Registry.RAW_ENCODING_ID
                                            : defaultFormat;
    this.n = n;
    this.e = e;
  }

  public BigInteger getModulus()
  {
    return getN();
  }

  public String getAlgorithm()
  {
    return Registry.RSA_KPG;
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

  /**
   * Returns the modulus <code>n</code>.
   * 
   * @return the modulus <code>n</code>.
   */
  public BigInteger getN()
  {
    return n;
  }

  /**
   * Returns the public exponent <code>e</code>.
   * 
   * @return the public exponent <code>e</code>.
   */
  public BigInteger getPublicExponent()
  {
    return getE();
  }

  /**
   * Same as {@link #getPublicExponent()}.
   * 
   * @return the public exponent <code>e</code>.
   */
  public BigInteger getE()
  {
    return e;
  }

  /**
   * Returns <code>true</code> if the designated object is an instance of
   * {@link RSAKey} and has the same RSA parameter values as this one.
   * 
   * @param obj the other non-null RSA key to compare to.
   * @return <code>true</code> if the designated object is of the same type
   *         and value as this one.
   */
  public boolean equals(final Object obj)
  {
    if (obj == null)
      return false;

    if (! (obj instanceof RSAKey))
      return false;

    final RSAKey that = (RSAKey) obj;
    return n.equals(that.getModulus());
  }

  public String toString()
  {
    if (str == null)
      {
        String ls = (String) AccessController.doPrivileged
            (new GetPropertyAction("line.separator"));
        str = new CPStringBuilder(ls)
            .append("defaultFormat=").append(defaultFormat).append(",").append(ls)
            .append("n=0x").append(n.toString(16)).append(",").append(ls)
            .append("e=0x").append(e.toString(16))
            .toString();
      }
    return str;
  }

  public abstract byte[] getEncoded(int format);
}
