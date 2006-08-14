/* DSSKey.java -- 
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


package gnu.java.security.key.dss;

import gnu.java.security.Registry;
import gnu.java.security.action.GetPropertyAction;
import gnu.java.security.util.FormatUtil;

import java.math.BigInteger;
import java.security.AccessController;
import java.security.Key;
import java.security.interfaces.DSAKey;
import java.security.interfaces.DSAParams;
import java.security.spec.DSAParameterSpec;

/**
 * A base asbtract class for both public and private DSS (Digital Signature
 * Standard) keys. It encapsulates the three DSS numbers: <code>p</code>,
 * <code>q</code> and <code>g</code>.
 * <p>
 * According to the JDK, cryptographic <i>Keys</i> all have a <i>format</i>.
 * The format used in this implementation is called <i>Raw</i>, and basically
 * consists of the raw byte sequences of algorithm parameters. The exact order
 * of the byte sequences and the implementation details are given in each of the
 * relevant <code>getEncoded()</code> methods of each of the private and
 * public keys.
 * <p>
 * <b>IMPORTANT</b>: Under certain circumstances (e.g. in an X.509 certificate
 * with inherited AlgorithmIdentifier's parameters of a SubjectPublicKeyInfo
 * element) these three MPIs may be <code>null</code>.
 * 
 * @see DSSPrivateKey#getEncoded
 * @see DSSPublicKey#getEncoded
 */
public abstract class DSSKey
    implements Key, DSAKey
{
  /**
   * A prime modulus, where
   * <code>2<sup>L-1</sup> &lt; p &lt; 2<sup>L</sup></code> for
   * <code>512 &lt;= L &lt;= 1024</code> and <code>L</code> a multiple of
   * <code>64</code>.
   */
  protected final BigInteger p;

  /**
   * A prime divisor of <code>p - 1</code>, where
   * <code>2<sup>159</sup> &lt; q
   * &lt; 2<sup>160</sup></code>.
   */
  protected final BigInteger q;

  /**
   * <code>g = h<sup>(p-1)</sup>/q mod p</code>, where <code>h</code> is
   * any integer with <code>1 &lt; h &lt; p - 1</code> such that <code>h<sup>
   * (p-1)</sup>/q mod p > 1</code> (<code>g</code>
   * has order <code>q mod p
   * </code>).
   */
  protected final BigInteger g;

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
   * @param p the DSS parameter <code>p</code>.
   * @param q the DSS parameter <code>q</code>.
   * @param g the DSS parameter <code>g</code>.
   */
  protected DSSKey(int defaultFormat, BigInteger p, BigInteger q, BigInteger g)
  {
    super();

    this.defaultFormat = defaultFormat <= 0 ? Registry.RAW_ENCODING_ID
                                            : defaultFormat;
    this.p = p;
    this.q = q;
    this.g = g;
  }

  public DSAParams getParams()
  {
    return new DSAParameterSpec(p, q, g);
  }

  public String getAlgorithm()
  {
    return Registry.DSS_KPG;
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
   * Returns <code>true</code> if the designated object is an instance of
   * {@link DSAKey} and has the same DSS (Digital Signature Standard) parameter
   * values as this one.
   * <p>
   * Always returns <code>false</code> if the MPIs of this key are
   * <i>inherited</i>. This may be the case when the key is re-constructed from
   * an X.509 certificate with absent or NULL AlgorithmIdentifier's parameters
   * field.
   * 
   * @param obj the other non-null DSS key to compare to.
   * @return <code>true</code> if the designated object is of the same type
   *         and value as this one.
   */
  public boolean equals(Object obj)
  {
    if (hasInheritedParameters())
      return false;

    if (obj == null)
      return false;

    if (! (obj instanceof DSAKey))
      return false;

    DSAKey that = (DSAKey) obj;
    return p.equals(that.getParams().getP())
           && q.equals(that.getParams().getQ())
           && g.equals(that.getParams().getG());
  }

  public String toString()
  {
    if (str == null)
      {
        String ls = (String) AccessController.doPrivileged(new GetPropertyAction("line.separator"));
        StringBuilder sb = new StringBuilder(ls)
            .append("defaultFormat=").append(defaultFormat).append(",")
            .append(ls);
        if (hasInheritedParameters())
          sb.append("p=inherited,").append(ls)
              .append("q=inherited,").append(ls)
              .append("g=inherited");
        else
          sb.append("p=0x").append(p.toString(16)).append(",").append(ls)
              .append("q=0x").append(q.toString(16)).append(",").append(ls)
              .append("g=0x").append(g.toString(16));
        str = sb.toString();
      }
    return str;
  }

  public abstract byte[] getEncoded(int format);

  /**
   * @return <code>true</code> if <code>p</code>, <code>q</code> and
   *         <code>g</code> are all <code>null</code>. Returns
   *         <code>false</code> otherwise.
   */
  public boolean hasInheritedParameters()
  {
    return p == null && q == null && g == null;
  }
}
