/* DSSParameters.java -- DSS parameters DAO
   Copyright (C) 2006 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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
exception statement from your version. */


package gnu.java.security.jce.sig;

import gnu.java.lang.CPStringBuilder;

import gnu.java.security.Registry;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;
import gnu.java.security.util.DerUtil;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.security.AlgorithmParametersSpi;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.DSAParameterSpec;
import java.security.spec.InvalidParameterSpecException;
import java.util.ArrayList;

/**
 * A JCE-specific Data Access Object (DAO) for DSS parameters.
 */
public class DSSParameters
    extends AlgorithmParametersSpi
{
  /**
   * A prime modulus, where <code>2<sup>L-1</sup> &lt; p &lt; 2<sup>L</sup></code>
   * for <code>512 &lt;= L &lt;= 1024</code> and <code>L</code> a multiple of
   * <code>64</code>.
   */
  private BigInteger p;

  /**
   * A prime divisor of <code>p - 1</code>, where <code>2<sup>159</sup> &lt; q
   * &lt; 2<sup>160</sup></code>.
   */
  private BigInteger q;

  /**
   * <code>g = h<sup>(p-1)</sup>/q mod p</code>, where <code>h</code> is any
   * integer with <code>1 &lt; h &lt; p - 1</code> such that <code>h<sup>
   * (p-1)</sup>/q mod p > 1</code> (<code>g</code> has order <code>q mod p
   * </code>).
   */
  private BigInteger g;

  // default 0-arguments constructor

  protected void engineInit(AlgorithmParameterSpec spec)
      throws InvalidParameterSpecException
  {
    if (! (spec instanceof DSAParameterSpec))
      throw new InvalidParameterSpecException("Wrong AlgorithmParameterSpec type: "
                                              + spec.getClass().getName());
    DSAParameterSpec dsaSpec = (DSAParameterSpec) spec;
    p = dsaSpec.getP();
    q = dsaSpec.getQ();
    g = dsaSpec.getG();
  }

  /**
   * Decodes the set of DSS parameters as per RFC-2459; i.e. the DER-encoded
   * form of the following ASN.1 construct:
   *
   * <pre>
   *   DssParams ::= SEQUENCE {
   *     p   INTEGER,
   *     q   INTEGER,
   *     g   INTEGER
   *   }
   * </pre>
   */
  protected void engineInit(byte[] params) throws IOException
  {
    DERReader der = new DERReader(params);

    DERValue derParams = der.read();
    DerUtil.checkIsConstructed(derParams, "Wrong DSS Parameters field");

    DERValue val = der.read();
    DerUtil.checkIsBigInteger(val, "Wrong P field");
    p = (BigInteger) val.getValue();
    val = der.read();
    DerUtil.checkIsBigInteger(val, "Wrong Q field");
    q = (BigInteger) val.getValue();
    val = der.read();
    DerUtil.checkIsBigInteger(val, "Wrong G field");
    g = (BigInteger) val.getValue();
  }

  protected void engineInit(byte[] params, String format) throws IOException
  {
    if (format != null)
      {
        format = format.trim();
        if (format.length() == 0)
          throw new IOException("Format MUST NOT be an empty string");

        if (! format.equalsIgnoreCase(Registry.ASN1_ENCODING_SHORT_NAME))
          throw new IOException("Unknown or unsupported format: " + format);
      }
    engineInit(params);
  }

  protected AlgorithmParameterSpec engineGetParameterSpec(Class paramSpec)
      throws InvalidParameterSpecException
  {
    if (! paramSpec.isAssignableFrom(DSAParameterSpec.class))
      throw new InvalidParameterSpecException("Wrong AlgorithmParameterSpec type: "
                                              + paramSpec.getName());
    return new DSAParameterSpec(p, q, g);
  }

  /**
   * Encodes the set of DSS parameters as per RFC-2459; i.e. as the DER-encoded
   * form of the following ASN.1 construct:
   *
   * <pre>
   *   DssParams ::= SEQUENCE {
   *     p   INTEGER,
   *     q   INTEGER,
   *     g   INTEGER
   *   }
   * </pre>
   */
  protected byte[] engineGetEncoded() throws IOException
  {
    DERValue derP = new DERValue(DER.INTEGER, p);
    DERValue derQ = new DERValue(DER.INTEGER, q);
    DERValue derG = new DERValue(DER.INTEGER, g);

    ArrayList params = new ArrayList(3);
    params.add(derP);
    params.add(derQ);
    params.add(derG);
    DERValue derParams = new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, params);

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DERWriter.write(baos, derParams);
    byte[] result = baos.toByteArray();

    return result;
  }

  protected byte[] engineGetEncoded(String format) throws IOException
  {
    if (format != null)
      {
        format = format.trim();
        if (format.length() == 0)
          throw new IOException("Format MUST NOT be an empty string");

        if (! format.equalsIgnoreCase(Registry.ASN1_ENCODING_SHORT_NAME))
          throw new IOException("Unknown or unsupported format: " + format);
      }
    return engineGetEncoded();
  }

  protected String engineToString()
  {
    CPStringBuilder sb = new CPStringBuilder("p=");
    if (p == null)
      sb.append("???");
    else
      sb.append("0x").append(p.toString(16));

    sb.append(", q=");
    if (q == null)
      sb.append("???");
    else
      sb.append("0x").append(q.toString(16));

    sb.append(", g=");
    if (g == null)
      sb.append("???");
    else
      sb.append("0x").append(g.toString(16));

    return sb.toString();
  }
}
