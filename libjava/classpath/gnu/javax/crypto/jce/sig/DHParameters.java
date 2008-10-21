/* DHParameters.java -- DH parameters DAO
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


package gnu.javax.crypto.jce.sig;

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
import java.security.spec.InvalidParameterSpecException;
import java.util.ArrayList;

import javax.crypto.spec.DHGenParameterSpec;
import javax.crypto.spec.DHParameterSpec;

/**
 * A JCE-specific Data Access Object (DAO) for DH parameters.
 */
public class DHParameters
    extends AlgorithmParametersSpi
{
  /** The prime public modulus. */
  private BigInteger p;

  /** The generator. */
  private BigInteger g;

  /** A prime factor of p-1. */
  private BigInteger q;

  /** The (private) random exponent's size (in bits). */
  private int l;

  // default 0-arguments constructor

  protected void engineInit(AlgorithmParameterSpec spec)
      throws InvalidParameterSpecException
  {
    if (! (spec instanceof DHParameterSpec))
      throw new InvalidParameterSpecException("Wrong AlgorithmParameterSpec type: "
                                              + spec.getClass().getName());
    DHParameterSpec dhSpec = (DHParameterSpec) spec;
    p = dhSpec.getP();
    g = dhSpec.getG();
    l = dhSpec.getL();
  }

  /**
   * Decodes the set of DH parameters as per RFC-2459; i.e. the DER-encoded
   * form of the following ASN.1 construct: 
   * 
   * <pre>
   *   DhParams ::= SEQUENCE {
   *     p  INTEGER, -- odd prime, p=jq +1
   *     g  INTEGER, -- generator, g
   *     q  INTEGER  -- factor of p-1
   *   }
   * </pre>
   */
  protected void engineInit(byte[] params) throws IOException
  {
    DERReader der = new DERReader(params);

    DERValue derParams = der.read();
    DerUtil.checkIsConstructed(derParams, "Wrong DH Parameters field");

    DERValue val = der.read();
    DerUtil.checkIsBigInteger(val, "Wrong P field");
    p = (BigInteger) val.getValue();
    val = der.read();
    DerUtil.checkIsBigInteger(val, "Wrong G field");
    g = (BigInteger) val.getValue();
    val = der.read();
    DerUtil.checkIsBigInteger(val, "Wrong Q field");
    q = (BigInteger) val.getValue();
    l = q.bitLength();
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
    if (paramSpec.isAssignableFrom(DHParameterSpec.class))
      return new DHParameterSpec(p, g, l);

    if (paramSpec.isAssignableFrom(DHGenParameterSpec.class))
      return new DHGenParameterSpec(p.bitLength(), l);

    throw new InvalidParameterSpecException("Wrong AlgorithmParameterSpec type: "
                                            + paramSpec.getName());
  }

  /**
   * Encodes the set of DH parameters as per RFC-2459; i.e. as the DER-encoded
   * form of the following ASN.1 construct: 
   * 
   * <pre>
   *   DhParams ::= SEQUENCE {
   *     p  INTEGER, -- odd prime, p=jq +1
   *     g  INTEGER, -- generator, g
   *     q  INTEGER  -- factor of p-1
   *   }
   * </pre>
   */
  protected byte[] engineGetEncoded() throws IOException
  {
    DERValue derP = new DERValue(DER.INTEGER, p);
    DERValue derG = new DERValue(DER.INTEGER, g);
    DERValue derQ = new DERValue(DER.INTEGER, q);

    ArrayList params = new ArrayList(3);
    params.add(derP);
    params.add(derG);
    params.add(derQ);
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

    sb.append(", g=");
    if (g == null)
      sb.append("???");
    else
      sb.append("0x").append(g.toString(16));

    sb.append(", q=");
    if (q == null)
      sb.append("???");
    else
      sb.append("0x").append(q.toString(16));

    sb.append(", l=").append(l);

    return sb.toString();
  }
}
