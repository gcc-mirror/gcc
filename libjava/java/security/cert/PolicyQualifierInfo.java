/* PolicyQualifierInfo.java -- policy qualifier info object.
   Copyright (C) 2003 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.security.cert;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import gnu.java.io.ASN1ParsingException;
import gnu.java.security.OID;
import gnu.java.security.der.DER;
import gnu.java.security.der.DEREncodingException;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;

/**
 * The PolicyQualifierInfo X.509 certificate extension.
 * PolicyQualifierInfo objects are represented by the ASN.1 structure:
 *
 * <pre>
 * PolicyQualifierInfo ::= SEQUENCE {
 *    policyQualifierId   PolicyQualifierId,
 *    qualifier           ANY DEFINED BY policyQualifierId
 * }
 *
 * PolicyQualifierId ::= OBJECT IDENTIFIER
 * </pre>
 *
 * @since JDK 1.4
 */
public final class PolicyQualifierInfo
{

  // Fields.
  // ------------------------------------------------------------------------

  /** The <code>policyQualifierId</code> field. */
  private OID oid;

  /** The DER encoded form of this object. */
  private byte[] encoded;

  /** The DER encoded form of the <code>qualifier</code> field. */
  private DERValue qualifier;

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Create a new PolicyQualifierInfo object from the DER encoded form
   * passed in the byte array. The argument is copied.
   *
   * <p>The ASN.1 form of PolicyQualifierInfo is:
<pre>
PolicyQualifierInfo ::= SEQUENCE {
   policyQualifierId     PolicyQualifierId,
   qualifier             ANY DEFINED BY policyQualifierId
}

PolicyQualifierId ::= OBJECT IDENTIFIER
</pre>
   *
   * @param encoded The DER encoded form.
   * @throws IOException If the structure cannot be parsed from the
   *         encoded bytes.
   */
  public PolicyQualifierInfo(byte[] encoded) throws IOException
  {
    if (encoded == null)
      throw new IOException("null bytes");
    this.encoded = (byte[]) encoded.clone();
    DERReader in = new DERReader(new ByteArrayInputStream(this.encoded));
    DERValue qualInfo = in.read();
    if (!qualInfo.isConstructed())
      throw new ASN1ParsingException("malformed PolicyQualifierInfo");
    DERValue val = in.read();
    if (!(val.getValue() instanceof OID))
      throw new ASN1ParsingException("value read not an OBJECT IDENTIFIER");
    oid = (OID) val.getValue();
    if (val.getEncodedLength() < val.getLength())
      qualifier = in.read();
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Returns the <code>policyQualifierId</code> field of this structure,
   * as a dotted-decimal representation of the object identifier.
   *
   * @return This structure's OID field.
   */
  public String getPolicyQualifierId()
  {
    return oid.toString();
  }

  /**
   * Returns the DER encoded form of this object; the contents of the
   * returned byte array are equivalent to those that were passed to the
   * constructor. The byte array is cloned every time this method is
   * called.
   *
   * @return The encoded form.
   */
  public byte[] getEncoded()
  {
    return (byte[]) encoded.clone();
  }

  /**
   * Get the <code>qualifier</code> field of this object, as a DER
   * encoded byte array. The byte array returned is cloned every time
   * this method is called.
   *
   * @return The encoded qualifier.
   */
  public byte[] getPolicyQualifier()
  {
    if (qualifier == null)
      return new byte[0];
    return qualifier.getEncoded();
  }

  /**
   * Returns a printable string representation of this object.
   *
   * @return The string representation.
   */
  public String toString()
  {
    return "PolicyQualifierInfo { policyQualifierId ::= " + oid
      + ", qualifier ::= " + qualifier + " }";
  }
}
