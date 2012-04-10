/* Extension.java -- an X.509 certificate or CRL extension.
   Copyright (C) 2004, 2006, 2010  Free Software Foundation, Inc.

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


package gnu.java.security.x509.ext;

import gnu.java.security.Configuration;
import gnu.java.security.OID;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.x509.Util;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

public class Extension
{
  private static final Logger log = Configuration.DEBUG ?
                        Logger.getLogger(Extension.class.getName()) : null;
  /**
   * This extension's object identifier.
   */
  protected final OID oid;

  /**
   * The criticality flag.
   */
  protected final boolean critical;

  /**
   * Whether or not this extension is locally supported.
   */
  protected boolean isSupported;

  /**
   * The extension value.
   */
  protected final Value value;

  /**
   * The DER encoded form.
   */
  protected byte[] encoded;

  // Constructors.
  // -------------------------------------------------------------------------

  public Extension(byte[] encoded) throws IOException
  {
    this.encoded = (byte[]) encoded.clone();
    DERReader der = new DERReader(encoded);

    // Extension ::= SEQUENCE {
    DERValue val = der.read();
    if (Configuration.DEBUG)
      log.fine("read val  tag == " + val.getTag() + " len == " + val.getLength());
    if (!val.isConstructed())
      throw new IOException("malformed Extension");

    //   extnID    OBJECT IDENTIFIER,
    val = der.read();
    if (val.getTag() != DER.OBJECT_IDENTIFIER)
      throw new IOException("expecting OBJECT IDENTIFIER");
    oid = (OID) val.getValue();
    if (Configuration.DEBUG)
      log.fine("read oid == " + oid);

    //   critical  BOOLEAN DEFAULT FALSE,
    val = der.read();
    if (val.getTag() == DER.BOOLEAN)
      {
        critical = ((Boolean) val.getValue()).booleanValue();
        val = der.read();
      }
    else
      critical = false;
    if (Configuration.DEBUG)
      log.fine("is critical == " + critical);

    //   extnValue OCTET STRING }
    if (val.getTag() != DER.OCTET_STRING)
      throw new IOException("expecting OCTET STRING");
    byte[] encval = (byte[]) val.getValue();
    isSupported = true;
    if (oid.equals(AuthorityKeyIdentifier.ID))
      {
        value = new AuthorityKeyIdentifier(encval);
      }
    else if (oid.equals(SubjectKeyIdentifier.ID))
      {
        value = new SubjectKeyIdentifier(encval);
      }
    else if (oid.equals(KeyUsage.ID))
      {
        value = new KeyUsage(encval);
      }
    else if (oid.equals(PrivateKeyUsagePeriod.ID))
      {
        value = new PrivateKeyUsagePeriod(encval);
      }
    else if (oid.equals(CertificatePolicies.ID))
      {
        value = new CertificatePolicies(encval);
      }
    else if (oid.equals (PolicyConstraint.ID))
      {
        value = new PolicyConstraint (encval);
      }
    else if (oid.equals(PolicyMappings.ID))
      {
        value = new PolicyMappings(encval);
      }
    else if (oid.equals(SubjectAlternativeNames.ID))
      {
        value = new SubjectAlternativeNames(encval);
      }
    else if (oid.equals(IssuerAlternativeNames.ID))
      {
        value = new IssuerAlternativeNames(encval);
      }
    else if (oid.equals(BasicConstraints.ID))
      {
        value = new BasicConstraints(encval);
      }
    else if (oid.equals(ExtendedKeyUsage.ID))
      {
        value = new ExtendedKeyUsage(encval);
      }
    else if (oid.equals(CRLNumber.ID))
      {
        value = new CRLNumber(encval);
      }
    else if (oid.equals(ReasonCode.ID))
      {
        value = new ReasonCode(encval);
      }
    else if (oid.equals(NameConstraints.ID))
      {
        value = new NameConstraints(encval);
      }
    else
      {
        value = new Value(encval);
        isSupported = false;
      }
    if (Configuration.DEBUG)
      log.fine("read value == " + value);
  }

  public Extension (final OID oid, final Value value, final boolean critical)
  {
    this.oid = oid;
    this.value = value;
    this.critical = critical;
    isSupported = true;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public OID getOid()
  {
    return oid;
  }

  public boolean isCritical()
  {
    return critical;
  }

  public boolean isSupported()
  {
    return isSupported;
  }

  public Value getValue()
  {
    return value;
  }

  public byte[] getEncoded()
  {
    if (encoded == null)
      encode();
    return (byte[]) encoded.clone();
  }

  public String toString()
  {
    return Extension.class.getName() + " [ id=" + oid + " critical=" +
      critical + " value=" + value + " ]";
  }

  public DERValue getDerValue()
  {
    List<DERValue> ext = new ArrayList<DERValue>(3);
    ext.add(new DERValue(DER.OBJECT_IDENTIFIER, oid));
    ext.add(new DERValue(DER.BOOLEAN, Boolean.valueOf(critical)));
    ext.add(new DERValue(DER.OCTET_STRING, value.getEncoded()));
    return new DERValue(DER.CONSTRUCTED|DER.SEQUENCE, ext);
  }

  // Own methods.
  // -------------------------------------------------------------------------

  private void encode()
  {
    encoded = getDerValue().getEncoded();
  }

  // Inner class.
  // -------------------------------------------------------------------------

  public static class Value
  {

    // Fields.
    // -----------------------------------------------------------------------

    protected byte[] encoded;

    // Constructor.
    // -----------------------------------------------------------------------

    public Value(byte[] encoded)
    {
      this.encoded = (byte[]) encoded.clone();
    }

    protected Value() { }

    // Instance methods.
    // -----------------------------------------------------------------------

    public byte[] getEncoded()
    {
      return (byte[]) encoded;
    }

    public int hashCode()
    {
      int result = 0;
      for (int i = 0; i < encoded.length; ++i)
        result = result * 31 + encoded[i];
      return result;
    }

    public boolean equals(Object o)
    {
      if (!(o instanceof Value))
        return false;
      return Arrays.equals(encoded, ((Value) o).encoded);
    }

    public String toString()
    {
      return Util.toHexString(encoded, ':');
    }
  }
}
