/* X509CRLEntry.java -- entry in a X.509 CRL.
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


package gnu.java.security.x509;

import java.io.InputStream;
import java.io.IOException;

import java.math.BigInteger;

import java.security.cert.CRLException;

import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import gnu.java.io.ASN1ParsingException;
import gnu.java.security.OID;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;

/**
 * A single entry in a X.509 certificate revocation list.
 *
 * @see X509CRL
 * @author Casey Marshall
 */
class X509CRLEntry extends java.security.cert.X509CRLEntry
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  /** The DER encoded form of this CRL entry. */
  private byte[] encoded;

  /** The revoked certificate's serial number. */
  private BigInteger serialNo;

  /** The date the certificate was revoked. */
  private Date revocationDate;

  /** The encoded extensions. */
  private HashMap extensions;

  /** The set of critical extension OIDs. */
  private HashSet critOids;

  /** the set of non-critical extension OIDs. */
  private HashSet nonCritOids;

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Create a new X.509 certificate revocation list entry from the given
   * input stream and CRL version number.
   *
   * @param version The CRL version.
   * @param encoded The stream of DER bytes.
   * @throws CRLException If the ASN.1 structure is invalid.
   * @throws IOException  If the bytes cannot be read.
   */
  X509CRLEntry(int version, InputStream encoded)
    throws CRLException, IOException
  {
    super();
    extensions = new HashMap();
    critOids = new HashSet();
    nonCritOids = new HashSet();
    try
      {
        parse(version, encoded);
      }
    catch (IOException ioe)
      {
        throw ioe;
      }
    catch (Exception x)
      {
        throw new CRLException(x.toString());
      }
  }

  // X509CRLEntry methods.
  // ------------------------------------------------------------------------

  public boolean equals(Object o)
  {
    return ((X509CRLEntry) o).serialNo.equals(serialNo) &&
           ((X509CRLEntry) o).revocationDate.equals(revocationDate);
  }

  public int hashCode()
  {
    return serialNo.hashCode();
  }

  public byte[] getEncoded() throws CRLException
  {
    return (byte[]) encoded.clone();
  }

  public BigInteger getSerialNumber()
  {
    return serialNo;
  }

  public Date getRevocationDate()
  {
    return (Date) revocationDate.clone();
  }

  public boolean hasExtensions()
  {
    return ! extensions.isEmpty();
  }

  public String toString()
  {
    return "X509CRLEntry serial=" + serialNo + " revocation date="
      + revocationDate + " critExt=" + critOids + " ext=" + nonCritOids;
  }

  // X509Extension methods.
  // ------------------------------------------------------------------------

  public boolean hasUnsupportedCriticalExtension()
  {
    return false; // XXX
  }

  public Set getCriticalExtensionOIDs()
  {
    return Collections.unmodifiableSet(critOids);
  }

  public Set getNonCriticalExtensionOIDs()
  {
    return Collections.unmodifiableSet(nonCritOids);
  }

  public byte[] getExtensionValue(String oid)
  {
    byte[] ext = (byte[]) extensions.get(oid);
    if (ext != null)
      return (byte[]) ext.clone();
    return null;
  }

  // Own methods.
  // ------------------------------------------------------------------------

  private void parse(int version, InputStream in) throws Exception
  {
    DERReader der = new DERReader(in);
    DERValue entry = der.read();
    if (!entry.isConstructed())
      throw new ASN1ParsingException("malformed revokedCertificate");
    encoded = entry.getEncoded();
    int len = 0;
    DERValue val = der.read();
    serialNo = (BigInteger) val.getValue();
    len += DERWriter.definiteEncodingSize(val.getLength())
         + val.getLength() + 1;
    val = der.read();
    revocationDate = (Date) val.getValue();
    len += DERWriter.definiteEncodingSize(val.getLength())
         + val.getLength() + 1;

    if (len < entry.getLength())
      {
        if (version < 2)
          throw new ASN1ParsingException("extra data in CRL entry");
        while (len < entry.getLength())
          {
            val = der.read();
            if (!val.isConstructed())
              throw new ASN1ParsingException("malformed Extension");
            OID extOid = (OID) der.read().getValue();
            Boolean critical = Boolean.valueOf(false);
            DERValue val2 = der.read();
            if (val2.getValue() instanceof Boolean)
              {
                critical = (Boolean) val2.getValue();
                val2 = der.read();
              }
            byte[] ext = (byte[]) val2.getValue();
            extensions.put(extOid.toString(), ext);
            if (critical.booleanValue())
              critOids.add(extOid.toString());
            else
              nonCritOids.add(extOid.toString());
            len += val.getEncodedLength();
          }
      }
  }
}
