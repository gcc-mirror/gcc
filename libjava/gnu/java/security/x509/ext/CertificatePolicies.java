/* CertificatePolicies.java -- certificate policy extension.
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package gnu.java.security.x509.ext;

import gnu.java.security.OID;
import gnu.java.security.der.DER;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;

import java.io.IOException;
import java.security.cert.PolicyQualifierInfo;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class CertificatePolicies extends Extension.Value
{

  // Constants and fields.
  // -------------------------------------------------------------------------

  public static final OID ID = new OID("2.5.29.32");

  private final List policies;
  private final Map policyQualifierInfos;

  // Constructor.
  // -------------------------------------------------------------------------

  public CertificatePolicies(final byte[] encoded) throws IOException
  {
    super(encoded);
    DERReader der = new DERReader(encoded);
    DERValue pol = der.read();
    if (!pol.isConstructed())
      throw new IOException("malformed CertificatePolicies");

    int len = 0;
    LinkedList policyList = new LinkedList();
    HashMap qualifierMap = new HashMap();
    while (len < pol.getLength())
      {
        DERValue policyInfo = der.read();
        if (!policyInfo.isConstructed())
          throw new IOException("malformed PolicyInformation");
        DERValue val = der.read();
        if (val.getTag() != DER.OBJECT_IDENTIFIER)
          throw new IOException("malformed CertPolicyId");
        OID policyId = (OID) val.getValue();
        policyList.add(policyId);
        if (val.getEncodedLength() < policyInfo.getLength())
          {
            DERValue qual = der.read();
            int len2 = 0;
            LinkedList quals = new LinkedList();
            while (len2 < qual.getLength())
              {
                val = der.read();
                quals.add(new PolicyQualifierInfo(val.getEncoded()));
                der.skip(val.getLength());
                len2 += val.getEncodedLength();
              }
            qualifierMap.put(policyId, quals);
          }
        len += policyInfo.getEncodedLength();
      }

    policies = Collections.unmodifiableList(policyList);
    policyQualifierInfos = Collections.unmodifiableMap(qualifierMap);
  }

  public CertificatePolicies (final List policies,
                              final Map policyQualifierInfos)
  {
    for (Iterator it = policies.iterator(); it.hasNext(); )
      if (!(it.next() instanceof OID))
        throw new IllegalArgumentException ("policies must be OIDs");
    for (Iterator it = policyQualifierInfos.entrySet().iterator(); it.hasNext();)
      {
        Map.Entry e = (Map.Entry) it.next();
        if (!(e.getKey() instanceof OID) || !policies.contains (e.getKey()))
          throw new IllegalArgumentException
            ("policyQualifierInfos keys must be OIDs");
        if (!(e.getValue() instanceof List))
          throw new IllegalArgumentException
            ("policyQualifierInfos values must be Lists of PolicyQualifierInfos");
        for (Iterator it2 = ((List) e.getValue()).iterator(); it.hasNext(); )
          if (!(it2.next() instanceof PolicyQualifierInfo))
            throw new IllegalArgumentException
              ("policyQualifierInfos values must be Lists of PolicyQualifierInfos");
      }
    this.policies = Collections.unmodifiableList (new ArrayList (policies));
    this.policyQualifierInfos = Collections.unmodifiableMap
      (new HashMap (policyQualifierInfos));
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public List getPolicies()
  {
    return policies;
  }

  public List getPolicyQualifierInfos(OID oid)
  {
    return (List) policyQualifierInfos.get(oid);
  }

  public byte[] getEncoded()
  {
    if (encoded == null)
      {
        List pol = new ArrayList (policies.size());
        for (Iterator it = policies.iterator(); it.hasNext(); )
          {
            OID policy = (OID) it.next();
            List qualifiers = getPolicyQualifierInfos (policy);
            List l = new ArrayList (qualifiers == null ? 1 : 2);
            l.add (new DERValue (DER.OBJECT_IDENTIFIER, policy));
            if (qualifiers != null)
              {
                List ll = new ArrayList (qualifiers.size());
                for (Iterator it2 = qualifiers.iterator(); it.hasNext(); )
                  {
                    PolicyQualifierInfo info = (PolicyQualifierInfo) it2.next();
                    try
                      {
                        ll.add (DERReader.read (info.getEncoded()));
                      }
                    catch (IOException ioe)
                      {
                      }
                  }
                l.add (new DERValue (DER.CONSTRUCTED|DER.SEQUENCE, ll));
              }
            pol.add (new DERValue (DER.CONSTRUCTED|DER.SEQUENCE, l));
          }
        encoded = new DERValue (DER.CONSTRUCTED|DER.SEQUENCE, pol).getEncoded();
      }
    return (byte[]) encoded.clone();
  }

  public String toString()
  {
    return CertificatePolicies.class.getName() + " [ policies=" + policies +
      " policyQualifierInfos=" + policyQualifierInfos + " ]";
  }
}
