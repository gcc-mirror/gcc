/* X509CRLSelectorImpl.java -- implementation of an X509CRLSelector.
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


package gnu.java.security.x509;

import java.io.IOException;

import java.security.Principal;
import java.security.cert.CRL;
import java.security.cert.CRLSelector;
import java.security.cert.X509CRL;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.security.auth.x500.X500Principal;

/**
 * Sun's implementation of X509CRLSelector sucks. This one tries to work
 * better.
 */
public class X509CRLSelectorImpl implements CRLSelector
{

  // Fields.
  // -------------------------------------------------------------------------

  private Set issuerNames;

  // Constructor.
  // -------------------------------------------------------------------------

  public X509CRLSelectorImpl()
  {
    issuerNames = new HashSet();
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void addIssuerName(byte[] issuerName) throws IOException
  {
    issuerNames.add(new X500DistinguishedName(issuerName));
  }

  public void addIssuerName(String issuerName)
  {
    issuerNames.add(new X500DistinguishedName(issuerName));
  }

  public void addIssuerName(Principal issuerName) throws IOException
  {
    if (issuerName instanceof X500DistinguishedName)
      issuerNames.add(issuerName);
    else if (issuerName instanceof X500Principal)
      issuerNames.add(new X500DistinguishedName(((X500Principal) issuerName).getEncoded()));
    else
      issuerNames.add(new X500DistinguishedName(issuerName.getName()));
  }

  public Collection getIssuerNames()
  {
    return Collections.unmodifiableSet(issuerNames);
  }

  public Object clone()
  {
    X509CRLSelectorImpl copy = new X509CRLSelectorImpl();
    copy.issuerNames.addAll(issuerNames);
    return copy;
  }

  public boolean match(CRL crl)
  {
    if (!(crl instanceof X509CRL))
      return false;
    try
      {
        Principal p = ((X509CRL) crl).getIssuerDN();
        X500DistinguishedName thisName = null;
        if (p instanceof X500DistinguishedName)
          thisName = (X500DistinguishedName) p;
        else if (p instanceof X500Principal)
          thisName = new X500DistinguishedName(((X500Principal) p).getEncoded());
        else
          thisName = new X500DistinguishedName(p.getName());
        for (Iterator it = issuerNames.iterator(); it.hasNext(); )
          {
            X500DistinguishedName name = (X500DistinguishedName) it.next();
            if (thisName.equals(name))
              return true;
          }
      }
    catch (Exception x)
      {
      }
    return false;
  }
}

