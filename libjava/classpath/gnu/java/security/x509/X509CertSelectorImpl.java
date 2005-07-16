/* X509CertSelectorImpl.java -- implementation of an X509CertSelector.
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


package gnu.java.security.x509;

import java.io.IOException;
import java.security.Principal;
import java.security.cert.CertSelector;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.security.auth.x500.X500Principal;

/**
 * Sun's implementation of X509CertSelector sucks. This one tries to work
 * better.
 */
public class X509CertSelectorImpl implements CertSelector
{

  // Fields.
  // -------------------------------------------------------------------------

  private Set issuerNames;
  private Set subjectNames;

  // Constructor.
  // -------------------------------------------------------------------------

  public X509CertSelectorImpl()
  {
    issuerNames = new HashSet();
    subjectNames = new HashSet();
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

  public void addSubjectName(byte[] subjectName) throws IOException
  {
    subjectNames.add(new X500DistinguishedName(subjectName));
  }

  public void addSubjectName(String subjectName) throws IOException
  {
    subjectNames.add(new X500DistinguishedName(subjectName));
  }

  public void addSubjectName(Principal subjectName) throws IOException
  {
    if (subjectName instanceof X500DistinguishedName)
      subjectNames.add(subjectName);
    else if (subjectName instanceof X500Principal)
      subjectNames.add(new X500DistinguishedName(((X500Principal) subjectName).getEncoded()));
    else
      subjectNames.add(new X500DistinguishedName(subjectName.getName()));
  }

  public Collection getSubjectNames()
  {
    return Collections.unmodifiableSet(subjectNames);
  }

  public Object clone()
  {
    X509CertSelectorImpl copy = new X509CertSelectorImpl();
    copy.issuerNames.addAll(issuerNames);
    copy.subjectNames.addAll(subjectNames);
    return copy;
  }

  public boolean match(Certificate cert)
  {
    if (!(cert instanceof X509Certificate))
      return false;
    boolean matchIssuer = false;
    boolean matchSubject = false;
    try
      {
        Principal p = ((X509Certificate) cert).getIssuerDN();
        X500DistinguishedName thisName = null;
        if (p instanceof X500DistinguishedName)
          thisName = (X500DistinguishedName) p;
        else if (p instanceof X500Principal)
          thisName = new X500DistinguishedName(((X500Principal) p).getEncoded());
        else
          thisName = new X500DistinguishedName(p.getName());
        if (issuerNames.isEmpty())
          matchIssuer = true;
        else
          {
            for (Iterator it = issuerNames.iterator(); it.hasNext(); )
              {
                X500DistinguishedName name = (X500DistinguishedName) it.next();
                if (thisName.equals(name))
                  {
                    matchIssuer = true;
                    break;
                  }
              }
          }

        p = ((X509Certificate) cert).getSubjectDN();
        thisName = null;
        if (p instanceof X500DistinguishedName)
          thisName = (X500DistinguishedName) p;
        else if (p instanceof X500Principal)
          thisName = new X500DistinguishedName(((X500Principal) p).getEncoded());
        else
          thisName = new X500DistinguishedName(p.getName());
        if (subjectNames.isEmpty())
          matchSubject = true;
        else
          {
            for (Iterator it = subjectNames.iterator(); it.hasNext(); )
              {
                X500DistinguishedName name = (X500DistinguishedName) it.next();
                if (thisName.equals(name))
                  {
                    matchSubject = true;
                    break;
                  }
              }
          }
      }
    catch (Exception x)
      {
      }
    return matchIssuer && matchSubject;
  }
}

