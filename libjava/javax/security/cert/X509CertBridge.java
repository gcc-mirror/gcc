/* X509CertBridge.java -- bridge between JDK and JSSE cert APIs.
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.security.cert;

import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Principal;
import java.security.PublicKey;
import java.security.SignatureException;
import java.util.Date;

/**
 * <p>An implementation of the {@link X509Certificate} class that delegates
 * calls to a {@link java.security.cert.X509Certificate}.</p>
 */
final class X509CertBridge extends X509Certificate
{

  // Fields.
  // -------------------------------------------------------------------------

  private java.security.cert.X509Certificate cert;

  // Constructor.
  // -------------------------------------------------------------------------

  X509CertBridge(java.security.cert.X509Certificate cert)
  {
    this.cert = cert;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public byte[] getEncoded() throws CertificateEncodingException
  {
    try
      {
        return cert.getEncoded();
      }
    catch (java.security.cert.CertificateEncodingException cee)
      {
        throw new CertificateEncodingException(cee.getMessage());
      }
  }

  public void verify(PublicKey key)
    throws CertificateException, NoSuchAlgorithmException, InvalidKeyException,
           NoSuchProviderException, SignatureException
  {
    try
      {
        cert.verify(key);
      }
    catch (java.security.cert.CertificateException ce)
      {
        throw new CertificateException(ce.getMessage());
      }
  }

  public void verify(PublicKey key, String sigProvider)
    throws CertificateException, NoSuchAlgorithmException, InvalidKeyException,
           NoSuchProviderException, SignatureException
  {
    try
      {
        cert.verify(key, sigProvider);
      }
    catch (java.security.cert.CertificateException ce)
      {
        throw new CertificateException(ce.getMessage());
      }
  }

  public String toString()
  {
    return cert.toString();
  }

  public PublicKey getPublicKey()
  {
    return cert.getPublicKey();
  }

  public void checkValidity()
    throws CertificateExpiredException, CertificateNotYetValidException
  {
    try
      {
        cert.checkValidity();
      }
    catch (java.security.cert.CertificateExpiredException cee)
      {
        throw new CertificateExpiredException(cee.getMessage());
      }
    catch (java.security.cert.CertificateNotYetValidException cnyve)
      {
        throw new CertificateNotYetValidException(cnyve.getMessage());
      }
  }

  public void checkValidity(Date date)
    throws CertificateExpiredException, CertificateNotYetValidException
  {
    try
      {
        cert.checkValidity(date);
      }
    catch (java.security.cert.CertificateExpiredException cee)
      {
        throw new CertificateExpiredException(cee.getMessage());
      }
    catch (java.security.cert.CertificateNotYetValidException cnyve)
      {
        throw new CertificateNotYetValidException(cnyve.getMessage());
      }
  }

  public int getVersion()
  {
    return cert.getVersion();
  }

  public BigInteger getSerialNumber()
  {
    return cert.getSerialNumber();
  }

  public Principal getIssuerDN()
  {
    return cert.getIssuerDN();
  }

  public Principal getSubjectDN()
  {
    return cert.getSubjectDN();
  }

  public Date getNotBefore()
  {
    return cert.getNotBefore();
  }

  public Date getNotAfter()
  {
    return cert.getNotAfter();
  }

  public String getSigAlgName()
  {
    return cert.getSigAlgName();
  }

  public String getSigAlgOID()
  {
    return cert.getSigAlgOID();
  }

  public byte[] getSigAlgParams()
  {
    return cert.getSigAlgParams();
  }
}
