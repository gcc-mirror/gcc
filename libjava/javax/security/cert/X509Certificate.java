/* X509Certificate.java -- base class of X.509 certificates.
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

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import java.math.BigInteger;

import java.security.Principal;
import java.security.cert.CertificateFactory;

import java.util.Date;

/**
 * <p>The base class of all X.509 certificates.</p>
 *
 * <p><b>This class is deprecated in favor of the {@link
 * java.security.cert.X509Certificate} class. It should not be used in new
 * applications.</b></p>
 */
public abstract class X509Certificate extends Certificate
{

  // Class methods.
  // -------------------------------------------------------------------------

  /**
   * <p>Get an instance of X509Certificate for the given encoded bytes.</p>
   *
   * @param encoded The encoded certificate.
   * @return An instance of X509Certificate.
   * @throws CertificateException If the encoded certificate cannot be parsed.
   */
  public static X509Certificate getInstance(byte[] encoded)
    throws CertificateException
  {
    return getInstance(new ByteArrayInputStream(encoded));
  }

  /**
   * <p>Get an instance of X509Certificate for the given encoded stream.</p>
   *
   * @param encoded The encoded certificate stream..
   * @return An instance of X509Certificate.
   * @throws CertificateException If the encoded certificate cannot be parsed.
   */
  public static X509Certificate getInstance(InputStream encoded)
    throws CertificateException
  {
    try
      {
        CertificateFactory cf = CertificateFactory.getInstance("X.509");
        return new X509CertBridge((java.security.cert.X509Certificate)
                                  cf.generateCertificate(encoded));
      }
    catch (java.security.cert.CertificateException ce)
      {
        throw new CertificateException(ce.getMessage());
      }
  }

  // Abstract methods.
  // -------------------------------------------------------------------------

  /**
   * <p>Check if this certificate is valid now.</p>
   *
   * @throws CertificateExpiredException If the certificate has expired.
   * @throws CertificateNotYetValidException If the certificate is not yet valid.
   * @see #checkValidity(java.util.Date)
   */
  public abstract void checkValidity()
    throws CertificateExpiredException, CertificateNotYetValidException;

  /**
   * <p>Check if this certificate is valid for the given date.</p>
   *
   * @param date The date to check.
   * @throws CertificateExpiredException If the certificate has expired.
   * @throws CertificateNotYetValidException If the certificate is not yet valid.
   */
  public abstract void checkValidity(Date date)
    throws CertificateExpiredException, CertificateNotYetValidException;

  /**
   * <p>Returns the X.509 version number.</p>
   *
   * @return The version number.
   */
  public abstract int getVersion();

  /**
   * <p>Returns this certificate's serial number.</p>
   *
   * @return The serial number.
   */
  public abstract BigInteger getSerialNumber();

  /**
   * <p>Returns the distinguished name of this certificate's issuer.</p>
   *
   * @return The issuer's distinguished name.
   */
  public abstract Principal getIssuerDN();

  /**
   * <p>Returns the distinguished name of this certificate's subject.</p>
   *
   * @return The subject's distinguished name.
   */
  public abstract Principal getSubjectDN();

  /**
   * <p>Returns the <i>not before</i> portion of this certificate's validity
   * period.</p>
   *
   * @return The not before date.
   */
  public abstract Date getNotBefore();

  /**
   * <p>Returns the <i>not after</i> portion of this certificate's validity
   * period.</p>
   *
   * @return The not after date.
   */
  public abstract Date getNotAfter();

  /**
   * <p>Returns the name of this certificate's signature algorithm.</p>
   *
   * @return The name of the signature algorithm.
   */
  public abstract String getSigAlgName();

  /**
   * <p>Returns the object identifier (OID) of this certificate's signature
   * algorithm. The returned string is a sequence of integers separated by
   * periods.</p>
   *
   * @return The signature OID.
   */
  public abstract String getSigAlgOID();

  /**
   * <p>Returns the signature parameters. The returned byte array contains the
   * raw DER-encoded parameters.</p>
   *
   * @return The signature parameters.
   */
  public abstract byte[] getSigAlgParams();
}
