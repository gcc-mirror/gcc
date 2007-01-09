/* CertificateFactorySpi.java --- Certificate Factory Class
   Copyright (C) 1999,2003 Free Software Foundation, Inc.

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


package java.security.cert;

import java.io.InputStream;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
   CertificateFactorySpi is the abstract class Service Provider
   Interface (SPI) for the CertificateFactory class. A provider
   must implement all the abstract methods if they wish to 
   supply a certificate factory for a particular certificate
   type. Ex: X.509
   
   Certificate factories are used to generate certificates and
   certificate revocation lists (CRL) from their encoding.
   
   @since 1.2
   
   @author Mark Benvenuto
 */
public abstract class CertificateFactorySpi
{

  // Constructor.
  // ------------------------------------------------------------------------

  /**
   * Constructs a new CertificateFactorySpi
   */
  public CertificateFactorySpi()
  {}

  // Abstract methods.
  // ------------------------------------------------------------------------

  /**
     Generates a Certificate based on the encoded data read
     from the InputStream.

     The input stream must contain only one certificate.

     If there exists a specialized certificate class for the
     certificate format handled by the certificate factory
     then the return Ceritificate should be a typecast of it.
     Ex: A X.509 CertificateFactory should return X509Certificate.

     For X.509 certificates, the certificate in inStream must be
     DER encoded and supplied in binary or printable (Base64) 
     encoding. If the certificate is in Base64 encoding, it must be 
     bounded by -----BEGIN CERTIFICATE-----, and 
     -----END CERTIFICATE-----. 

     @param inStream an input stream containing the certificate data

     @return a certificate initialized with InputStream data.

     @throws CertificateException Certificate parsing error
  */
  public abstract Certificate engineGenerateCertificate(InputStream inStream)
    throws CertificateException;

  /**
     Returns a collection of certificates that were read from the 
     input stream. It may be empty, have only one, or have 
     multiple certificates.

     For a X.509 certificate factory, the stream may contain a
     single DER encoded certificate or a PKCS#7 certificate 
     chain. This is a PKCS#7 <I>SignedData</I> object with the 
     most significant field being <I>certificates</I>. If no 
     CRLs are present, then an empty collection is returned.
	
     @param inStream an input stream containing the certificates

     @return a collection of certificates initialized with 
     the InputStream data.

     @throws CertificateException Certificate parsing error
  */
  public abstract Collection<? extends Certificate> engineGenerateCertificates(InputStream inStream)
    throws CertificateException;

  /**
     Generates a CRL based on the encoded data read
     from the InputStream.

     The input stream must contain only one CRL.

     If there exists a specialized CRL class for the
     CRL format handled by the certificate factory
     then the return CRL should be a typecast of it.
     Ex: A X.509 CertificateFactory should return X509CRL.

     @param inStream an input stream containing the CRL data

     @return a CRL initialized with InputStream data.

     @throws CRLException CRL parsing error
  */
  public abstract CRL engineGenerateCRL(InputStream inStream)
    throws CRLException;

  /**
     Generates CRLs based on the encoded data read
     from the InputStream.

     For a X.509 certificate factory, the stream may contain a
     single DER encoded CRL or a PKCS#7 CRL set. This is a 
     PKCS#7 <I>SignedData</I> object with the most significant 
     field being <I>crls</I>. If no CRLs are present, then an
     empty collection is returned.

     @param inStream an input stream containing the CRLs

     @return a collection of CRLs initialized with 
     the InputStream data.

     @throws CRLException CRL parsing error
  */
  public abstract Collection<? extends CRL> engineGenerateCRLs(InputStream inStream)
    throws CRLException;

  // 1.4 instance methods.
  // ------------------------------------------------------------------------

  /**
   * Generate a {@link CertPath} and initialize it with data parsed from
   * the input stream. The default encoding of this factory is used.
   *
   * @param inStream The InputStream containing the CertPath data.
   * @return A CertPath initialized from the input stream data.
   * @throws CertificateException If an error occurs decoding the
   * CertPath.
   */
  public CertPath engineGenerateCertPath(InputStream inStream)
    throws CertificateException
  {
    throw new UnsupportedOperationException("not implemented");
  }

  /**
   * Generate a {@link CertPath} and initialize it with data parsed from
   * the input stream, using the specified encoding.
   *
   * @param inStream The InputStream containing the CertPath data.
   * @param encoding The encoding of the InputStream data.
   * @return A CertPath initialized from the input stream data.
   * @throws CertificateException If an error occurs decoding the
   *   CertPath.
   */
  public CertPath engineGenerateCertPath(InputStream inStream, String encoding)
    throws CertificateException
  {
    throw new UnsupportedOperationException("not implemented");
  }

  /**
   * Generate a {@link CertPath} and initialize it with the certificates
   * in the {@link java.util.List} argument.
   *
   * @param certificates The list of certificates with which to create
   *   the CertPath.
   * @return A CertPath initialized from the certificates.
   * @throws CertificateException If an error occurs generating the
   *   CertPath.
   */
  public CertPath engineGenerateCertPath(List<? extends Certificate> certificates)
    throws CertificateException
  {
    throw new UnsupportedOperationException("not implemented");
  }

  /**
   * Returns an Iterator of CertPath encodings supported by this
   * factory, with the default encoding first. The returned Iterator
   * cannot be modified.
   *
   * @return The Iterator of supported encodings.
   */
  public Iterator<String> engineGetCertPathEncodings()
  {
    throw new UnsupportedOperationException("not implemented");
  }
}

