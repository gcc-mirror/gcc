/* CertificateFactory.java -- Certificate Factory Class
   Copyright (C) 1999, 2002, 2003, 2004  Free Software Foundation, Inc.

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

import gnu.java.security.Engine;

import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Provider;
import java.security.Security;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 * This class implements the CertificateFactory class interface used to
 * generate certificates, certificate revocation lists (CRLs), and certificate
 * paths objects from their encoded forms.
 *
 * @author Mark Benvenuto
 * @author Casey Marshall
 * @since 1.2
 * @status Fully compatible with JDK 1.4.
 */
public class CertificateFactory
{

  /** The service name for certificate factories. */
  private static final String CERTIFICATE_FACTORY = "CertificateFactory";

  private CertificateFactorySpi certFacSpi;
  private Provider provider;
  private String type;

  /**
   * Creates an instance of CertificateFactory.
   *
   * @param certFacSpi The underlying CertificateFactory engine.
   * @param provider   The provider of this implementation.
   * @param type       The type of Certificate this factory creates.
   */
  protected CertificateFactory(CertificateFactorySpi certFacSpi,
                               Provider provider, String type)
  {
    this.certFacSpi = certFacSpi;
    this.provider = provider;
    this.type = type;
  }

  /**
   * Returns an instance of a <code>CertificateFactory</code> representing the
   * specified certificate factory type.
   * 
   * @param type The type of certificate factory to create.
   * @return A <code>CertificateFactory</code> of the desired type.
   * @throws CertificateException If the type of certificate factory is not
   *           implemented by any installed provider.
   * @throws IllegalArgumentException if <code>type</code> is
   *           <code>null</code> or is an empty string.
   */
  public static final CertificateFactory getInstance(String type)
      throws CertificateException
  {
    Provider[] p = Security.getProviders();
    CertificateException lastException = null;
    for (int i = 0; i < p.length; i++)
      try
        {
          return getInstance(type, p[i]);
        }
      catch (CertificateException x)
        {
          lastException = x;
        }
    if (lastException != null)
      throw lastException;
    throw new CertificateException(type);
  }

  /**
   * Returns an instance of a <code>CertificateFactory</code> representing the
   * specified certificate factory type from the named provider.
   * 
   * @param type The type of certificate factory to create.
   * @param provider The name of the provider to use.
   * @return A <code>CertificateFactory</code> for the desired type.
   * @throws CertificateException If the type of certificate is not implemented
   *           by the named provider.
   * @throws NoSuchProviderException If the named provider is not installed.
   * @throws IllegalArgumentException if either <code>type</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>type</code> is an empty string.
   */
  public static final CertificateFactory getInstance(String type,
                                                     String provider) 
    throws CertificateException, NoSuchProviderException
  {
    if (provider == null)
      throw new IllegalArgumentException("provider MUST NOT be null");
    Provider p = Security.getProvider(provider);
    if (p == null)
      throw new NoSuchProviderException(provider);
    return getInstance(type, p);
  }

  /**
   * Returns an instance of a <code>CertificateFactory</code> representing the
   * specified certificate factory type from the designated provider.
   * 
   * @param type The type of certificate factory to create.
   * @param provider The provider from which to get the implementation.
   * @return A <code>CertificateFactory</code> for the desired type.
   * @throws CertificateException If the type of certificate is not implemented
   *           by the provider.
   * @throws IllegalArgumentException if either <code>type</code> or
   *           <code>provider</code> is <code>null</code>, or if
   *           <code>type</code> is an empty string.
   */
  public static final CertificateFactory getInstance(String type,
                                                     Provider provider)
      throws CertificateException
  {
    Throwable cause;
    try
      {
        Object spi = Engine.getInstance(CERTIFICATE_FACTORY, type, provider);
        return new CertificateFactory((CertificateFactorySpi) spi, provider, type);
      }
    catch (ClassCastException x)
      {
        cause = x;
      }
    catch (InvocationTargetException x)
      {
        cause = x.getCause() != null ? x.getCause() : x;
      }
    catch (NoSuchAlgorithmException x)
      {
        cause = x;
      }
    CertificateException x = new CertificateException(type);
    x.initCause(cause);
    throw x;
  }

  /**
   * Gets the provider of this implementation.
   *
   * @return The provider of this implementation.
   */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
   * Returns the type of the certificate this factory creates.
   *
   * @return A string with the type of certificate
   */
  public final String getType()
  {
    return type;
  }

  /**
   * Generates a Certificate from the encoded data read
   * from an InputStream.
   *
   * <p>The input stream must contain only one certificate.
   *
   * <p>If there exists a specialized certificate class for the
   * certificate format handled by the certificate factory
   * then the return Ceritificate should be a typecast of it.
   * Ex: A X.509 CertificateFactory should return X509Certificate.
   *
   * <p>For X.509 certificates, the certificate in inStream must be
   * DER encoded and supplied in binary or printable (Base64) 
   * encoding. If the certificate is in Base64 encoding, it must be 
   * bounded by -----BEGINCERTIFICATE-----, and 
   * -----END CERTIFICATE-----. 
   *
   * @param inStream An input stream containing the certificate data.
   * @return A certificate initialized from the decoded InputStream data.
   * @throws CertificateException If an error occurs decoding the
   *   certificate.
   */
  public final Certificate generateCertificate(InputStream inStream)
    throws CertificateException
  {
    return certFacSpi.engineGenerateCertificate(inStream);
  }

  /**
   * Returns a collection of certificates that were read from the 
   * input stream. It may be empty, have only one, or have 
   * multiple certificates.
   *
   * For a X.509 certificate factory, the stream may contain a
   * single DER encoded certificate or a PKCS#7 certificate 
   * chain. This is a PKCS#7 <I>SignedData</I> object with the 
   * most significant field being <I>certificates</I>. If no 
   * CRLs are present, then an empty collection is returned.
	 *
   * @param inStream An input stream containing the certificate data.
   * @return A collection of certificates initialized from the decoded
   *   InputStream data.
   * @throws CertificateException If an error occurs decoding the
   *   certificates.
   */
  public final Collection<? extends Certificate> generateCertificates(InputStream inStream)
    throws CertificateException
  {
    return certFacSpi.engineGenerateCertificates(inStream);
  }

  /**
   * Generates a CRL based on the encoded data read
   * from the InputStream.
   *
   * <p>The input stream must contain only one CRL.
   *
   * <p>If there exists a specialized CRL class for the
   * CRL format handled by the certificate factory
   * then the return CRL should be a typecast of it.
   * Ex: A X.509 CertificateFactory should return X509CRL.
   *
   * @param inStream An input stream containing the CRL data.
   * @return A CRL initialized from the decoded InputStream data.
   * @throws CRLException If an error occurs decoding the CRL.
   */
  public final CRL generateCRL(InputStream inStream)
    throws CRLException
  {
    return certFacSpi.engineGenerateCRL(inStream);
  }

  /**
   * <p>Generates CRLs based on the encoded data read
   * from the InputStream.
   *
   * <p>For a X.509 certificate factory, the stream may contain a
   * single DER encoded CRL or a PKCS#7 CRL set. This is a 
   * PKCS#7 <I>SignedData</I> object with the most significant 
   * field being <I>crls</I>. If no CRLs are present, then an
   * empty collection is returned.
   *
   * @param inStream an input stream containing the CRLs.
   * @return a collection of CRLs initialized from the decoded
   *    InputStream data.
   * @throws CRLException If an error occurs decoding the CRLs.
   */
  public final Collection<? extends CRL> generateCRLs(InputStream inStream)
    throws CRLException
  {
    return certFacSpi.engineGenerateCRLs( inStream );
  }

  /**
   * Generate a {@link CertPath} and initialize it with data parsed from
   * the input stream. The default encoding of this factory is used.
   *
   * @param inStream The InputStream containing the CertPath data.
   * @return A CertPath initialized from the input stream data.
   * @throws CertificateException If an error occurs decoding the
   * CertPath.
   */
  public final CertPath generateCertPath(InputStream inStream)
    throws CertificateException
  {
    return certFacSpi.engineGenerateCertPath(inStream);
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
  public final CertPath generateCertPath(InputStream inStream, String encoding)
    throws CertificateException
  {
    return certFacSpi.engineGenerateCertPath(inStream, encoding);
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
  public final CertPath generateCertPath(List<? extends Certificate> certificates)
    throws CertificateException
  {
    return certFacSpi.engineGenerateCertPath(certificates);
  }

  /**
   * Returns an Iterator of CertPath encodings supported by this
   * factory, with the default encoding first. The returned Iterator
   * cannot be modified.
   *
   * @return The Iterator of supported encodings.
   */
  public final Iterator<String> getCertPathEncodings()
  {
    return certFacSpi.engineGetCertPathEncodings();
  }
} // class CertificateFactory
