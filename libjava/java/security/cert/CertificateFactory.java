/* CertificateFactory.java -- Certificate Factory Class
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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
import java.security.NoSuchProviderException;
import java.security.Provider;
import java.security.Security;
import java.io.InputStream;
import java.util.Collection;

/**
   This class implments the CertificateFactory class interface
   used to generate certificates and certificate revocation
   list (CRL) objects from their encodings.
   
   A certifcate factory for X.509 returns certificates of the 
   java.security.cert.X509Certificate class, and CRLs of the 
   java.security.cert.X509CRL class. 
   
   @author Mark Benvenuto
   @since JDK 1.2
   @status still missing full 1.4 support
*/
public class CertificateFactory
{

  private CertificateFactorySpi certFacSpi;
  private Provider provider;
  private String type;

  /**
     Creates an instance of CertificateFactory

     @param certFacSpi A CertificateFactory engine to use
     @param provider A provider to use
     @param type The type of Certificate
  */
  protected CertificateFactory(CertificateFactorySpi certFacSpi, Provider provider, String type)
  {
    this.certFacSpi = certFacSpi;
    this.provider = provider;
    this.type = type;
  }


  /** 
      Gets an instance of the CertificateFactory class representing
      the specified certificate factory. If the type is not 
      found then, it throws CertificateException.

      @param type the type of certificate to choose

      @return a CertificateFactory repesenting the desired type

      @throws CertificateException if the type of certificate is not implemented by providers
  */
  public static final CertificateFactory getInstance(String type) throws CertificateException
  {
    Provider[] p = Security.getProviders ();

    for (int i = 0; i < p.length; i++)
      {
	String classname = p[i].getProperty ("CertificateFactory." + type);
	if (classname != null)
	  return getInstance (classname, type, p[i]);
      }

    throw new CertificateException(type);
  }



  /** 
      Gets an instance of the CertificateFactory class representing
      the specified certificate factory from the specified provider. 
      If the type is not found then, it throws CertificateException. 
      If the provider is not found, then it throws 
      NoSuchProviderException.

      @param type the type of certificate to choose

      @return a CertificateFactory repesenting the desired type

      @throws CertificateException if the type of certificate is not implemented by providers
      @throws NoSuchProviderException if the provider is not found
  */
  public static final CertificateFactory getInstance(String type, String provider) 
    throws CertificateException, NoSuchProviderException
  {
    Provider p = Security.getProvider(provider);
    if( p == null)
      throw new NoSuchProviderException();

    return getInstance (p.getProperty ("CertificateFactory." + type),
			type, p);
  }

  private static CertificateFactory getInstance (String classname,
						 String type,
						 Provider provider)
    throws CertificateException
  {
    try {
      return new CertificateFactory( (CertificateFactorySpi)Class.forName( classname ).newInstance(), provider, type );
    } catch( ClassNotFoundException cnfe) {
      throw new CertificateException("Class not found");
    } catch( InstantiationException ie) {
      throw new CertificateException("Class instantiation failed");
    } catch( IllegalAccessException iae) {
      throw new CertificateException("Illegal Access");
    }
  }


  /**
     Gets the provider that the class is from.

     @return the provider of this class
  */
  public final Provider getProvider()
  {
    return provider;
  }

  /**
     Returns the type of the certificate supported

     @return A string with the type of certificate
  */
  public final String getType()
  {
    return type;
  }

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
     bounded by -----BEGINCERTIFICATE-----, and 
     -----END CERTIFICATE-----. 

     @param inStream an input stream containing the certificate data

     @return a certificate initialized with InputStream data.

     @throws CertificateException Certificate parsing error
  */
  public final Certificate generateCertificate(InputStream inStream)
    throws CertificateException
  {
    return certFacSpi.engineGenerateCertificate( inStream );
  }

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
  public final Collection generateCertificates(InputStream inStream)
    throws CertificateException
  {
    return certFacSpi.engineGenerateCertificates( inStream );
  }

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
  public final CRL generateCRL(InputStream inStream)
    throws CRLException
  {
    return certFacSpi.engineGenerateCRL( inStream );
  }


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
  public final Collection generateCRLs(InputStream inStream)
    throws CRLException
  {
    return certFacSpi.engineGenerateCRLs( inStream );
  }

  public final CertPath generateCertPath(InputStream inStream)
    throws CertificateException
  {
    throw new CertificateException("not implemented");
  }
} // class CertificateFactory
