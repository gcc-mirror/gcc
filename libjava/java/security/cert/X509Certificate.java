/* X509Certificate.java --- X.509 Certificate class
   Copyright (C) 1999 Free Software Foundation, Inc.

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
import java.math.BigInteger;
import java.security.Principal;
import java.security.PublicKey;
import java.security.NoSuchAlgorithmException;
import java.security.InvalidKeyException;
import java.security.NoSuchProviderException;
import java.security.SignatureException;
import java.util.Date;

/**
	X509Certificate is the abstract class for X.509 certificates.
	This provides a stanard class interface for accessing all 
	the attributes of X.509 certificates.

	In June 1996, the basic X.509 v3 format was finished by 
	ISO/IEC and ANSI X.9. The ASN.1 DER format is below:

	   Certificate  ::=  SEQUENCE  {
        	tbsCertificate       TBSCertificate,
	        signatureAlgorithm   AlgorithmIdentifier,
        	signatureValue       BIT STRING  }

	These certificates are widely used in various Internet 
	protocols to support authentication. It is used in 
	Privacy Enhanced Mail (PEM), Transport Layer Security (TLS),
	Secure Sockets Layer (SSL), code signing for trusted software
	distribution, and Secure Electronic Transactions (SET).

	The certificates are managed and vouched for by 
	<I>Certificate Authorities</I> (CAs). CAs are companies or 
	groups that create certificates by placing the data in the 
	X.509 certificate format and signing it with their private
	key. CAs serve as trusted third parties by certifying that
	the person or group specified in the certificate is who
	they say they are. 

	The ASN.1 defintion for <I>tbsCertificate</I> is
	
   TBSCertificate  ::=  SEQUENCE  {
        version         [0]  EXPLICIT Version DEFAULT v1,
        serialNumber         CertificateSerialNumber,
        signature            AlgorithmIdentifier,
        issuer               Name,
        validity             Validity,
        subject              Name,
        subjectPublicKeyInfo SubjectPublicKeyInfo,
        issuerUniqueID  [1]  IMPLICIT UniqueIdentifier OPTIONAL,
                             -- If present, version shall be v2 or v3
        subjectUniqueID [2]  IMPLICIT UniqueIdentifier OPTIONAL,
                             -- If present, version shall be v2 or v3
        extensions      [3]  EXPLICIT Extensions OPTIONAL
                             -- If present, version shall be v3
        }

   Version  ::=  INTEGER  {  v1(0), v2(1), v3(2)  }

   CertificateSerialNumber  ::=  INTEGER

   Validity ::= SEQUENCE {
        notBefore      Time,
        notAfter       Time }

   Time ::= CHOICE {
        utcTime        UTCTime,
        generalTime    GeneralizedTime }

   UniqueIdentifier  ::=  BIT STRING

   SubjectPublicKeyInfo  ::=  SEQUENCE  {
        algorithm            AlgorithmIdentifier,
        subjectPublicKey     BIT STRING  }

   Extensions  ::=  SEQUENCE SIZE (1..MAX) OF Extension

   Extension  ::=  SEQUENCE  {
        extnID      OBJECT IDENTIFIER,
        critical    BOOLEAN DEFAULT FALSE,
        extnValue   OCTET STRING  }


	Certificates are created with the CertificateFactory.
	For more information about X.509 certificates, consult
	rfc2459.

	@since JDK 1.2

	@author Mark Benvenuto
*/
public abstract class X509Certificate extends Certificate implements X509Extension
{

  /**
     Constructs a new certificate of the specified type.
  */
  protected X509Certificate()
  {
    super( "X.509" );
  }

  /**
     Checks the validity of the X.509 certificate. It is valid
     if the current date and time are within the period specified
     by the certificate.

     The ASN.1 DER encoding is:

     validity             Validity,

     Validity ::= SEQUENCE {
     notBefore      Time,
     notAfter       Time }

     Time ::= CHOICE {
     utcTime        UTCTime,
     generalTime    GeneralizedTime }

     Consult rfc2459 for more information.

     @throws CertificateExpiredException if the certificate expired
     @throws CertificateNotYetValidException if the certificate is 
     not yet valid
  */
  public abstract void checkValidity()
    throws CertificateExpiredException,
    CertificateNotYetValidException;

  /**
     Checks the validity of the X.509 certificate for the 
     specified time and date. It is valid if the specified 
     date and time are within the period specified by 
     the certificate.

     @throws CertificateExpiredException if the certificate expired 
     based on the date
     @throws CertificateNotYetValidException if the certificate is 
     not yet valid based on the date
  */
  public abstract void checkValidity(Date date)
    throws CertificateExpiredException,
    CertificateNotYetValidException;

  /**
     Returns the version of this certificate.

     The ASN.1 DER encoding is:

     version         [0]  EXPLICIT Version DEFAULT v1,

     Version  ::=  INTEGER  {  v1(0), v2(1), v3(2)  }

     Consult rfc2459 for more information.

     @return version number of certificate	
  */
  public abstract int getVersion();

  /**
     Gets the serial number for serial Number in
     this Certifcate. It must be a unique number 
     unique other serial numbers from the granting CA.

     The ASN.1 DER encoding is:

     serialNumber         CertificateSerialNumber,

     CertificateSerialNumber  ::=  INTEGER

     Consult rfc2459 for more information.

     @return the serial number for this X509CRLEntry.
  */
  public abstract BigInteger getSerialNumber();

  /**
     Returns the issuer (issuer distinguished name) of the 
     Certificate. The issuer is the entity who signed 
     and issued the Certificate.

     The ASN.1 DER encoding is:

     issuer                  Name,

     Name ::= CHOICE {
     RDNSequence }

     RDNSequence ::= SEQUENCE OF RelativeDistinguishedName

     RelativeDistinguishedName ::=
     SET OF AttributeTypeAndValue

     AttributeTypeAndValue ::= SEQUENCE {
     type     AttributeType,
     value    AttributeValue }

     AttributeType ::= OBJECT IDENTIFIER

     AttributeValue ::= ANY DEFINED BY AttributeType

     DirectoryString ::= CHOICE {
     teletexString           TeletexString (SIZE (1..MAX)),
     printableString         PrintableString (SIZE (1..MAX)),
     universalString         UniversalString (SIZE (1..MAX)),
     utf8String              UTF8String (SIZE (1.. MAX)),
     bmpString               BMPString (SIZE (1..MAX)) }

     Consult rfc2459 for more information.

     @return the issuer in the Principal class
  */
  public abstract Principal getIssuerDN();

  /**
     Returns the subject (subject distinguished name) of the 
     Certificate. The subject is the entity who the Certificate
     identifies.

     The ASN.1 DER encoding is:

     subject              Name,

     Consult rfc2459 for more information.

     @return the issuer in the Principal class
  */
  public abstract Principal getSubjectDN();

  /**
     Returns the date that this certificate is not to be used
     before, <I>notBefore</I>.

     The ASN.1 DER encoding is:

     validity             Validity,

     Validity ::= SEQUENCE {
     notBefore      Time,
     notAfter       Time }

     Time ::= CHOICE {
     utcTime        UTCTime,
     generalTime    GeneralizedTime }

     Consult rfc2459 for more information.

     @return the date <I>notBefore</I>
  */
  public abstract Date getNotBefore();

  /**
     Returns the date that this certificate is not to be used
     after, <I>notAfter</I>.

     @return the date <I>notAfter</I>
  */
  public abstract Date getNotAfter();


  /**
     Returns the <I>tbsCertificate</I> from the certificate.

     @return the DER encoded tbsCertificate

     @throws CertificateEncodingException if encoding error occurred
  */
  public abstract byte[] getTBSCertificate() throws CertificateEncodingException;

  /**
     Returns the signature in its raw DER encoded format.

     The ASN.1 DER encoding is:

     signatureValue       BIT STRING

     Consult rfc2459 for more information.

     @return byte array representing signature
  */
  public abstract byte[] getSignature();

  /**
     Returns the signature algorithm used to sign the CRL. 
     An examples is "SHA-1/DSA".

     The ASN.1 DER encoding is:

     signatureAlgorithm   AlgorithmIdentifier,

     AlgorithmIdentifier  ::=  SEQUENCE  {
     algorithm               OBJECT IDENTIFIER,
     parameters              ANY DEFINED BY algorithm OPTIONAL  }

     Consult rfc2459 for more information.

     The algorithm name is determined from the OID.

     @return a string with the signature algorithm name
  */
  public abstract String getSigAlgName();


  /**
     Returns the OID for the signature algorithm used.
     Example "1.2.840.10040.4.3" is return for SHA-1 with DSA.\

     The ASN.1 DER encoding for the example is:

     id-dsa-with-sha1 ID  ::=  {
     iso(1) member-body(2) us(840) x9-57 (10040)
     x9cm(4) 3 }

     Consult rfc2459 for more information.

     @return a string containing the OID.
  */
  public abstract String getSigAlgOID();


  /**
     Returns the AlgorithmParameters in the encoded form
     for the signature algorithm used. 

     If access to the parameters is need, create an 
     instance of AlgorithmParameters.

     @return byte array containing algorithm parameters, null
     if no parameters are present in certificate
  */
  public abstract byte[] getSigAlgParams();


  /**
     Returns the issuer unique ID for this certificate.

     The ASN.1 DER encoding is:

     issuerUniqueID  [1]  IMPLICIT UniqueIdentifier OPTIONAL,
     -- If present, version shall be v2 or v3

     UniqueIdentifier  ::=  BIT STRING
	
     Consult rfc2459 for more information.

     @return bit representation of <I>issuerUniqueID</I>
  */
  public abstract boolean[] getIssuerUniqueID();

  /**
     Returns the subject unique ID for this certificate.

     The ASN.1 DER encoding is:

     subjectUniqueID [2]  IMPLICIT UniqueIdentifier OPTIONAL,
     -- If present, version shall be v2 or v3

     UniqueIdentifier  ::=  BIT STRING
	
     Consult rfc2459 for more information.

     @return bit representation of <I>subjectUniqueID</I>
  */
  public abstract boolean[] getSubjectUniqueID();

  /**
     Returns a boolean array representing the <I>KeyUsage</I> 
     extension for the certificate. The KeyUsage (OID = 2.5.29.15)
     defines the purpose of the key in the certificate.

     The ASN.1 DER encoding is:

     id-ce-keyUsage OBJECT IDENTIFIER ::=  { id-ce 15 }

     KeyUsage ::= BIT STRING {
     digitalSignature        (0),
     nonRepudiation          (1),
     keyEncipherment         (2),
     dataEncipherment        (3),
     keyAgreement            (4),
     keyCertSign             (5),
     cRLSign                 (6),
     encipherOnly            (7),
     decipherOnly            (8) }

     Consult rfc2459 for more information.

     @return bit representation of <I>KeyUsage</I>
  */
  public abstract boolean[] getKeyUsage();

  /**
     Returns the certificate constraints path length from the
     critical BasicConstraints extension, (OID = 2.5.29.19).	

     The basic constraints extensions is used to determine if 
     the subject of the certificate is a Certificate Authority (CA) 
     and how deep the certification path may exist. The 
     <I>pathLenConstraint</I> only takes affect if <I>cA</I>
     is set to true. "A value of zero indicates that only an 
     end-entity certificate may follow in the path." (rfc2459)
	
     The ASN.1 DER encoding is:

     id-ce-basicConstraints OBJECT IDENTIFIER ::=  { id-ce 19 }

     BasicConstraints ::= SEQUENCE {
     cA                      BOOLEAN DEFAULT FALSE,
     pathLenConstraint       INTEGER (0..MAX) OPTIONAL }

     Consult rfc2459 for more information.

     @return the length of the path constraint if BasicConstraints
     is present and cA is TRUE. Otherwise returns -1.
  */
  public abstract int getBasicConstraints();


}
