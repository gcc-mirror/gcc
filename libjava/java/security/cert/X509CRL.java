/* X509CRL.java --- X.509 Certificate Revocation List
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
import java.util.Set;

/**
   The X509CRL class is the abstract class used to manage
   X.509 Certificate Revocation Lists. The CRL is a list of
   time stamped entries which indicate which lists have been
   revoked. The list is signed by a Certificate Authority (CA)
   and made publically available in a repository.
   
   Each revoked certificate in the CRL is identified by its 
   certificate serial number. When a piece of code uses a 
   certificate, the certificates validity is checked by 
   validating its signature and determing that it is not
   only a recently acquired CRL. The recently aquired CRL
   is depends on the local policy in affect. The CA issues
   a new CRL periodically and entries are removed as the 
   certificate expiration date is reached
   
   
   A description of the X.509 v2 CRL follows below from rfc2459.
   
   "The X.509 v2 CRL syntax is as follows.  For signature calculation,
   the data that is to be signed is ASN.1 DER encoded.  ASN.1 DER
   encoding is a tag, length, value encoding system for each element.
   
	   CertificateList  ::=  SEQUENCE  {
        	tbsCertList          TBSCertList,
	        signatureAlgorithm   AlgorithmIdentifier,
        	signatureValue       BIT STRING  }
	
	   TBSCertList  ::=  SEQUENCE  {
        	version                 Version OPTIONAL,
                                     -- if present, shall be v2
	        signature               AlgorithmIdentifier,
        	issuer                  Name,
	        thisUpdate              Time,
	        nextUpdate              Time OPTIONAL,
	        revokedCertificates     SEQUENCE OF SEQUENCE  {
	             userCertificate         CertificateSerialNumber,
	             revocationDate          Time,
	             crlEntryExtensions      Extensions OPTIONAL
	                                           -- if present, shall be v2
	                                  }  OPTIONAL,
	        crlExtensions           [0]  EXPLICIT Extensions OPTIONAL
	                                           -- if present, shall be v2
	                                  }"

	@author Mark Benvenuto

	@since JDK 1.2
*/
public abstract class X509CRL extends CRL implements X509Extension
{

  /**
     Constructs a new X509CRL.
  */
  protected X509CRL()
  {
    super("X.509");
  }

  /**
     Compares this X509CRL to other. It checks if the
     object if instanceOf X509CRL and then checks if
     the encoded form matches.

     @param other An Object to test for equality

     @return true if equal, false otherwise
  */
  public boolean equals(Object other)
  {
    if( other instanceof X509CRL ) {
      try {
	X509CRL x = (X509CRL) other;
	if( getEncoded().length != x.getEncoded().length )
	  return false;

	byte b1[] = getEncoded();
	byte b2[] = x.getEncoded();

	for( int i = 0; i < b1.length; i++ )
	  if( b1[i] != b2[i] )
	    return false;

      } catch( CRLException crle ) { 
	return false;
      }
      return true;
    }
    return false;
  }

  /**
     Returns a hash code for this X509CRL in its encoded
     form.

     @return A hash code of this class
  */
  public int hashCode()
  {
    return super.hashCode();
  }

  /**
     Gets the DER ASN.1 encoded format for this X.509 CRL.

     @return byte array containg encoded form

     @throws CRLException if an error occurs
  */
  public abstract byte[] getEncoded() throws CRLException;

  /**
     Verifies that this CRL was properly signed with the
     PublicKey that corresponds to its private key. 

     @param key PublicKey to verify with

     @throws CRLException encoding error
     @throws NoSuchAlgorithmException unsupported algorithm
     @throws InvalidKeyException incorrect key
     @throws NoSuchProviderException no provider
     @throws SignatureException signature error
  */
  public abstract void verify(PublicKey key)
    throws CRLException,
    NoSuchAlgorithmException,
    InvalidKeyException,
    NoSuchProviderException,
    SignatureException;

  /**
     Verifies that this CRL was properly signed with the
     PublicKey that corresponds to its private key and uses
     the signature engine provided by the provider. 

     @param key PublicKey to verify with
     @param sigProvider Provider to use for signature algorithm

     @throws CRLException encoding error
     @throws NoSuchAlgorithmException unsupported algorithm
     @throws InvalidKeyException incorrect key
     @throws NoSuchProviderException incorrect provider
     @throws SignatureException signature error
  */
  public abstract void verify(PublicKey key,
			      String sigProvider)
    throws CRLException,
    NoSuchAlgorithmException,
    InvalidKeyException,
    NoSuchProviderException,
    SignatureException;

  /**
     Gets the version of this CRL.

     The ASN.1 encoding is:

     version                 Version OPTIONAL,
     -- if present, shall be v2

     Version  ::=  INTEGER  {  v1(0), v2(1), v3(2)  }

     Consult rfc2459 for more information.

     @return the version number, Ex: 1 or 2
  */
  public abstract int getVersion();

  /**
     Returns the issuer (issuer distinguished name) of the CRL.
     The issuer is the entity who signed and issued the 
     Certificate Revocation List.

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
     Returns the thisUpdate date of the CRL.

     The ASN.1 DER encoding is:

     thisUpdate              Time,

     Time ::= CHOICE {
     utcTime        UTCTime,
     generalTime    GeneralizedTime }

     Consult rfc2459 for more information.

     @return the thisUpdate date
  */
  public abstract Date getThisUpdate();

  /*
    Gets the nextUpdate field

    The ASN.1 DER encoding is:

    nextUpdate              Time OPTIONAL,

    Time ::= CHOICE {
    utcTime        UTCTime,
    generalTime    GeneralizedTime }

    Consult rfc2459 for more information.

    @return the nextUpdate date
  */
  public abstract Date getNextUpdate();

  /**
     Gets the requeste dX509Entry for the specified
     certificate serial number.

     @return a X509CRLEntry representing the X.509 CRL entry
  */
  public abstract X509CRLEntry getRevokedCertificate(BigInteger serialNumber);

  /**
     Returns a Set of revoked certificates.

     @return a set of revoked certificates.
  */
  public abstract Set getRevokedCertificates();

  /**
     Returns the DER ASN.1 encoded tbsCertList which is 
     the basic information of the list and associated certificates
     in the encoded state. See top for more information.

     The ASN.1 DER encoding is:

     tbsCertList          TBSCertList,

     Consult rfc2459 for more information.

     @return byte array representing tbsCertList
  */
  public abstract byte[] getTBSCertList() throws CRLException;


  /**
     Returns the signature for the CRL. 

     The ASN.1 DER encoding is:

     signatureValue       BIT STRING

     Consult rfc2459 for more information.
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
     if no parameters are present in CRL
  */
  public abstract byte[] getSigAlgParams();

}
