/* X509CRLEntry.java --- X.509 Certificate Revocation List Entry
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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.security.cert;
import java.math.BigInteger;
import java.util.Date;

/**
	Abstract class for entries in the CRL (Certificate Revocation 
	List). The ASN.1 definition for <I>revokedCertificates</I> is

        revokedCertificates     SEQUENCE OF SEQUENCE  {
             userCertificate         CertificateSerialNumber,
             revocationDate          Time,
             crlEntryExtensions      Extensions OPTIONAL
                                           -- if present, shall be v2
                                  }  OPTIONAL,

	CertificateSerialNumber  ::=  INTEGER

	Time ::= CHOICE {
             utcTime        UTCTime,
	     generalTime    GeneralizedTime }

	Extensions  ::=  SEQUENCE SIZE (1..MAX) OF Extension

	Extension  ::=  SEQUENCE  {
	     extnID      OBJECT IDENTIFIER,
             critical    BOOLEAN DEFAULT FALSE,
             extnValue   OCTET STRING  }
 
	For more information consult rfc2459.

	@author Mark Benvenuto

	@since JDK 1.2
*/
public abstract class X509CRLEntry implements X509Extension
{

  /**
     Creates a new X509CRLEntry
  */
  public X509CRLEntry()
  {}

  /**
     Compares this X509CRLEntry to other. It checks if the
     object if instanceOf X509CRLEntry and then checks if
     the encoded form( the inner SEQUENCE) matches.

     @param other An Object to test for equality

     @return true if equal, false otherwise
  */
  public boolean equals(Object other)
  {
    if( other instanceof X509CRLEntry ) {
      try {
	X509CRLEntry xe = (X509CRLEntry) other;
	if( getEncoded().length != xe.getEncoded().length )
	  return false;

	byte b1[] = getEncoded();
	byte b2[] = xe.getEncoded();

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
     Returns a hash code for this X509CRLEntry in its encoded
     form.

     @return A hash code of this class
  */
  public int hashCode()
  {
    return super.hashCode();
  }

  /**
     Gets the DER ASN.1 encoded format for this CRL Entry,
     the inner SEQUENCE.

     @return byte array containg encoded form

     @throws CRLException if an error occurs
  */
  public abstract byte[] getEncoded() throws CRLException;

  /**
     Gets the serial number for <I>userCertificate</I> in
     this X509CRLEntry.

     @return the serial number for this X509CRLEntry.
  */
  public abstract BigInteger getSerialNumber();


  /**
     Gets the revocation date in <I>revocationDate</I> for
     this X509CRLEntry.

     @return the revocation date for this X509CRLEntry.
  */
  public abstract Date getRevocationDate();


  /**
     Checks if this X509CRLEntry has extensions.

     @return true if it has extensions, false otherwise
  */
  public abstract boolean hasExtensions();


  /**
     Returns a string that represents this X509CRLEntry.

     @return a string representing this X509CRLEntry.
  */
  public abstract String toString();

}
