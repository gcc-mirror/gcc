/* Certificate.java --- Certificate class
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
import java.security.PublicKey;
import java.security.NoSuchAlgorithmException;
import java.security.InvalidKeyException;
import java.security.NoSuchProviderException;
import java.security.SignatureException;
import java.io.ObjectInputStream;
import java.io.ByteArrayInputStream;
import java.io.ObjectStreamException;

/**
   The Certificate class is an abstract class used to manage 
   identity certificates. An identity certificate is a
   combination of a principal and a public key which is 
   certified by another principal. This is the puprose of 
   Certificate Authorities (CA).
   
   This class is used to manage different types of certificates
   but have important common puposes. Different types of 
   certificates like X.509 and OpenPGP share general certificate
   functions (like encoding and verifying) and information like
   public keys.
   
   X.509, OpenPGP, and SDSI can be implemented by subclassing this
   class even though they differ in storage methods and information
   stored.
   
   @since JDK 1.2
   
   @author Mark Benvenuto
*/
public abstract class Certificate
{

  private String type;
  /**
     Constructs a new certificate of the specified type. An example
     is "X.509".

     @param type a valid standard name for a certificate.
  */
  protected Certificate(String type)
  {
    this.type = type;
  }

  /**
     Returns the Certificate type.

     @return a string representing the Certificate type
  */
  public final String getType()
  {
    return type;
  }

  /**
     Compares this Certificate to other. It checks if the
     object if instanceOf Certificate and then checks if
     the encoded form matches.

     @param other An Object to test for equality

     @return true if equal, false otherwise
  */
  public boolean equals(Object other)
  {
    if( other instanceof Certificate ) {
      try {
	Certificate x = (Certificate) other;
	if( getEncoded().length != x.getEncoded().length )
	  return false;

	byte b1[] = getEncoded();
	byte b2[] = x.getEncoded();

	for( int i = 0; i < b1.length; i++ )
	  if( b1[i] != b2[i] )
	    return false;

      } catch( CertificateEncodingException cee ) { 
	return false;
      }
      return true;
    }
    return false;
  }

  /**
     Returns a hash code for this Certificate in its encoded
     form.

     @return A hash code of this class
  */
  public int hashCode()
  {
    return super.hashCode();
  }

  /**
     Gets the DER ASN.1 encoded format for this Certificate.
     It assumes each certificate has only one encoding format.
     Ex: X.509 is encoded as ASN.1 DER

     @return byte array containg encoded form

     @throws CertificateEncodingException if an error occurs
  */
  public abstract byte[] getEncoded() throws CertificateEncodingException;

  /**
     Verifies that this Certificate was properly signed with the
     PublicKey that corresponds to its private key. 

     @param key PublicKey to verify with

     @throws CertificateException encoding error
     @throws NoSuchAlgorithmException unsupported algorithm
     @throws InvalidKeyException incorrect key
     @throws NoSuchProviderException no provider
     @throws SignatureException signature error
  */
  public abstract void verify(PublicKey key)
    throws CertificateException,
    NoSuchAlgorithmException,
    InvalidKeyException,
    NoSuchProviderException,
    SignatureException;

  /**
     Verifies that this Certificate was properly signed with the
     PublicKey that corresponds to its private key and uses
     the signature engine provided by the provider. 

     @param key PublicKey to verify with
     @param sigProvider Provider to use for signature algorithm

     @throws CertificateException encoding error
     @throws NoSuchAlgorithmException unsupported algorithm
     @throws InvalidKeyException incorrect key
     @throws NoSuchProviderException incorrect provider
     @throws SignatureException signature error
  */
  public abstract void verify(PublicKey key,
			      String sigProvider)
    throws CertificateException,
    NoSuchAlgorithmException,
    InvalidKeyException,
    NoSuchProviderException,
    SignatureException;

  /**
     Returns a string representing the Certificate.

     @return a string representing the Certificate.
  */
  public abstract String toString();


  /**
     Returns the public key stored in the Certificate.

     @return The public key
  */
  public abstract PublicKey getPublicKey();


  /* INNER CLASS */
  /**
     Certificate.CertificateRep is an inner class used to provide an alternate
     storage mechanism for serialized Certificates.
  */
  protected static class CertificateRep implements java.io.Serializable
  {
    private String type;
    private byte[] data;

    /**
       Create an alternate Certificate class to store a serialized Certificate

       @param type the name of certificate type
       @param data the certificate data
    */
    protected CertificateRep(String type,
			     byte[] data)
    {
      this.type = type;
      this.data = data;
    }

    /**
       Return the stored Certificate

       @return the stored certificate

       @throws ObjectStreamException if certificate cannot be resolved
    */
    protected Object readResolve()
      throws ObjectStreamException
    {
      try {
	return new ObjectInputStream( new ByteArrayInputStream( data ) ).readObject();
      } catch ( Exception e ) {
	e.printStackTrace();
	throw new RuntimeException ( e.toString() );
      }
    }
  }

}
