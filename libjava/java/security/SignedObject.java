/* SignedObject.java --- Signed Object Class
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

package java.security;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
   SignedObject is used for storing rutime objects whose integrity
   cannot be compromised without being detected.

   SignedObject contains a Serializable object which is yet to be 
   signed and its signature.

   The signed copy is a "deep copy" (in serialized form) of the 
   original object. Any changes to the original will not affect 
   the original.

   Several things to note are that, first there is no need to 
   initialize the signature engine as this class will handle that 
   automatically. Second, verification will only succeed if the
   public key corresponds to the private key used to generate 
   the SignedObject.

   For fexibility, the signature engine can be specified in the 
   constructor or the verify method. The programmer who writes 
   code that verifies the SignedObject has not changed should be 
   aware of the Signature engine they use. A malicious Signature 
   may choose to always return true on verification and 
   bypass the secrity check.

   The GNU provider provides the NIST standard DSA which uses DSA 
   and SHA-1. It can be specified by SHA/DSA, SHA-1/DSA or its 
   OID. If the RSA signature algorithm is provided then
   it could be MD2/RSA. MD5/RSA, or SHA-1/RSA. The algorithm must
   be specified because there is no default.

   @author Mark Benvenuto <ivymccough@worldnet.att.net>

   @since JDK 1.2
 */
public final class SignedObject implements Serializable
{
  static final long serialVersionUID = 720502720485447167L;

  private byte[] content;
  private byte[] signature;
  private String thealgorithm;

  /**
     Constructs a new SignedObject from a Serializeable object. The 
     object is signed with private key and signature engine

     @param object the object to sign
     @param signingKey the key to sign with
     @param signingEngine the signature engine to use

     @throws IOException serialization error occurred
     @throws InvalidKeyException invalid key
     @throws SignatureException signing error
   */
  public SignedObject(Serializable object, PrivateKey signingKey,
		      Signature signingEngine) throws IOException,
    InvalidKeyException, SignatureException
  {
    thealgorithm = signingEngine.getAlgorithm();

    ByteArrayOutputStream ostream = new ByteArrayOutputStream();
    ObjectOutputStream p = new ObjectOutputStream(ostream);
    p.writeObject(object);
    p.flush();

    content = ostream.toByteArray();

    signingEngine.initSign(signingKey);
    signingEngine.update(content);
    signature = signingEngine.sign();
  }

  /**
     Returns the encapsulated object. The object is 
     de-serialized before being returned.

     @return the encapsulated object

     @throws IOException de-serialization error occurred
     @throws ClassNotFoundException de-serialization error occurred
   */
  public Object getObject() throws IOException, ClassNotFoundException
  {
    ByteArrayInputStream istream = new ByteArrayInputStream(content);

    return new ObjectInputStream(istream).readObject();
  }

  /**
     Returns the signature of the encapsulated object.

     @return a byte array containing the signature
   */
  public byte[] getSignature()
  {
    return signature;
  }

  /**
     Returns the name of the signature algorithm.

     @return the name of the signature algorithm.
   */
  public String getAlgorithm()
  {
    return thealgorithm;
  }

  /**
     Verifies the SignedObject by checking that the signature that 
     this class contains for the encapsulated object.

     @param verificationKey the public key to use
     @param verificationEngine the signature engine to use

     @return true if signature is correct, false otherwise

     @throws InvalidKeyException invalid key
     @throws SignatureException signature verification failed
   */
  public boolean verify(PublicKey verificationKey,
			Signature verificationEngine) throws
    InvalidKeyException, SignatureException
  {
    verificationEngine.initVerify(verificationKey);
    verificationEngine.update(content);
    return verificationEngine.verify(signature);
  }

  //     readObject is called to restore the state of the SignedObject from a
  //     stream.
  //private void readObject(ObjectInputStream s)
  //                 throws IOException, ClassNotFoundException
}
