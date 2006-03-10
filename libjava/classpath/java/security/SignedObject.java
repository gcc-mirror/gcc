/* SignedObject.java --- Signed Object Class
   Copyright (C) 1999, 2003, Free Software Foundation, Inc.

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

package java.security;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * <code>SignedObject</code> is used for storing runtime objects whose
 * integrity cannot be compromised without being detected.
 * 
 * <p><code>SignedObject</code> contains a {@link Serializable} object which is
 * yet to be signed and a digital signature of that object.</p>
 * 
 * <p>The signed copy is a "deep copy" (in serialized form) of an original
 * object. Any changes to that original instance are not reflected in the
 * enclosed copy inside this <code>SignedObject</code>.</p>
 * 
 * <p>Several things to note are that, first there is no need to initialize the
 * signature engine as this class will handle that automatically. Second,
 * verification will only succeed if the public key corresponds to the private
 * key used to generate the digital signature inside this
 * <code>SignedObject</code>.</p>
 * 
 * <p>For fexibility, the signature engine can be specified in the constructor
 * or the <code>verify()</code> method. Programmers wishing to verify
 * <code>SignedObject</code>s should be aware of the {@link Signature} engine
 * they use. A malicious or flawed {@link Signature} implementation may always
 * return true on verification thus circumventing the intended secrity check
 * provided by the <code>SignedObject</code>.</p>
 * 
 * <p>The GNU security provider offers an implementation of the standard NIST
 * DSA which uses "DSA" and "SHA-1". It can be specified by "SHA/DSA",
 * "SHA-1/DSA" or its OID. If the RSA signature algorithm is provided then it
 * could be "MD2/RSA". "MD5/RSA", or "SHA-1/RSA". The algorithm must be
 * specified because there is no default.</p>
 *
 * @author Mark Benvenuto (ivymccough@worldnet.att.net)
 * @since 1.2
 * @see Signature
 */
public final class SignedObject implements Serializable
{
  private static final long serialVersionUID = 720502720485447167L;

  /** @serial */
  private byte[] content;
  /** @serial */
  private byte[] signature;
  /** @serial */
  private String thealgorithm;

  /**
   * Constructs a new instance of <code>SignedObject</code> from a
   * {@link Serializable} object. The object is signed with a designated
   * private key and a signature engine.
   * 
   * @param object
   *          the object to sign.
   * @param signingKey
   *          the key to use.
   * @param signingEngine
   *          the signature engine to use.
   * @throws IOException
   *           if a serialization error occurred.
   * @throws InvalidKeyException
   *           if the key is invalid.
   * @throws SignatureException
   *           if a signing error occurs.
   */
  public SignedObject(Serializable object, PrivateKey signingKey,
		      Signature signingEngine)
    throws IOException, InvalidKeyException, SignatureException
  {
    thealgorithm = signingEngine.getAlgorithm();

    ByteArrayOutputStream ostream = new ByteArrayOutputStream();
    ObjectOutputStream p = new ObjectOutputStream(ostream);
    p.writeObject(object);
    p.flush();
    p.close();

    content = ostream.toByteArray();

    signingEngine.initSign(signingKey);
    signingEngine.update(content);
    signature = signingEngine.sign();
  }

  /**
   * Returns the encapsulated object. The object is de-serialized before being
   * returned.
   * 
   * @return the encapsulated object.
   * @throws IOException
   *           if a de-serialization error occurs.
   * @throws ClassNotFoundException
   *           if the encapsulated object's class was not found.
   */
  public Object getObject() throws IOException, ClassNotFoundException
  {
    ByteArrayInputStream bais = new ByteArrayInputStream(content);
    ObjectInput oi = new ObjectInputStream(bais);
    Object obj = oi.readObject();
    oi.close();
    bais.close();

    return obj;
  }

  /**
   * Returns the signature bytes of the encapsulated object.
   * 
   * @return the signature bytes of the encapsulated object.
   */
  public byte[] getSignature()
  {
    return (byte[]) signature.clone();

  }

  /**
   * Returns the name of the signature algorithm.
   * 
   * @return the name of the signature algorithm.
   */
  public String getAlgorithm()
  {
    return thealgorithm;
  }

  /**
   * Verifies the encapsulated digital signature by checking that it was
   * generated by the owner of a designated public key.
   * 
   * @param verificationKey
   *          the public key to use.
   * @param verificationEngine
   *          the signature engine to use.
   * @return <code>true</code> if signature is correct, <code>false</code>
   *         otherwise.
   * @throws InvalidKeyException
   *           if the key is invalid.
   * @throws SignatureException
   *           if verification fails.
   */
  public boolean verify(PublicKey verificationKey, Signature verificationEngine)
    throws InvalidKeyException, SignatureException
  {
    verificationEngine.initVerify(verificationKey);
    verificationEngine.update(content);
    return verificationEngine.verify(signature);
  }

  /** Called to restore the state of the SignedObject from a stream. */
  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    s.defaultReadObject();
    content = (byte[]) content.clone();
    signature = (byte[]) signature.clone();
  }
}
