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
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * <p><code>SignedObject</code> is a class for the purpose of creating authentic
 * runtime objects whose integrity cannot be compromised without being detected.
 * </p>
 *
 * <p>More specifically, a <code>SignedObject</code> contains another
 * {@link Serializable} object, the (to-be-)signed object and its signature.</p>
 *
 * <p>The signed object is a <i>"deep copy"</i> (in serialized form) of an
 * original object. Once the copy is made, further manipulation of the original
 * object has no side effect on the copy.</p>
 *
 * <p>The underlying signing algorithm is designated by the {@link Signature}
 * object passed to the constructor and the <code>verify()</code> method. A
 * typical usage for signing is the following:</p>
 *
 * <pre>
 * Signature signingEngine = Signature.getInstance(algorithm, provider);
 * SignedObject so = new SignedObject(myobject, signingKey, signingEngine);
 * </pre>
 *
 * <p>A typical usage for verification is the following (having received
 * <code>SignedObject</code> so):</p>
 *
 * <pre>
 * Signature verificationEngine = Signature.getInstance(algorithm, provider);
 * if (so.verify(publickey, verificationEngine))
 *   try
 *     {
 *       Object myobj = so.getObject();
 *     }
 *   catch (ClassNotFoundException ignored) {};
 * </pre>
 *
 * <p>Several points are worth noting. First, there is no need to initialize the
 * signing or verification engine, as it will be re-initialized inside the
 * constructor and the <code>verify()</code> method. Secondly, for verification
 * to succeed, the specified public key must be the public key corresponding to
 * the private key used to generate the <code>SignedObject</code>.</p>
 *
 * <p>More importantly, for flexibility reasons, the <code>constructor</code>
 * and <code>verify()</code> method allow for customized signature engines,
 * which can implement signature algorithms that are not installed formally as
 * part of a crypto provider. However, it is crucial that the programmer writing
 * the verifier code be aware what {@link Signature} engine is being used, as
 * its own implementation of the <code>verify()</code> method is invoked to
 * verify a signature. In other words, a malicious {@link Signature} may choose
 * to always return <code>true</code> on verification in an attempt to bypass a
 * security check.</p>
 *
 * <p>The signature algorithm can be, among others, the NIST standard <i>DSS</i>,
 * using <i>DSA</i> and <i>SHA-1</i>. The algorithm is specified using the same
 * convention as that for signatures. The <i>DSA</i> algorithm using the
 * <i>SHA-1</i> message digest algorithm can be specified, for example, as
 * <code>"SHA/DSA"</code> or <code>"SHA-1/DSA"</code> (they are equivalent). In
 * the case of <i>RSA</i>, there are multiple choices for the message digest
 * algorithm, so the signing algorithm could be specified as, for example,
 * <code>"MD2/RSA"</code>, <code>"MD5/RSA"</code> or <code>"SHA-1/RSA"</code>.
 * The algorithm name must be specified, as there is no default.</p>
 *
 * <p>The name of the Cryptography Package Provider is designated also by the
 * {@link Signature} parameter to the <code>constructor</code> and the <code>
 * verify()</code> method. If the provider is not specified, the default
 * provider is used. Each installation can be configured to use a particular
 * provider as default.</p>
 *
 * <p>Potential applications of <code>SignedObject</code> include:</p>
 *
 * <ul>
 *  <li>It can be used internally to any Java runtime as an unforgeable
 *  authorization token -- one that can be passed around without the fear that
 *  the token can be maliciously modified without being detected.</li>
 *  <li>It can be used to sign and serialize data/object for storage outside the
 *  Java runtime (e.g., storing critical access control data on disk).</li>
 *  <li>Nested <i>SignedObjects</i> can be used to construct a logical sequence
 *  of signatures, resembling a chain of authorization and delegation.</li>
 * </ul>
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
   * Constructs a <code>SignedObject</code> from any {@link Serializable}
   * object. The given object is signed with the given signing key, using the
   * designated signature engine.
   *
   * @param object the object to be signed.
   * @param signingKey the private key for signing.
   * @param signingEngine the signature signing engine.
   * @throws IOException if an error occurs during serialization.
   * @throws InvalidKeyException if the key is invalid.
   * @throws SignatureException if signing fails.
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
   * Retrieves the encapsulated object. The encapsulated object is de-serialized
   * before it is returned.
   *
   * @return the encapsulated object.
   * @throws IOException if an error occurs during de-serialization.
   * @throws ClassNotFoundException if an error occurs during de-serialization.
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
   * Retrieves the signature on the signed object, in the form of a byte array.
   *
   * @return a copy of the signature.
   */
  public byte[] getSignature()
  {
    return (byte[]) signature.clone();

  }

  /**
   * Retrieves the name of the signature algorithm.
   *
   * @return the signature algorithm name.
   */
  public String getAlgorithm()
  {
    return thealgorithm;
  }

  /**
   * Verifies that the signature in this <code>SignedObject</code> is the valid
   * signature for the object stored inside, with the given verification key,
   * using the designated verification engine.
   *
   * @param verificationKey the public key for verification.
   * @param verificationEngine the signature verification engine.
   * @return <code>true</code> if the signature is valid, <code>false</code>
   * otherwise.
   * @throws SignatureException if signature verification failed.
   * @throws InvalidKeyException if the verification key is invalid.
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
