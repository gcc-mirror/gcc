/* SignatureAdapter.java -- 
   Copyright 2001, 2002, 2006 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.java.security.jce.sig;

import gnu.java.security.Configuration;
import gnu.java.security.sig.BaseSignature;
import gnu.java.security.sig.ISignature;
import gnu.java.security.sig.ISignatureCodec;
import gnu.java.security.sig.SignatureFactory;

import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.InvalidParameterException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.SignatureException;
import java.security.SignatureSpi;
import java.security.spec.AlgorithmParameterSpec;
import java.util.HashMap;
import java.util.logging.Logger;

/**
 * The implementation of a generic {@link java.security.Signature} adapter class
 * to wrap GNU signature instances.
 * <p>
 * This class defines the <i>Service Provider Interface</i> (<b>SPI</b>) for
 * the {@link java.security.Signature} class, which provides the functionality
 * of a digital signature algorithm. Digital signatures are used for
 * authentication and integrity assurance of digital data.
 * <p>
 * All the abstract methods in the {@link SignatureSpi} class are implemented by
 * this class and all its sub-classes.
 * <p>
 * All the implementations which subclass this object, and which are serviced by
 * the GNU provider implement the {@link Cloneable} interface.
 */
class SignatureAdapter
    extends SignatureSpi
    implements Cloneable
{
  private static final Logger log = Logger.getLogger(SignatureAdapter.class.getName());

  /** Our underlying signature instance. */
  private ISignature adaptee;

  /** Our underlying signature encoder/decoder engine. */
  private ISignatureCodec codec;

  /**
   * Trivial protected constructor.
   * 
   * @param sigName the canonical name of the signature scheme.
   * @param codec the signature codec engine to use with this scheme.
   */
  protected SignatureAdapter(String sigName, ISignatureCodec codec)
  {
    this(SignatureFactory.getInstance(sigName), codec);
  }

  /**
   * Private constructor for cloning purposes.
   * 
   * @param adaptee a clone of the underlying signature scheme instance.
   * @param codec the signature codec engine to use with this scheme.
   */
  private SignatureAdapter(ISignature adaptee, ISignatureCodec codec)
  {
    super();

    this.adaptee = adaptee;
    this.codec = codec;
  }

  public Object clone()
  {
    return new SignatureAdapter((ISignature) adaptee.clone(), codec);
  }

  public void engineInitVerify(PublicKey publicKey) throws InvalidKeyException
  {
    HashMap attributes = new HashMap();
    attributes.put(BaseSignature.VERIFIER_KEY, publicKey);
    try
      {
        adaptee.setupVerify(attributes);
      }
    catch (IllegalArgumentException x)
      {
        throw new InvalidKeyException(x.getMessage(), x);
      }
  }

  public void engineInitSign(PrivateKey privateKey) throws InvalidKeyException
  {
    HashMap attributes = new HashMap();
    attributes.put(BaseSignature.SIGNER_KEY, privateKey);
    try
      {
        adaptee.setupSign(attributes);
      }
    catch (IllegalArgumentException x)
      {
        throw new InvalidKeyException(x.getMessage(), x);
      }
  }

  public void engineInitSign(PrivateKey privateKey, SecureRandom random)
      throws InvalidKeyException
  {
    HashMap attributes = new HashMap();
    attributes.put(BaseSignature.SIGNER_KEY, privateKey);
    attributes.put(BaseSignature.SOURCE_OF_RANDOMNESS, random);
    try
      {
        adaptee.setupSign(attributes);
      }
    catch (IllegalArgumentException x)
      {
        throw new InvalidKeyException(x.getMessage(), x);
      }
  }

  public void engineUpdate(byte b) throws SignatureException
  {
    try
      {
        adaptee.update(b);
      }
    catch (IllegalStateException x)
      {
        throw new SignatureException(x.getMessage(), x);
      }
  }

  public void engineUpdate(byte[] b, int off, int len)
      throws SignatureException
  {
    try
      {
        adaptee.update(b, off, len);
      }
    catch (IllegalStateException x)
      {
        throw new SignatureException(x.getMessage(), x);
      }
  }

  public byte[] engineSign() throws SignatureException
  {
    Object signature = null;
    try
      {
        signature = adaptee.sign();
      }
    catch (IllegalStateException x)
      {
        throw new SignatureException(x.getMessage(), x);
      }
    byte[] result = codec.encodeSignature(signature);
    return result;
  }

  public int engineSign(byte[] outbuf, int offset, int len)
      throws SignatureException
  {
    byte[] signature = this.engineSign();
    int result = signature.length;
    if (result > len)
      throw new SignatureException("Not enough room to store signature");

    System.arraycopy(signature, 0, outbuf, offset, result);
    return result;
  }

  public boolean engineVerify(byte[] sigBytes) throws SignatureException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineVerify");
    Object signature = codec.decodeSignature(sigBytes);
    boolean result = false;
    try
      {
        result = adaptee.verify(signature);
      }
    catch (IllegalStateException x)
      {
        throw new SignatureException(x.getMessage(), x);
      }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineVerify",
                  Boolean.valueOf(result));
    return result;
  }

  // Deprecated. Replaced by engineSetParameter.
  public void engineSetParameter(String param, Object value)
      throws InvalidParameterException
  {
    throw new InvalidParameterException("deprecated");
  }

  public void engineSetParameter(AlgorithmParameterSpec params)
      throws InvalidAlgorithmParameterException
  {
  }

  // Deprecated
  public Object engineGetParameter(String param)
      throws InvalidParameterException
  {
    throw new InvalidParameterException("deprecated");
  }
}
