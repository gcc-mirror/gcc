/* SealedObject.java -- An encrypted Serializable object.
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package javax.crypto;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;

/**
 * This class allows any {@link java.io.Serializable} object to be
 * stored in an encrypted form.
 *
 * <p>When the sealed object is ready to be unsealed (and deserialized)
 * the caller may use either
 *
 * <ol>
 * <li>{@link #getObject(javax.crypto.Cipher)}, which uses an
 * already-initialized {@link javax.crypto.Cipher}.<br>
 * <br>
 * or,</li>
 *
 * <li>{@link #getObject(java.security.Key)} or {@link
 * #getObject(java.security.Key,java.lang.String)}, which will
 * initialize a new cipher instance with the {@link #encodedParams} that
 * were stored with this sealed object (this is so parameters, such as
 * the IV, don't need to be known by the one unsealing the object).</li>
 * </ol>
 *
 * @author Casey Marshall (csm@gnu.org)
 * @since 1.4
 */
public class SealedObject implements Serializable
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  /** The encoded algorithm parameters. */
  protected byte[] encodedParams;

  /** The serialized, encrypted object. */
  private byte[] encryptedContent;

  /** The algorithm used to seal the object. */
  private String sealAlg;

  /** The parameter type. */
  private String paramsAlg;

  /** The cipher that decrypts when this object is unsealed. */
  private transient Cipher sealCipher;

  /** Compatible with JDK1.4. */
  private static final long serialVersionUID = 4482838265551344752L;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new sealed object from a {@link java.io.Serializable}
   * object and a cipher.
   *
   * @param object The object to seal.
   * @param cipher The cipher to encrypt with.
   * @throws java.io.IOException If serializing the object fails.
   * @throws javax.crypto.IllegalBlockSizeException If the cipher has no
   *         padding and the size of the serialized representation of the
   *         object is not a multiple of the cipher's block size.
   */
  public SealedObject(Serializable object, Cipher cipher)
    throws IOException, IllegalBlockSizeException
  {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ObjectOutputStream oos = new ObjectOutputStream(baos);
    oos.writeObject(object);
    oos.flush();
    try
      {
        encryptedContent = cipher.doFinal(baos.toByteArray());
      }
    catch (IllegalStateException ise)
      {
        throw new IOException("cipher not in proper state");
      }
    catch (BadPaddingException bpe)
      {
        throw new IOException(
          "encrypting but got javax.crypto.BadPaddingException");
      }
    sealAlg = cipher.getAlgorithm();
    encodedParams = cipher.getParameters().getEncoded();
    paramsAlg = cipher.getParameters().getAlgorithm();
  }

  /**
   * Create a new sealed object from another sealed object.
   *
   * @param so The other sealed object.
   */
  protected SealedObject(SealedObject so)
  {
    this.encodedParams = (byte[]) so.encodedParams.clone();
    this.encryptedContent = (byte[]) so.encryptedContent.clone();
    this.sealAlg = so.sealAlg;
    this.paramsAlg = so.paramsAlg;
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Get the name of the algorithm used to seal this object.
   *
   * @return The algorithm's name.
   */
  public final String getAlgorithm()
  {
    return sealAlg;
  }

  /**
   * Unseal and deserialize this sealed object with a specified (already
   * initialized) cipher.
   *
   * @param cipher The cipher to decrypt with.
   * @return The original object.
   * @throws java.io.IOException If reading fails.
   * @throws java.lang.ClassNotFoundException If deserialization fails.
   * @throws javax.crypto.IllegalBlockSizeException If the cipher has no
   *         padding and the encrypted data is not a multiple of the
   *         cipher's block size.
   * @throws javax.crypto.BadPaddingException If the padding bytes are
   *         incorrect.
   */
  public final Object getObject(Cipher cipher)
    throws IOException, ClassNotFoundException, IllegalBlockSizeException,
           BadPaddingException
  {
    sealCipher = cipher;
    return unseal();
  }

  /**
   * Unseal and deserialize this sealed object with the specified key.
   *
   * @param key The key to decrypt with.
   * @return The original object.
   * @throws java.io.IOException If reading fails.
   * @throws java.lang.ClassNotFoundException If deserialization fails.
   * @throws java.security.InvalidKeyException If the supplied key
   *         cannot be used to unseal this object.
   * @throws java.security.NoSuchAlgorithmException If the algorithm
   *         used to originally seal this object is not available.
   */
  public final Object getObject(Key key)
    throws IOException, ClassNotFoundException, InvalidKeyException,
           NoSuchAlgorithmException
  {
    try
      {
        if (sealCipher == null)
          sealCipher = Cipher.getInstance(sealAlg);
      }
    catch (NoSuchPaddingException nspe)
      {
        throw new NoSuchAlgorithmException(nspe.getMessage());
      }
    AlgorithmParameters params = null;
    if (encodedParams != null)
      {
        params = AlgorithmParameters.getInstance(paramsAlg);
        params.init(encodedParams);
      }
    try
      {
        sealCipher.init(Cipher.DECRYPT_MODE, key, params);
        return unseal();
      }
    catch (InvalidAlgorithmParameterException iape)
      {
        throw new IOException("bad parameters");
      }
    catch (IllegalBlockSizeException ibse)
      {
        throw new IOException("illegal block size");
      }
    catch (BadPaddingException bpe)
      {
        throw new IOException("bad padding");
      }
  }

  /**
   * Unseal and deserialize this sealed object with the specified key,
   * using a cipher from the named provider.
   *
   * @param key      The key to decrypt with.
   * @param provider The name of the provider to use.
   * @return The original object.
   * @throws java.io.IOException If reading fails.
   * @throws java.lang.ClassNotFoundException If deserialization fails.
   * @throws java.security.InvalidKeyException If the supplied key
   *         cannot be used to unseal this object.
   * @throws java.security.NoSuchAlgorithmException If the algorithm
   *         used to originally seal this object is not available from
   *         the named provider.
   * @throws java.security.NoSuchProviderException If the named provider
   *         does not exist.
   */
  public final Object getObject(Key key, String provider)
    throws IOException, ClassNotFoundException, InvalidKeyException,
           NoSuchAlgorithmException, NoSuchProviderException
  {
    try
      {
        sealCipher = Cipher.getInstance(sealAlg, provider);
      }
    catch (NoSuchPaddingException nspe)
      {
        throw new NoSuchAlgorithmException(nspe.getMessage());
      }
    AlgorithmParameters params = null;
    if (encodedParams != null)
      {
        params = AlgorithmParameters.getInstance(paramsAlg, provider);
        params.init(encodedParams);
      }
    try
      {
        sealCipher.init(Cipher.DECRYPT_MODE, key, params);
        return unseal();
      }
    catch (InvalidAlgorithmParameterException iape)
      {
        throw new IOException("bad parameters");
      }
    catch (IllegalBlockSizeException ibse)
      {
        throw new IOException("illegal block size");
      }
    catch (BadPaddingException bpe)
      {
        throw new IOException("bad padding");
      }
  }

  // Own methods.
  // ------------------------------------------------------------------------

  /**
   * Deserialize this object.
   *
   * @param ois The input stream.
   * @throws java.io.IOException If reading fails.
   * @throws java.lang.ClassNotFoundException If reading fails.
   */
  private void readObject(ObjectInputStream ois)
    throws IOException, ClassNotFoundException
  {
    encodedParams = (byte[]) ois.readObject();
    encryptedContent = (byte[]) ois.readObject();
    sealAlg = (String) ois.readObject();
    paramsAlg = (String) ois.readObject();
  }

  /**
   * Serialize this object.
   *
   * @param oos The output stream.
   * @throws java.io.IOException If writing fails.
   */
  private void writeObject(ObjectOutputStream oos)
    throws IOException
  {
    oos.writeObject(encodedParams);
    oos.writeObject(encryptedContent);
    oos.writeObject(sealAlg);
    oos.writeObject(paramsAlg);
  }

  /**
   * Unseal this object, returning it.
   *
   * @return The unsealed, deserialized Object.
   * @throws java.io.IOException If reading fails.
   * @throws java.io.ClassNotFoundException If reading fails.
   * @throws javax.crypto.IllegalBlockSizeException If the cipher has no
   *         padding and the encrypted data is not a multiple of the
   *         cipher's block size.
   * @throws javax.crypto.BadPaddingException If the padding bytes are
   *         incorrect.
   */
  private Object unseal()
    throws IOException, ClassNotFoundException, IllegalBlockSizeException,
           BadPaddingException
  {
    ByteArrayInputStream bais = null;
    try
      {
        bais = new ByteArrayInputStream(sealCipher.doFinal(encryptedContent));
      }
    catch (IllegalStateException ise)
      {
        throw new IOException("cipher not initialized");
      }
    ObjectInputStream ois = new ObjectInputStream(bais);
    return ois.readObject();
  }
}
