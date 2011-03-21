/* CALG.java --
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.sasl.srp;

import gnu.java.security.Registry;
import gnu.javax.crypto.assembly.Assembly;
import gnu.javax.crypto.assembly.Cascade;
import gnu.javax.crypto.assembly.Direction;
import gnu.javax.crypto.assembly.Stage;
import gnu.javax.crypto.assembly.Transformer;
import gnu.javax.crypto.assembly.TransformerException;
import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.mode.IMode;
import gnu.javax.crypto.mode.ModeFactory;
import gnu.javax.crypto.pad.IPad;
import gnu.javax.crypto.pad.PadFactory;
import gnu.javax.crypto.sasl.ConfidentialityException;

import java.util.HashMap;

import javax.security.sasl.SaslException;

/**
 * A Factory class that returns CALG (Confidentiality Algorithm) instances that
 * operate as described in the draft-burdis-cat-sasl-srp-08.
 * <p>
 * The designated CALG block cipher should be used in OFB (Output Feedback
 * Block) mode in the ISO variant, as described in <i>The Handbook of Applied
 * Cryptography</i>, algorithm 7.20.
 * <p>
 * Let <code>k</code> be the block size of the chosen symmetric key block
 * cipher algorithm; e.g. for AES this is <code>128</code> bits or
 * <code>16</code> octets. The OFB mode used shall be of length/size
 * <code>k</code>.
 * <p>
 * It is recommended that block ciphers operating in OFB mode be used with an
 * Initial Vector (the mode's IV). In such a mode of operation - OFB with key
 * re-use - the IV need not be secret. For the mechanism in question the IVs
 * shall be a random octet sequence of <code>k</code> bytes.
 * <p>
 * The input data to the confidentiality protection algorithm shall be a
 * multiple of the symmetric cipher block size <code>k</code>. When the input
 * length is not a multiple of <code>k</code> octets, the data shall be padded
 * according to the following scheme:
 * <p>
 * Assuming the length of the input is <code>l</code> octets,
 * <code>(k - (l mod k))</code> octets, all having the value
 * <code>(k - (l mod k))</code>, shall be appended to the original data. In
 * other words, the input is padded at the trailing end with one of the
 * following sequences:
 * <pre>
 *
 *                     01 -- if l mod k = k-1
 *                    02 02 -- if l mod k = k-2
 *                              ...
 *                              ...
 *                              ...
 *                  k k ... k k -- if l mod k = 0
 * </pre>
 * <p>
 * The padding can be removed unambiguously since all input is padded and no
 * padding sequence is a suffix of another. This padding method is well-defined
 * if and only if <code>k &lt; 256</code> octets, which is the case with
 * symmetric key block ciphers today, and in the forseeable future.
 */
public final class CALG
{
  private Assembly assembly;
  private Object modeNdx; // initialisation key of the cascade's attributes
  private int blockSize; // the underlying cipher's blocksize == IV length
  private int keySize; // the underlying cipher's key size (in bytes).

  /** Private constructor to enforce instantiation through Factory method. */
  private CALG(final int blockSize, final int keySize, final Object modeNdx,
               final Assembly assembly)
  {
    super();

    this.blockSize = blockSize;
    this.keySize = keySize;
    this.modeNdx = modeNdx;
    this.assembly = assembly;
  }

  /**
   * Returns an instance of a SASL-SRP CALG implementation.
   *
   * @param algorithm the name of the symmetric cipher algorithm.
   * @return an instance of this object.
   */
  static synchronized CALG getInstance(final String algorithm)
  {
    final IBlockCipher cipher = CipherFactory.getInstance(algorithm);
    final int blockSize = cipher.defaultBlockSize();
    final int keySize = cipher.defaultKeySize();
    final Cascade ofbCipher = new Cascade();
    IMode ofbMode = ModeFactory.getInstance(Registry.OFB_MODE,
                                            cipher,
                                            blockSize);
    Stage modeStage = Stage.getInstance(ofbMode, Direction.FORWARD);
    final Object modeNdx = ofbCipher.append(modeStage);
    final IPad pkcs7 = PadFactory.getInstance(Registry.PKCS7_PAD);
    final Assembly asm = new Assembly();
    asm.addPreTransformer(Transformer.getCascadeTransformer(ofbCipher));
    asm.addPreTransformer(Transformer.getPaddingTransformer(pkcs7));
    return new CALG(blockSize, keySize, modeNdx, asm);
  }

  /**
   * Initialises a SASL-SRP CALG implementation.
   *
   * @param kdf the key derivation function.
   * @param iv the initial vector value to use.
   * @param dir whether this CALG is used for encryption or decryption.
   */
  public void init(final KDF kdf, final byte[] iv, final Direction dir)
      throws SaslException
  {
    final byte[] realIV;
    if (iv.length == blockSize)
      realIV = iv;
    else
      {
        realIV = new byte[blockSize];
        if (iv.length > blockSize)
          System.arraycopy(iv, 0, realIV, 0, blockSize);
        else // shouldnt happen
          System.arraycopy(iv, 0, realIV, 0, iv.length);
      }
    final HashMap modeAttributes = new HashMap();
    final byte[] sk = kdf.derive(keySize);
    modeAttributes.put(IBlockCipher.KEY_MATERIAL, sk);
    modeAttributes.put(IMode.IV, realIV);
    final HashMap attributes = new HashMap();
    attributes.put(Assembly.DIRECTION, dir);
    attributes.put(modeNdx, modeAttributes);
    try
      {
        assembly.init(attributes);
      }
    catch (TransformerException x)
      {
        throw new SaslException("getInstance()", x);
      }
  }

  /**
   * Encrypts or decrypts, depending on the mode already set, a designated array
   * of bytes and returns the result.
   *
   * @param data the data to encrypt/decrypt.
   * @return the decrypted/encrypted result.
   * @throws ConfidentialityException if an exception occurs duirng the process.
   */
  public byte[] doFinal(final byte[] data) throws ConfidentialityException
  {
    return doFinal(data, 0, data.length);
  }

  /**
   * Encrypts or decrypts, depending on the mode already set, a designated array
   * of bytes and returns the result.
   *
   * @param data the data to encrypt/decrypt.
   * @param offset where to start in <code>data</code>.
   * @param length how many bytes to consider in <code>data</code>.
   * @return the decrypted/encrypted result.
   * @throws ConfidentialityException if an exception occurs duirng the process.
   */
  public byte[] doFinal(final byte[] data, final int offset, final int length)
      throws ConfidentialityException
  {
    final byte[] result;
    try
      {
        result = assembly.lastUpdate(data, offset, length);
      }
    catch (TransformerException x)
      {
        throw new ConfidentialityException("doFinal()", x);
      }
    return result;
  }
}
