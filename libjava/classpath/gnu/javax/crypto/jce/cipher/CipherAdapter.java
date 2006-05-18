/* CipherAdapter.java -- 
   Copyright (C) 2002, 2003, 2006  Free Software Foundation, Inc.

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


package gnu.javax.crypto.jce.cipher;

import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.jce.spec.BlockCipherParameterSpec;
import gnu.javax.crypto.mode.IMode;
import gnu.javax.crypto.mode.ModeFactory;
import gnu.javax.crypto.pad.IPad;
import gnu.javax.crypto.pad.PadFactory;
import gnu.javax.crypto.pad.WrongPaddingException;

import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.InvalidParameterSpecException;
import java.util.HashMap;
import java.util.Map;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.CipherSpi;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.ShortBufferException;
import javax.crypto.spec.IvParameterSpec;

/**
 * <p>The implementation of a generic {@link Cipher} <i>Adapter</i> class to
 * wrap GNU Crypto cipher instances.</p>
 *
 * <p>This class defines the <i>Service Provider Interface</i> (<b>SPI</b>) for
 * the {@link Cipher} class, which provides the functionality of symmetric-key
 * block ciphers, such as the AES.<p>
 *
 * <p>This base class defines all of the abstract methods in {@link CipherSpi},
 * but does not define the (non-abstract) key wrapping functions that extended
 * the base cipher SPI, and these methods thus immediately throw an
 * {@link UnsupportedOperationException}. If a cipher implementation provides
 * this functionality, or if it in fact accepts parameters other than the key
 * and the initialization vector, the subclass should override those methods.
 * Otherwise a subclass need only call the {@link #CipherAdapter(String)}
 * constructor with the name of the cipher.</p>
 */
class CipherAdapter extends CipherSpi
{

  // Constants and variables.
  // -------------------------------------------------------------------------

  /** Our cipher instance. */
  protected IBlockCipher cipher;

  /** Our mode instance. */
  protected IMode mode;

  /** Our padding instance. */
  protected IPad pad;

  /** The current key size. */
  protected int keyLen;

  /** Our attributes map. */
  protected Map attributes;

  /** An incomplete block. */
  protected byte[] partBlock;

  /** The number of bytes in {@link #partBlock}. */
  protected int partLen;

  /** The length of blocks we are processing. */
  protected int blockLen;

  // Constructor(s)
  // -------------------------------------------------------------------------

  /**
   * <p>Protected constructor to be called by subclasses. The cipher name
   * argument should be the appropriate one listed in {@link gnu.crypto.Registry}.
   * The basic cipher instance is created, along with an instance of the
   * {@link gnu.crypto.mode.ECB} mode and no padding.</p>
   *
   * @param cipherName The cipher to instantiate.
   * @param blockLen The block length to use.
   */
  protected CipherAdapter(String cipherName, int blockLen)
  {
    cipher = CipherFactory.getInstance(cipherName);
    attributes = new HashMap();
    this.blockLen = blockLen;
    mode = ModeFactory.getInstance("ECB", cipher, blockLen);
    attributes.put(IBlockCipher.CIPHER_BLOCK_SIZE, new Integer(blockLen));
  }

  /**
   * <p>Creates a new cipher adapter with the default block size.</p>
   *
   * @param cipherName The cipher to instantiate.
   */
  protected CipherAdapter(String cipherName)
  {
    cipher = CipherFactory.getInstance(cipherName);
    blockLen = cipher.defaultBlockSize();
    attributes = new HashMap();
    mode = ModeFactory.getInstance("ECB", cipher, blockLen);
    attributes.put(IBlockCipher.CIPHER_BLOCK_SIZE, new Integer(blockLen));
  }

  // Instance methods implementing javax.crypto.CipherSpi.
  // -------------------------------------------------------------------------

  protected void engineSetMode(String modeName) throws NoSuchAlgorithmException
  {
    if (modeName.length() >= 3
        && modeName.substring(0, 3).equalsIgnoreCase("CFB"))
      {
        if (modeName.length() > 3)
          {
            try
              {
                int bs = Integer.parseInt(modeName.substring(3));
                attributes.put(IMode.MODE_BLOCK_SIZE, new Integer(bs / 8));
              }
            catch (NumberFormatException nfe)
              {
                throw new NoSuchAlgorithmException(modeName);
              }
            modeName = "CFB";
          }
      }
    else
      {
        attributes.remove(IMode.MODE_BLOCK_SIZE);
      }
    mode = ModeFactory.getInstance(modeName, cipher, blockLen);
    if (mode == null)
      {
        throw new NoSuchAlgorithmException(modeName);
      }
  }

  protected void engineSetPadding(String padName) throws NoSuchPaddingException
  {
    if (padName.equalsIgnoreCase("NoPadding"))
      {
        pad = null;
        return;
      }
    pad = PadFactory.getInstance(padName);
    if (pad == null)
      {
        throw new NoSuchPaddingException(padName);
      }
  }

  protected int engineGetBlockSize()
  {
    if (cipher != null)
      {
        return blockLen;
      }
    return 0;
  }

  protected int engineGetOutputSize(int inputLen)
  {
    final int blockSize = mode.currentBlockSize();
    return ((inputLen + partLen) / blockSize) * blockSize;
  }

  protected byte[] engineGetIV()
  {
    byte[] iv = (byte[]) attributes.get(IMode.IV);
    if (iv == null)
      {
        return null;
      }
    return (byte[]) iv.clone();
  }

  protected AlgorithmParameters engineGetParameters()
  {
    BlockCipherParameterSpec spec = new BlockCipherParameterSpec(
                                                                 (byte[]) attributes.get(IMode.IV),
                                                                 cipher.currentBlockSize(),
                                                                 keyLen);
    AlgorithmParameters params;
    try
      {
        params = AlgorithmParameters.getInstance("BlockCipherParameters");
        params.init(spec);
      }
    catch (NoSuchAlgorithmException nsae)
      {
        return null;
      }
    catch (InvalidParameterSpecException ipse)
      {
        return null;
      }
    return params;
  }

  protected void engineInit(int opmode, Key key, SecureRandom random)
      throws InvalidKeyException
  {
    switch (opmode)
      {
      case Cipher.ENCRYPT_MODE:
        attributes.put(IMode.STATE, new Integer(IMode.ENCRYPTION));
        break;
      case Cipher.DECRYPT_MODE:
        attributes.put(IMode.STATE, new Integer(IMode.DECRYPTION));
        break;
      }
    if (!key.getFormat().equalsIgnoreCase("RAW"))
      {
        throw new InvalidKeyException("bad key format " + key.getFormat());
      }
    byte[] kb = key.getEncoded();
    if (keyLen == 0)
      {
        keyLen = kb.length;
      }
    else if (keyLen < kb.length)
      {
        byte[] kbb = kb;
        kb = new byte[keyLen];
        System.arraycopy(kbb, 0, kb, 0, keyLen);
      }
    attributes.put(IBlockCipher.KEY_MATERIAL, kb);
    reset();
  }

  protected void engineInit(int opmode, Key key, AlgorithmParameterSpec params,
                            SecureRandom random) throws InvalidKeyException,
      InvalidAlgorithmParameterException
  {
    if (params == null)
      {
        byte[] iv = new byte[blockLen];
        random.nextBytes(iv);
        attributes.put(IMode.IV, iv);
        blockLen = cipher.defaultBlockSize();
        attributes.put(IBlockCipher.CIPHER_BLOCK_SIZE, new Integer(blockLen));
        keyLen = 0;
      }
    else if (params instanceof BlockCipherParameterSpec)
      {
        attributes.put(
                       IBlockCipher.CIPHER_BLOCK_SIZE,
                       new Integer(
                                   ((BlockCipherParameterSpec) params).getBlockSize()));
        attributes.put(IMode.IV, ((BlockCipherParameterSpec) params).getIV());
        keyLen = ((BlockCipherParameterSpec) params).getKeySize();
        blockLen = ((BlockCipherParameterSpec) params).getBlockSize();
      }
    else if (params instanceof IvParameterSpec)
      {
        attributes.put(IMode.IV, ((IvParameterSpec) params).getIV());
        blockLen = cipher.defaultBlockSize();
        attributes.put(IBlockCipher.CIPHER_BLOCK_SIZE, new Integer(blockLen));
        keyLen = 0;
      }
    engineInit(opmode, key, random);
  }

  protected void engineInit(int opmode, Key key, AlgorithmParameters params,
                            SecureRandom random) throws InvalidKeyException,
      InvalidAlgorithmParameterException
  {
    AlgorithmParameterSpec spec = null;
    try
      {
        if (params != null)
          {
            spec = params.getParameterSpec(BlockCipherParameterSpec.class);
          }
      }
    catch (InvalidParameterSpecException ignored)
      {
      }
    engineInit(opmode, key, spec, random);
  }

  protected byte[] engineUpdate(byte[] input, int off, int len)
  {
    final int blockSize = mode.currentBlockSize();
    final int count = (partLen + len) / blockSize;
    final byte[] out = new byte[count * blockSize];
    try
      {
        engineUpdate(input, off, len, out, 0);
      }
    catch (ShortBufferException x)
      { // should not happen
        x.printStackTrace(System.err);
      }
    return out;
  }

  //   protected int
  //   engineUpdate(byte[] in, int inOff, int inLen, byte[] out, int outOff)
  //   throws ShortBufferException
  //   {
  //      int blockSize = mode.currentBlockSize();
  //      int count = (partLen + inLen) / blockSize;
  //      if (count * blockSize > out.length - outOff) {
  //         throw new ShortBufferException();
  //      }
  //      byte[] buf;
  //      if (partLen > 0 && count > 0) {
  //         buf = new byte[partLen + inLen];
  //         System.arraycopy(partBlock, 0, buf, 0, partLen);
  //         if (in != null && inLen > 0) {
  //            System.arraycopy(in, inOff, buf, partLen, inLen);
  //         }
  //         partLen = 0;
  //         inOff = 0;
  //      } else {
  //         buf = in;
  //      }
  //      for (int i = 0; i < count; i++) {
  //         mode.update(buf, i * blockSize + inOff, out, i * blockSize + outOff);
  //      }
  //      if (inOff + inLen > count * blockSize) {
  //         partLen = (inOff + inLen) - (count * blockSize);
  //         System.arraycopy(in, count * blockSize, partBlock, 0, partLen);
  //      }
  //      return count * blockSize;
  //   }

  protected int engineUpdate(byte[] in, int inOff, int inLen, byte[] out,
                             int outOff) throws ShortBufferException
  {
    if (inLen == 0)
      { // nothing to process
        return 0;
      }
    final int blockSize = mode.currentBlockSize();
    final int blockCount = (partLen + inLen) / blockSize;
    final int result = blockCount * blockSize;
    if (result > out.length - outOff)
      {
        throw new ShortBufferException();
      }
    if (blockCount == 0)
      { // not enough bytes for even 1 block
        System.arraycopy(in, inOff, partBlock, partLen, inLen);
        partLen += inLen;
        return 0;
      }
    final byte[] buf;
    // we have enough bytes for at least 1 block
    if (partLen == 0)
      { // if no cached bytes use input
        buf = in;
      }
    else
      { // prefix input with cached bytes
        buf = new byte[partLen + inLen];
        System.arraycopy(partBlock, 0, buf, 0, partLen);
        if (in != null && inLen > 0)
          {
            System.arraycopy(in, inOff, buf, partLen, inLen);
          }
        inOff = 0;
      }
    for (int i = 0; i < blockCount; i++)
      { // update blockCount * blockSize
        mode.update(buf, inOff, out, outOff);
        inOff += blockSize;
        outOff += blockSize;
      }
    partLen += inLen - result;
    if (partLen > 0)
      { // cache remaining bytes from buf
        System.arraycopy(buf, inOff, partBlock, 0, partLen);
      }
    return result;
  }

  protected byte[] engineDoFinal(byte[] input, int off, int len)
      throws IllegalBlockSizeException, BadPaddingException
  {
    final byte[] result;
    final byte[] buf = engineUpdate(input, off, len);
    if (pad != null)
      {
        switch (((Integer) attributes.get(IMode.STATE)).intValue())
          {
          case IMode.ENCRYPTION:
            byte[] padding = pad.pad(partBlock, 0, partLen);
            byte[] buf2 = engineUpdate(padding, 0, padding.length);
            result = new byte[buf.length + buf2.length];
            System.arraycopy(buf, 0, result, 0, buf.length);
            System.arraycopy(buf2, 0, result, buf.length, buf2.length);
            break;
          case IMode.DECRYPTION:
            int padLen;
            try
              {
                padLen = pad.unpad(buf, 0, buf.length);
              }
            catch (WrongPaddingException wpe)
              {
                throw new BadPaddingException(wpe.getMessage());
              }
            result = new byte[buf.length - padLen];
            System.arraycopy(buf, 0, result, 0, result.length);
            break;
          default:
            throw new IllegalStateException();
          }
      }
    else
      {
        if (partLen > 0)
          {
            throw new IllegalBlockSizeException(partLen + " trailing bytes");
          }
        result = buf;
      }

    try
      {
        reset();
      }
    catch (InvalidKeyException ike)
      {
        // Should not happen; if we initialized it with the current
        // parameters before, we should be able to do it again.
        throw new Error(ike);
      }
    return result;
  }

  protected int engineDoFinal(byte[] in, int inOff, int inLen, byte[] out,
                              int outOff) throws BadPaddingException,
      IllegalBlockSizeException, ShortBufferException
  {
    byte[] buf = engineDoFinal(in, inOff, inLen);
    if (out.length + outOff < buf.length)
      {
        throw new ShortBufferException();
      }
    System.arraycopy(buf, 0, out, outOff, buf.length);
    return buf.length;
  }

  private void reset() throws InvalidKeyException
  {
    mode.reset();
    mode.init(attributes);
    if (pad != null)
      {
        pad.reset();
        pad.init(blockLen);
      }
    partBlock = new byte[blockLen];
    partLen = 0;
  }
}
