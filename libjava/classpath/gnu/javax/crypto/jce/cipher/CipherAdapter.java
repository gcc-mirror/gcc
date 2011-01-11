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

import gnu.java.security.Registry;
import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;
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
import java.util.Iterator;
import java.util.Map;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.CipherSpi;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.ShortBufferException;
import javax.crypto.spec.IvParameterSpec;

/**
 * The implementation of a generic {@link Cipher} <i>Adapter</i> class to wrap
 * GNU cipher instances.
 * <p>
 * This class defines the <i>Service Provider Interface</i> (<b>SPI</b>) for
 * the {@link Cipher} class, which provides the functionality of symmetric-key
 * block ciphers, such as the AES.
 * <p>
 * This base class defines all of the abstract methods in {@link CipherSpi},
 * but does not define the (non-abstract) key wrapping functions that extended
 * the base cipher SPI, and these methods thus immediately throw an
 * {@link UnsupportedOperationException}. If a cipher implementation provides
 * this functionality, or if it in fact accepts parameters other than the key
 * and the initialization vector, the subclass should override those methods.
 * Otherwise a subclass need only call the {@link #CipherAdapter(String)}
 * constructor with the name of the cipher.
 */
class CipherAdapter
    extends CipherSpi
{
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

  /**
   * Protected constructor to be called by subclasses. The cipher name argument
   * should be the appropriate one listed in {@link Registry}. The basic cipher
   * instance is created, along with an instance of the
   * {@link gnu.javax.crypto.mode.ECB} mode and no padding.
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
    attributes.put(IBlockCipher.CIPHER_BLOCK_SIZE, Integer.valueOf(blockLen));
  }

  /**
   * Creates a new cipher adapter with the default block size.
   *
   * @param cipherName The cipher to instantiate.
   */
  protected CipherAdapter(String cipherName)
  {
    cipher = CipherFactory.getInstance(cipherName);
    blockLen = cipher.defaultBlockSize();
    attributes = new HashMap();
    mode = ModeFactory.getInstance("ECB", cipher, blockLen);
    attributes.put(IBlockCipher.CIPHER_BLOCK_SIZE, Integer.valueOf(blockLen));
  }

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
                attributes.put(IMode.MODE_BLOCK_SIZE, Integer.valueOf(bs / 8));
              }
            catch (NumberFormatException nfe)
              {
                throw new NoSuchAlgorithmException(modeName);
              }
            modeName = "CFB";
          }
      }
    else
      attributes.remove(IMode.MODE_BLOCK_SIZE);
    mode = ModeFactory.getInstance(modeName, cipher, blockLen);
    if (mode == null)
      throw new NoSuchAlgorithmException(modeName);
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
      throw new NoSuchPaddingException(padName);
  }

  protected int engineGetBlockSize()
  {
    if (cipher != null)
      return blockLen;
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
      return null;
    return (byte[]) iv.clone();
  }

  protected AlgorithmParameters engineGetParameters()
  {
    byte[] iv = (byte[]) attributes.get(IMode.IV);
    int cipherBlockSize = cipher.currentBlockSize();
    BlockCipherParameterSpec spec = new BlockCipherParameterSpec(iv,
                                                                 cipherBlockSize,
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
    try
      {
        engineInit(opmode, key, (AlgorithmParameterSpec) null, random);
      }
    catch (InvalidAlgorithmParameterException e)
      {
        throw new InvalidKeyException(e.getMessage(), e);
      }
  }

  /**
   * Executes initialization logic after all parameters have been handled by the
   * engineInit()s.
   *
   * @param opmode the desired mode of operation for this instance.
   * @param key the key material to use for initialization.
   * @param random a source of randmoness to use if/when needed.
   * @throws InvalidKeyException if <code>key</code> is invalid or the cipher
   *           needs extra parameters which can not be derived from
   *           <code>key</code>; e.g. an IV.
   */
  private void engineInitHandler(int opmode, Key key, SecureRandom random)
      throws InvalidKeyException
  {
    switch (opmode)
      {
      case Cipher.ENCRYPT_MODE:
        attributes.put(IMode.STATE, Integer.valueOf(IMode.ENCRYPTION));
        break;
      case Cipher.DECRYPT_MODE:
        attributes.put(IMode.STATE, Integer.valueOf(IMode.DECRYPTION));
        break;
      }
    if (! key.getFormat().equalsIgnoreCase("RAW"))
      throw new InvalidKeyException("bad key format " + key.getFormat());
    byte[] kb = key.getEncoded();
    int kbLength = kb.length;
    if (keyLen == 0)
      {
        // no key-size given; instead key-material is provided in kb --which
        // can be more than what we need.  if we don't cull this down to what
        // the cipher likes/wants we may get an InvalidKeyException.
        //
        // try to find the largest key-size value that is less than or equal
        // to kbLength
        for (Iterator it = cipher.keySizes(); it.hasNext();)
          {
            int aKeySize = ((Integer) it.next()).intValue();
            if (aKeySize == kbLength)
              {
                keyLen = aKeySize;
                break;
              }
            else if (aKeySize < kbLength)
              keyLen = aKeySize;
            else // all remaining key-sizes are longer than kb.length
              break;
          }
      }
    if (keyLen == 0)
      {
        // we were unable to find a key-size, among those advertised by the
        // cipher, that is less than or equal to the length of the kb array.
        // set keyLen to kbLength.  either the cipher implementation will throw
        // an InvalidKeyException, or it is implemented in a way which can deal
        // with an unsupported key-size.
        keyLen = kbLength;
      }
    if (keyLen < kbLength)
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
        // All cipher modes require parameters (like an IV) except ECB. When
        // these cant be derived from the given key then it must be generated
        // randomly if in ENCRYPT or WRAP mode. Parameters that have defaults
        // for our cipher must be set to these defaults.
        if (! mode.name().toLowerCase().startsWith(Registry.ECB_MODE + "("))
          {
            switch (opmode)
              {
              case Cipher.ENCRYPT_MODE:
              case Cipher.WRAP_MODE:
                byte[] iv = new byte[blockLen];
                random.nextBytes(iv);
                attributes.put(IMode.IV, iv);
                break;
              default:
                throw new InvalidAlgorithmParameterException(
                    "Required algorithm parameters are missing for mode: "
                    + mode.name());
              }
          }
        // Add default for block length etc.
        blockLen = cipher.defaultBlockSize();
        attributes.put(IBlockCipher.CIPHER_BLOCK_SIZE,
                       Integer.valueOf(blockLen));
        keyLen = 0;
      }
    else if (params instanceof BlockCipherParameterSpec)
      {
        BlockCipherParameterSpec bcps = (BlockCipherParameterSpec) params;
        blockLen = bcps.getBlockSize();
        attributes.put(IBlockCipher.CIPHER_BLOCK_SIZE, Integer.valueOf(blockLen));
        attributes.put(IMode.IV, bcps.getIV());
        keyLen = bcps.getKeySize();
      }
    else if (params instanceof IvParameterSpec)
      {
        // The size of the IV must match the block size
        if (((IvParameterSpec) params).getIV().length != cipher.defaultBlockSize())
          {
            throw new InvalidAlgorithmParameterException();
          }

        attributes.put(IMode.IV, ((IvParameterSpec) params).getIV());
        blockLen = cipher.defaultBlockSize();
        attributes.put(IBlockCipher.CIPHER_BLOCK_SIZE, Integer.valueOf(blockLen));
        keyLen = 0;
      }
    engineInitHandler(opmode, key, random);
  }

  protected void engineInit(int opmode, Key key, AlgorithmParameters params,
                            SecureRandom random) throws InvalidKeyException,
      InvalidAlgorithmParameterException
  {
    AlgorithmParameterSpec spec = null;
    try
      {
        if (params != null)
          spec = params.getParameterSpec(BlockCipherParameterSpec.class);
      }
    catch (InvalidParameterSpecException ignored)
      {
      }
    engineInit(opmode, key, spec, random);
  }

  protected byte[] engineUpdate(byte[] input, int inOff, int inLen)
  {
    if (inLen == 0) // nothing to process
      return new byte[0];
    final int blockSize = mode.currentBlockSize();
    int blockCount = (partLen + inLen) / blockSize;

    // always keep data for unpadding in padded decryption mode;
    // might even be a complete block
    if (pad != null
        && ((Integer) attributes.get(IMode.STATE)).intValue() == IMode.DECRYPTION
        && (partLen + inLen) % blockSize == 0)
      blockCount--;

    final byte[] out = new byte[blockCount * blockSize];
    try
      {
        engineUpdate(input, inOff, inLen, out, 0);
      }
    catch (ShortBufferException x) // should not happen
      {
        x.printStackTrace(System.err);
      }
    return out;
  }

  protected int engineUpdate(byte[] in, int inOff, int inLen, byte[] out,
                             int outOff) throws ShortBufferException
  {
    if (inLen == 0) // nothing to process
      return 0;
    final int blockSize = mode.currentBlockSize();
    int blockCount = (partLen + inLen) / blockSize;

    // always keep data for unpadding in padded decryption mode;
    // might even be a complete block
    if (pad != null
        && ((Integer) attributes.get(IMode.STATE)).intValue() == IMode.DECRYPTION
        && (partLen + inLen) % blockSize == 0)
      blockCount--;

    final int result = blockCount * blockSize;
    if (result > out.length - outOff)
      throw new ShortBufferException();
    if (blockCount == 0) // not enough bytes for even 1 block
      {
        System.arraycopy(in, inOff, partBlock, partLen, inLen);
        partLen += inLen;
        return 0;
      }
    final byte[] buf;
    // we have enough bytes for at least 1 block
    if (partLen == 0) // if no cached bytes use input
      buf = in;
    else // prefix input with cached bytes
      {
        buf = new byte[partLen + inLen];
        System.arraycopy(partBlock, 0, buf, 0, partLen);
        if (in != null && inLen > 0)
          System.arraycopy(in, inOff, buf, partLen, inLen);
        inOff = 0;
      }
    for (int i = 0; i < blockCount; i++) // update blockCount * blockSize
      {
        mode.update(buf, inOff, out, outOff);
        inOff += blockSize;
        outOff += blockSize;
      }
    partLen += inLen - result;
    if (partLen > 0) // cache remaining bytes from buf
      System.arraycopy(buf, inOff, partBlock, 0, partLen);
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
            byte[] buf3 = new byte[buf.length + partLen];
            try
              {
                if (partLen != mode.currentBlockSize())
                  throw new WrongPaddingException();
                System.arraycopy(buf, 0, buf3, 0, buf.length);
                mode.update(partBlock, 0, buf3, buf.length);
                padLen = pad.unpad(buf3, 0, buf3.length);
              }
            catch (WrongPaddingException wpe)
              {
                throw new BadPaddingException(wpe.getMessage());
              }
            result = new byte[buf3.length - padLen];
            System.arraycopy(buf3, 0, result, 0, result.length);
            break;
          default:
            throw new IllegalStateException();
          }
      }
    else
      {
        if (partLen > 0)
          throw new IllegalBlockSizeException(partLen + " trailing bytes");
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
      throw new ShortBufferException();
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
