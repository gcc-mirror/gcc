/* AESWrap.java -- An implementation of RFC-3394 AES Key Wrap Algorithm
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.kwa;

import gnu.java.security.Registry;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.cipher.Rijndael;

import java.security.InvalidKeyException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * The GNU implementation of the AES Key Wrap Algorithm as described in [1].
 * <p>
 * References:
 * <ol>
 * <li><a href="http://csrc.nist.gov/encryption/kms/key-wrap.pdf"></a>.</li>
 * <li><a href="http://www.rfc-archive.org/getrfc.php?rfc=3394">Advanced
 * Encryption Standard (AES) Key Wrap Algorithm</a>.</li>
 * <li><a href="http://www.w3.org/TR/xmlenc-core/">XML Encryption Syntax and
 * Processing</a>.</li>
 * </ol>
 */
public class AESKeyWrap
    extends BaseKeyWrappingAlgorithm
{
  private static final byte[] DEFAULT_IV = new byte[] {
      (byte) 0xA6, (byte) 0xA6, (byte) 0xA6, (byte) 0xA6,
      (byte) 0xA6, (byte) 0xA6, (byte) 0xA6, (byte) 0xA6 };

  private Rijndael aes;
  private byte[] iv;

  public AESKeyWrap()
  {
    super(Registry.AES_KWA);

    aes = new Rijndael();
  }

  protected void engineInit(Map attributes) throws InvalidKeyException
  {
    Map cipherAttributes = new HashMap();
    cipherAttributes.put(IBlockCipher.CIPHER_BLOCK_SIZE, Integer.valueOf(16));
    cipherAttributes.put(IBlockCipher.KEY_MATERIAL,
                         attributes.get(KEY_ENCRYPTION_KEY_MATERIAL));
    aes.reset();
    aes.init(cipherAttributes);
    byte[] initialValue = (byte[]) attributes.get(INITIAL_VALUE);
    iv = initialValue == null ? DEFAULT_IV : (byte[]) initialValue.clone();
  }

  protected byte[] engineWrap(byte[] in, int inOffset, int length)
  {
    // TODO: handle input length which is not a multiple of 8 as suggested by
    // section 2.2.3.2 of RFC-3394
    if (length % 8 != 0)
      throw new IllegalArgumentException("Input length MUST be a multiple of 8");
    int n = length / 8;
    // output is always one block larger than input
    byte[] result = new byte[length + 8];

    // 1. init variables: we'll use out buffer for our work buffer;
    //    A will be the first block in out, while R will be the rest
    System.arraycopy(iv, 0, result, 0, 8);
    System.arraycopy(in, inOffset, result, 8, length);
    byte[] B = new byte[2 * 8];
    // 2. compute intermediate values
    long t;
    for (int j = 0; j < 6; j++)
      for (int i = 1; i <= n; i++)
        {
          System.arraycopy(result, 0, B, 0, 8);
          System.arraycopy(result, i * 8, B, 8, 8);
          aes.encryptBlock(B, 0, B, 0);
          t = (n * j) + i;
          result[0] = (byte)(B[0] ^ (t >>> 56));
          result[1] = (byte)(B[1] ^ (t >>> 48));
          result[2] = (byte)(B[2] ^ (t >>> 40));
          result[3] = (byte)(B[3] ^ (t >>> 32));
          result[4] = (byte)(B[4] ^ (t >>> 24));
          result[5] = (byte)(B[5] ^ (t >>> 16));
          result[6] = (byte)(B[6] ^ (t >>>  8));
          result[7] = (byte)(B[7] ^  t        );
          System.arraycopy(B, 8, result, i * 8, 8);
        }
    return result;
  }

  protected byte[] engineUnwrap(byte[] in, int inOffset, int length)
      throws KeyUnwrappingException
  {
    // TODO: handle input length which is not a multiple of 8 as suggested by
    // section 2.2.3.2 of RFC-3394
    if (length % 8 != 0)
      throw new IllegalArgumentException("Input length MUST be a multiple of 8");
    // output is always one block shorter than input
    byte[] result = new byte[length - 8];

    // 1. init variables: we'll use out buffer for our R work buffer
    byte[] A = new byte[8];
    System.arraycopy(in, inOffset, A, 0, 8);
    System.arraycopy(in, inOffset + 8, result, 0, result.length);
    byte[] B = new byte[2 * 8];
    // 2. compute intermediate values
    int n = length / 8 - 1;
    long t;
    for (int j = 5; j >= 0; j--)
      for (int i = n; i >= 1; i--)
        {
          t = (n * j) + i;
          B[0] = (byte)(A[0] ^ (t >>> 56));
          B[1] = (byte)(A[1] ^ (t >>> 48));
          B[2] = (byte)(A[2] ^ (t >>> 40));
          B[3] = (byte)(A[3] ^ (t >>> 32));
          B[4] = (byte)(A[4] ^ (t >>> 24));
          B[5] = (byte)(A[5] ^ (t >>> 16));
          B[6] = (byte)(A[6] ^ (t >>>  8));
          B[7] = (byte)(A[7] ^  t        );
          System.arraycopy(result, (i - 1) * 8, B, 8, 8);
          aes.decryptBlock(B, 0, B, 0);
          System.arraycopy(B, 0, A, 0, 8);
          System.arraycopy(B, 8, result, (i - 1) * 8, 8);
        }
    if (! Arrays.equals(A, iv))
      throw new KeyUnwrappingException();

    return result;
  }
}
