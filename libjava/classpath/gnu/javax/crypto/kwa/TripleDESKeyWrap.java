/* TripleDESKeyWrap.java -- FIXME: briefly describe file purpose
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
import gnu.java.security.hash.Sha160;
import gnu.javax.crypto.assembly.Assembly;
import gnu.javax.crypto.assembly.Cascade;
import gnu.javax.crypto.assembly.Direction;
import gnu.javax.crypto.assembly.Stage;
import gnu.javax.crypto.assembly.Transformer;
import gnu.javax.crypto.assembly.TransformerException;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.cipher.TripleDES;
import gnu.javax.crypto.mode.IMode;
import gnu.javax.crypto.mode.ModeFactory;

import java.security.InvalidKeyException;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * The GNU implementation of the Triple DES Key Wrap Algorithm as described in
 * [1].
 * <p>
 * <b>IMPORTANT</b>: This class is NOT thread safe.
 * <p>
 * References:
 * <ol>
 * <li><a href="http://www.rfc-archive.org/getrfc.php?rfc=3217">Triple-DES and
 * RC2 Key Wrapping</a>.</li>
 * <li><a href="http://www.w3.org/TR/xmlenc-core/">XML Encryption Syntax and
 * Processing</a>.</li>
 * </ol>
 */
public class TripleDESKeyWrap
    extends BaseKeyWrappingAlgorithm
{
  private static final byte[] DEFAULT_IV = new byte[] {
     (byte) 0x4A, (byte) 0xDD, (byte) 0xA2, (byte) 0x2C,
     (byte) 0x79, (byte) 0xE8, (byte) 0x21, (byte) 0x05 };

  private Assembly asm;
  private HashMap asmAttributes = new HashMap();
  private HashMap modeAttributes = new HashMap();
  private Sha160 sha = new Sha160();
  private SecureRandom rnd;

  public TripleDESKeyWrap()
  {
    super(Registry.TRIPLEDES_KWA);
  }

  protected void engineInit(Map attributes) throws InvalidKeyException
  {
    rnd = (SecureRandom) attributes.get(IKeyWrappingAlgorithm.SOURCE_OF_RANDOMNESS);
    IMode des3CBC = ModeFactory.getInstance(Registry.CBC_MODE, new TripleDES(), 8);
    Stage des3CBCStage = Stage.getInstance(des3CBC, Direction.FORWARD);
    Cascade cascade = new Cascade();
    Object modeNdx = cascade.append(des3CBCStage);

    asmAttributes.put(modeNdx, modeAttributes);

    asm = new Assembly();
    asm.addPreTransformer(Transformer.getCascadeTransformer(cascade));

    modeAttributes.put(IBlockCipher.KEY_MATERIAL,
                       attributes.get(KEY_ENCRYPTION_KEY_MATERIAL));
    asmAttributes.put(Assembly.DIRECTION, Direction.FORWARD);
  }

  protected byte[] engineWrap(byte[] in, int inOffset, int length)
  {
    // The same key wrap algorithm is used for both Two-key Triple-DES and
    // Three-key Triple-DES keys.  When a Two-key Triple-DES key is to be
    // wrapped, a third DES key with the same value as the first DES key is
    // created.  Thus, all wrapped Triple-DES keys include three DES keys.
    if (length != 16 && length != 24)
      throw new IllegalArgumentException("Only 2- and 3-key Triple DES keys are alowed");

    byte[] CEK = new byte[24];
    if (length == 16)
      {
        System.arraycopy(in, inOffset, CEK, 0,  16);
        System.arraycopy(in, inOffset, CEK, 16, 8);
      }
    else
      System.arraycopy(in, inOffset, CEK, 0, 24);
    
    // TODO: check for the following:
    // However, a Two-key Triple-DES key MUST NOT be used to wrap a Three-
    // key Triple-DES key that is comprised of three unique DES keys.

    // 1. Set odd parity for each of the DES key octets comprising the
    //    Three-Key Triple-DES key that is to be wrapped, call the result
    //    CEK.
    TripleDES.adjustParity(CEK, 0);

    // 2. Compute an 8 octet key checksum value on CEK as described above in
    //    Section 2, call the result ICV.
    sha.update(CEK);
    byte[] hash = sha.digest();
    byte[] ICV = new byte[8];
    System.arraycopy(hash, 0, ICV, 0, 8);

    // 3. Let CEKICV = CEK || ICV.
    byte[] CEKICV = new byte[CEK.length + ICV.length];
    System.arraycopy(CEK, 0, CEKICV, 0,          CEK.length);
    System.arraycopy(ICV, 0, CEKICV, CEK.length, ICV.length);

    // 4. Generate 8 octets at random, call the result IV.
    byte[] IV = new byte[8];
    nextRandomBytes(IV);

    // 5. Encrypt CEKICV in CBC mode using the key-encryption key.  Use the
    //    random value generated in the previous step as the initialization
    //    vector (IV).  Call the ciphertext TEMP1.
    modeAttributes.put(IMode.IV, IV);
    asmAttributes.put(Assembly.DIRECTION, Direction.FORWARD);
    byte[] TEMP1;
    try
      {
        asm.init(asmAttributes);
        TEMP1 = asm.lastUpdate(CEKICV);
      }
    catch (TransformerException x)
      {
        throw new RuntimeException(x);
      }

    // 6. Let TEMP2 = IV || TEMP1.
    byte[] TEMP2 = new byte[IV.length + TEMP1.length];
    System.arraycopy(IV,    0, TEMP2, 0,         IV.length);
    System.arraycopy(TEMP1, 0, TEMP2, IV.length, TEMP1.length);

    // 7. Reverse the order of the octets in TEMP2.  That is, the most
    //    significant (first) octet is swapped with the least significant
    //    (last) octet, and so on.  Call the result TEMP3.
    byte[] TEMP3 = new byte[TEMP2.length];
    for (int i = 0, j = TEMP2.length - 1; i < TEMP2.length; i++, j--)
      TEMP3[j] = TEMP2[i];

    // 8. Encrypt TEMP3 in CBC mode using the key-encryption key.  Use an
    //    initialization vector (IV) of 0x4adda22c79e82105.  The ciphertext
    //    is 40 octets long.
    modeAttributes.put(IMode.IV, DEFAULT_IV);
    asmAttributes.put(Assembly.DIRECTION, Direction.FORWARD);
    byte[] result;
    try
      {
        asm.init(asmAttributes);
        result = asm.lastUpdate(TEMP3);
      }
    catch (TransformerException x)
      {
        throw new RuntimeException(x);
      }
    return result;
  }

  protected byte[] engineUnwrap(byte[] in, int inOffset, int length)
      throws KeyUnwrappingException
  {
    // 1. If the wrapped key is not 40 octets, then error.
    if (length != 40)
      throw new IllegalArgumentException("length MUST be 40");

    // 2. Decrypt the wrapped key in CBC mode using the key-encryption key.
    //    Use an initialization vector (IV) of 0x4adda22c79e82105.  Call the
    //    output TEMP3.
    modeAttributes.put(IMode.IV, DEFAULT_IV);
    asmAttributes.put(Assembly.DIRECTION, Direction.REVERSED);
    byte[] TEMP3;
    try
      {
        asm.init(asmAttributes);
        TEMP3 = asm.lastUpdate(in, inOffset, 40);
      }
    catch (TransformerException x)
      {
        throw new RuntimeException(x);
      }

    // 3. Reverse the order of the octets in TEMP3.  That is, the most
    //    significant (first) octet is swapped with the least significant
    //    (last) octet, and so on.  Call the result TEMP2.
    byte[] TEMP2 = new byte[40];
    for (int i = 0, j = 40 - 1; i < 40; i++, j--)
      TEMP2[j] = TEMP3[i];

    // 4. Decompose TEMP2 into IV and TEMP1.  IV is the most significant
    //    (first) 8 octets, and TEMP1 is the least significant (last) 32
    //    octets.
    byte[] IV = new byte[8];
    byte[] TEMP1 = new byte[32];
    System.arraycopy(TEMP2, 0, IV,    0, 8);
    System.arraycopy(TEMP2, 8, TEMP1, 0, 32);

    // 5. Decrypt TEMP1 in CBC mode using the key-encryption key.  Use the
    //    IV value from the previous step as the initialization vector.
    //    Call the ciphertext CEKICV.
    modeAttributes.put(IMode.IV, IV);
    asmAttributes.put(Assembly.DIRECTION, Direction.REVERSED);
    byte[] CEKICV;
    try
      {
        asm.init(asmAttributes);
        CEKICV = asm.lastUpdate(TEMP1, 0, 32);
      }
    catch (TransformerException x)
      {
        throw new RuntimeException(x);
      }

    // 6. Decompose CEKICV into CEK and ICV.  CEK is the most significant
    //    (first) 24 octets, and ICV is the least significant (last) 8
    //    octets.
    byte[] CEK = new byte[24];
    byte[] ICV = new byte[8];
    System.arraycopy(CEKICV, 0,  CEK, 0, 24);
    System.arraycopy(CEKICV, 24, ICV, 0, 8);

    // 7. Compute an 8 octet key checksum value on CEK as described above in
    //    Section 2.  If the computed key checksum value does not match the
    //    decrypted key checksum value, ICV, then error.
    sha.update(CEK);
    byte[] hash = sha.digest();
    byte[] computedICV = new byte[8];
    System.arraycopy(hash, 0, computedICV, 0, 8);
    if (! Arrays.equals(ICV, computedICV))
      throw new KeyUnwrappingException("ICV and computed ICV MUST match");

    // 8. Check for odd parity each of the DES key octets comprising CEK.
    //    If parity is incorrect, then error.
    if (! TripleDES.isParityAdjusted(CEK, 0))
      throw new KeyUnwrappingException("Triple-DES key parity MUST be adjusted");

    // 9. Use CEK as a Triple-DES key.
    return CEK;
  }
  
  /**
   * Fills the designated byte array with random data.
   * 
   * @param buffer the byte array to fill with random data.
   */
  private void nextRandomBytes(byte[] buffer)
  {
    if (rnd != null)
      rnd.nextBytes(buffer);
    else
      getDefaultPRNG().nextBytes(buffer);
  }
}
