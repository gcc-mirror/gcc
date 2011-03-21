/* OMAC.java --
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.mac;

import gnu.java.security.Configuration;
import gnu.java.security.Registry;
import gnu.java.security.util.Util;
import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.mode.IMode;

import java.security.InvalidKeyException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * The One-Key CBC MAC, OMAC. This message authentication code is based on a
 * block cipher in CBC mode.
 * <p>
 * References:
 * <ol>
 * <li>Tetsu Iwata and Kaoru Kurosawa, <i><a
 * href="http://crypt.cis.ibaraki.ac.jp/omac/docs/omac.pdf">OMAC: One-Key CBC
 * MAC</a></i>.</li>
 * </ol>
 */
public class OMAC
    implements IMac
{
  private static final Logger log = Logger.getLogger(OMAC.class.getName());
  private static final byte C1 = (byte) 0x87;
  private static final byte C2 = 0x1b;
  // Test key for OMAC-AES-128
  private static final byte[] KEY0 =
      Util.toBytesFromString("2b7e151628aed2a6abf7158809cf4f3c");
  // Test MAC for zero-length input.
  private static final byte[] DIGEST0 =
      Util.toBytesFromString("bb1d6929e95937287fa37d129b756746");
  private static Boolean valid;
  private final IBlockCipher cipher;
  private final String name;
  private IMode mode;
  private int blockSize;
  private int outputSize;
  private byte[] Lu, Lu2;
  private byte[] M;
  private byte[] Y;
  private boolean init;
  private int index;

  public OMAC(IBlockCipher cipher)
  {
    this.cipher = cipher;
    this.name = "OMAC-" + cipher.name();
  }

  public Object clone()
  {
    return new OMAC(cipher);
  }

  public String name()
  {
    return name;
  }

  public int macSize()
  {
    return outputSize;
  }

  public void init(Map attrib) throws InvalidKeyException
  {
    HashMap attrib2 = new HashMap();
    attrib2.put(IBlockCipher.KEY_MATERIAL, attrib.get(MAC_KEY_MATERIAL));
    cipher.reset();
    cipher.init(attrib2);
    blockSize = cipher.currentBlockSize();
    Integer os = (Integer) attrib.get(TRUNCATED_SIZE);
    if (os != null)
      {
        outputSize = os.intValue();
        if (outputSize < 0 || outputSize > blockSize)
          throw new IllegalArgumentException("truncated size out of range");
      }
    else
      outputSize = blockSize;

    byte[] L = new byte[blockSize];
    cipher.encryptBlock(L, 0, L, 0);
    if (Configuration.DEBUG)
      log.fine("L = " + Util.toString(L).toLowerCase());
    if (Lu != null)
      {
        Arrays.fill(Lu, (byte) 0);
        if (Lu.length != blockSize)
          Lu = new byte[blockSize];
      }
    else
      Lu = new byte[blockSize];
    if (Lu2 != null)
      {
        Arrays.fill(Lu2, (byte) 0);
        if (Lu2.length != blockSize)
          Lu2 = new byte[blockSize];
      }
    else
      Lu2 = new byte[blockSize];

    boolean msb = (L[0] & 0x80) != 0;
    for (int i = 0; i < blockSize; i++)
      {
        Lu[i] = (byte)(L[i] << 1 & 0xFF);
        if (i + 1 < blockSize)
          Lu[i] |= (byte)((L[i + 1] & 0x80) >> 7);
      }
    if (msb)
      {
        if (blockSize == 16)
          Lu[Lu.length - 1] ^= C1;
        else if (blockSize == 8)
          Lu[Lu.length - 1] ^= C2;
        else
          throw new IllegalArgumentException("unsupported cipher block size: "
                                             + blockSize);
      }
    if (Configuration.DEBUG)
      log.fine("Lu = " + Util.toString(Lu).toLowerCase());
    msb = (Lu[0] & 0x80) != 0;
    for (int i = 0; i < blockSize; i++)
      {
        Lu2[i] = (byte)(Lu[i] << 1 & 0xFF);
        if (i + 1 < blockSize)
          Lu2[i] |= (byte)((Lu[i + 1] & 0x80) >> 7);
      }
    if (msb)
      {
        if (blockSize == 16)
          Lu2[Lu2.length - 1] ^= C1;
        else
          Lu2[Lu2.length - 1] ^= C2;
      }
    if (Configuration.DEBUG)
      log.fine("Lu2 = " + Util.toString(Lu2).toLowerCase());
    if (M != null)
      {
        Arrays.fill(M, (byte) 0);
        if (M.length != blockSize)
          M = new byte[blockSize];
      }
    else
      M = new byte[blockSize];
    if (Y != null)
      {
        Arrays.fill(Y, (byte) 0);
        if (Y.length != blockSize)
          Y = new byte[blockSize];
      }
    else
      Y = new byte[blockSize];

    index = 0;
    init = true;
  }

  public void update(byte b)
  {
    if (! init)
      throw new IllegalStateException("not initialized");
    if (index == M.length)
      {
        process();
        index = 0;
      }
    M[index++] = b;
  }

  public void update(byte[] buf, int off, int len)
  {
    if (! init)
      throw new IllegalStateException("not initialized");
    if (off < 0 || len < 0 || off + len > buf.length)
      throw new IndexOutOfBoundsException("size=" + buf.length + "; off=" + off
                                          + "; len=" + len);
    for (int i = 0; i < len;)
      {
        if (index == blockSize)
          {
            process();
            index = 0;
          }
        int count = Math.min(blockSize - index, len - i);
        System.arraycopy(buf, off + i, M, index, count);
        index += count;
        i += count;
      }
  }

  public byte[] digest()
  {
    byte[] b = new byte[outputSize];
    digest(b, 0);
    return b;
  }

  public void digest(byte[] out, int off)
  {
    if (! init)
      throw new IllegalStateException("not initialized");
    if (off < 0 || off + outputSize > out.length)
      throw new IndexOutOfBoundsException("size=" + out.length + "; off=" + off
                                          + "; len=" + outputSize);
    byte[] T = new byte[blockSize];
    byte[] L = Lu;
    if (index < blockSize)
      {
        M[index++] = (byte) 0x80;
        while (index < blockSize)
          M[index++] = 0;
        L = Lu2;
      }
    for (int i = 0; i < blockSize; i++)
      T[i] = (byte)(M[i] ^ Y[i] ^ L[i]);
    cipher.encryptBlock(T, 0, T, 0);
    System.arraycopy(T, 0, out, off, outputSize);
    reset();
  }

  public void reset()
  {
    index = 0;
    if (Y != null)
      Arrays.fill(Y, (byte) 0);
    if (M != null)
      Arrays.fill(M, (byte) 0);
  }

  public boolean selfTest()
  {
    OMAC mac = new OMAC(CipherFactory.getInstance(Registry.AES_CIPHER));
    mac.reset();
    Map attr = new HashMap();
    attr.put(MAC_KEY_MATERIAL, KEY0);
    byte[] digest = null;
    try
      {
        mac.init(attr);
        digest = mac.digest();
      }
    catch (Exception x)
      {
        return false;
      }
    if (digest == null)
      return false;
    return Arrays.equals(DIGEST0, digest);
  }

  private void process()
  {
    for (int i = 0; i < blockSize; i++)
      M[i] = (byte)(M[i] ^ Y[i]);
    cipher.encryptBlock(M, 0, Y, 0);
  }
}
