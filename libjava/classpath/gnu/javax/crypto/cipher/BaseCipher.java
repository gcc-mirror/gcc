/* BaseCipher.java --
   Copyright (C) 2001, 2002, 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.cipher;

import gnu.java.lang.CPStringBuilder;

import gnu.java.security.Configuration;

import java.security.InvalidKeyException;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A basic abstract class to facilitate implementing symmetric key block
 * ciphers.
 */
public abstract class BaseCipher
    implements IBlockCipher, IBlockCipherSpi
{
  private static final Logger log = Logger.getLogger(BaseCipher.class.getName());
  /** The canonical name prefix of the cipher. */
  protected String name;
  /** The default block size, in bytes. */
  protected int defaultBlockSize;
  /** The default key size, in bytes. */
  protected int defaultKeySize;
  /** The current block size, in bytes. */
  protected int currentBlockSize;
  /** The session key for this instance. */
  protected transient Object currentKey;
  /** The instance lock. */
  protected Object lock = new Object();

  /**
   * Trivial constructor for use by concrete subclasses.
   *
   * @param name the canonical name prefix of this instance.
   * @param defaultBlockSize the default block size in bytes.
   * @param defaultKeySize the default key size in bytes.
   */
  protected BaseCipher(String name, int defaultBlockSize, int defaultKeySize)
  {
    super();

    this.name = name;
    this.defaultBlockSize = defaultBlockSize;
    this.defaultKeySize = defaultKeySize;
  }

  public abstract Object clone();

  public String name()
  {
    CPStringBuilder sb = new CPStringBuilder(name).append('-');
    if (currentKey == null)
      sb.append(String.valueOf(8 * defaultBlockSize));
    else
      sb.append(String.valueOf(8 * currentBlockSize));
    return sb.toString();
  }

  public int defaultBlockSize()
  {
    return defaultBlockSize;
  }

  public int defaultKeySize()
  {
    return defaultKeySize;
  }

  public void init(Map attributes) throws InvalidKeyException
  {
    synchronized (lock)
      {
        if (currentKey != null)
          throw new IllegalStateException();
        Integer bs = (Integer) attributes.get(CIPHER_BLOCK_SIZE);
        if (bs == null) // no block size was specified
          {
            if (currentBlockSize == 0) // happy birthday
              currentBlockSize = defaultBlockSize;
            // else it's a clone. use as is
          }
        else
          {
            currentBlockSize = bs.intValue();
            // ensure that value is valid
            Iterator it;
            boolean ok = false;
            for (it = blockSizes(); it.hasNext();)
              {
                ok = (currentBlockSize == ((Integer) it.next()).intValue());
                if (ok)
                  break;
              }
            if (! ok)
              throw new IllegalArgumentException(IBlockCipher.CIPHER_BLOCK_SIZE);
          }
        byte[] k = (byte[]) attributes.get(KEY_MATERIAL);
        currentKey = makeKey(k, currentBlockSize);
      }
  }

  public int currentBlockSize()
  {
    if (currentKey == null)
      throw new IllegalStateException();
    return currentBlockSize;
  }

  public void reset()
  {
    synchronized (lock)
      {
        currentKey = null;
      }
  }

  public void encryptBlock(byte[] in, int inOffset, byte[] out, int outOffset)
      throws IllegalStateException
  {
    synchronized (lock)
      {
        if (currentKey == null)
          throw new IllegalStateException();
        encrypt(in, inOffset, out, outOffset, currentKey, currentBlockSize);
      }
  }

  public void decryptBlock(byte[] in, int inOffset, byte[] out, int outOffset)
      throws IllegalStateException
  {
    synchronized (lock)
      {
        if (currentKey == null)
          throw new IllegalStateException();
        decrypt(in, inOffset, out, outOffset, currentKey, currentBlockSize);
      }
  }

  public boolean selfTest()
  {
    int ks;
    Iterator bit;
    // do symmetry tests for all block-size/key-size combos
    for (Iterator kit = keySizes(); kit.hasNext();)
      {
        ks = ((Integer) kit.next()).intValue();
        for (bit = blockSizes(); bit.hasNext();)
          if (! testSymmetry(ks, ((Integer) bit.next()).intValue()))
            return false;
      }
    return true;
  }

  private boolean testSymmetry(int ks, int bs)
  {
    try
      {
        byte[] kb = new byte[ks];
        byte[] pt = new byte[bs];
        byte[] ct = new byte[bs];
        byte[] cpt = new byte[bs];
        int i;
        for (i = 0; i < ks; i++)
          kb[i] = (byte) i;
        for (i = 0; i < bs; i++)
          pt[i] = (byte) i;
        Object k = makeKey(kb, bs);
        encrypt(pt, 0, ct, 0, k, bs);
        decrypt(ct, 0, cpt, 0, k, bs);
        return Arrays.equals(pt, cpt);
      }
    catch (Exception x)
      {
        if (Configuration.DEBUG)
          log.log(Level.FINE, "Exception in testSymmetry() for " + name(), x);
        return false;
      }
  }

  protected boolean testKat(byte[] kb, byte[] ct)
  {
    return testKat(kb, ct, new byte[ct.length]); // all-zero plaintext
  }

  protected boolean testKat(byte[] kb, byte[] ct, byte[] pt)
  {
    try
      {
        int bs = pt.length;
        byte[] t = new byte[bs];
        Object k = makeKey(kb, bs);
        // test encryption
        encrypt(pt, 0, t, 0, k, bs);
        if (! Arrays.equals(t, ct))
          return false;
        // test decryption
        decrypt(t, 0, t, 0, k, bs);
        return Arrays.equals(t, pt);
      }
    catch (Exception x)
      {
        if (Configuration.DEBUG)
          log.log(Level.FINE, "Exception in testKat() for " + name(), x);
        return false;
      }
  }
}
