/* BaseMode.java --
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


package gnu.javax.crypto.mode;

import gnu.java.lang.CPStringBuilder;

import gnu.javax.crypto.cipher.IBlockCipher;

import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * A basic abstract class to facilitate implementing block cipher modes of
 * operations.
 */
public abstract class BaseMode
    implements IMode
{
  /** The canonical name prefix of this mode. */
  protected String name;
  /** The state indicator of this instance. */
  protected int state;
  /** The underlying block cipher implementation. */
  protected IBlockCipher cipher;
  /** The block size, in bytes, to operate the underlying block cipher in. */
  protected int cipherBlockSize;
  /** The block size, in bytes, in which to operate the mode instance. */
  protected int modeBlockSize;
  /** The initialisation vector value. */
  protected byte[] iv;
  /** The instance lock. */
  protected Object lock = new Object();

  /**
   * Trivial constructor for use by concrete subclasses.
   *
   * @param name the canonical name prefix of this mode.
   * @param underlyingCipher the implementation of the underlying cipher.
   * @param cipherBlockSize the block size, in bytes, in which to operate the
   *          underlying cipher.
   */
  protected BaseMode(String name, IBlockCipher underlyingCipher,
                     int cipherBlockSize)
  {
    super();

    this.name = name;
    this.cipher = underlyingCipher;
    this.cipherBlockSize = cipherBlockSize;
    state = -1;
  }

  public void update(byte[] in, int inOffset, byte[] out, int outOffset)
      throws IllegalStateException
  {
    synchronized (lock)
      {
        switch (state)
          {
          case ENCRYPTION:
            encryptBlock(in, inOffset, out, outOffset);
            break;
          case DECRYPTION:
            decryptBlock(in, inOffset, out, outOffset);
            break;
          default:
            throw new IllegalStateException();
          }
      }
  }

  public String name()
  {
    return new CPStringBuilder(name).append('(').append(cipher.name()).append(')')
        .toString();
  }

  /**
   * Returns the default value, in bytes, of the mode's block size. This value
   * is part of the construction arguments passed to the Factory methods in
   * {@link ModeFactory}. Unless changed by an invocation of any of the
   * <code>init()</code> methods, a <i>Mode</i> instance would operate with
   * the same block size as its underlying block cipher. As mentioned earlier,
   * the block size of the underlying block cipher itself is specified in one of
   * the method(s) available in the factory class.
   *
   * @return the default value, in bytes, of the mode's block size.
   * @see ModeFactory
   */
  public int defaultBlockSize()
  {
    return cipherBlockSize;
  }

  /**
   * Returns the default value, in bytes, of the underlying block cipher key
   * size.
   *
   * @return the default value, in bytes, of the underlying cipher's key size.
   */
  public int defaultKeySize()
  {
    return cipher.defaultKeySize();
  }

  /**
   * Returns an {@link Iterator} over the supported block sizes. Each element
   * returned by this object is an {@link Integer}.
   * <p>
   * The default behaviour is to return an iterator with just one value, which
   * is that currently configured for the underlying block cipher. Concrete
   * implementations may override this behaviour to signal their ability to
   * support other values.
   *
   * @return an {@link Iterator} over the supported block sizes.
   */
  public Iterator blockSizes()
  {
    ArrayList al = new ArrayList();
    al.add(Integer.valueOf(cipherBlockSize));
    return Collections.unmodifiableList(al).iterator();
  }

  /**
   * Returns an {@link Iterator} over the supported underlying block cipher key
   * sizes. Each element returned by this object is an instance of
   * {@link Integer}.
   *
   * @return an {@link Iterator} over the supported key sizes.
   */
  public Iterator keySizes()
  {
    return cipher.keySizes();
  }

  public void init(Map attributes) throws InvalidKeyException,
      IllegalStateException
  {
    synchronized (lock)
      {
        if (state != -1)
          throw new IllegalStateException();
        Integer want = (Integer) attributes.get(STATE);
        if (want != null)
          {
            switch (want.intValue())
              {
              case ENCRYPTION:
                state = ENCRYPTION;
                break;
              case DECRYPTION:
                state = DECRYPTION;
                break;
              default:
                throw new IllegalArgumentException();
              }
          }
        Integer bs = (Integer) attributes.get(MODE_BLOCK_SIZE);
        modeBlockSize = (bs == null ? cipherBlockSize : bs.intValue());
        byte[] iv = (byte[]) attributes.get(IV);
        if (iv != null)
          this.iv = (byte[]) iv.clone();
        else
          this.iv = new byte[modeBlockSize];
        cipher.init(attributes);
        setup();
      }
  }

  public int currentBlockSize()
  {
    if (state == -1)
      throw new IllegalStateException();
    return modeBlockSize;
  }

  public void reset()
  {
    synchronized (lock)
      {
        state = -1;
        iv = null;
        cipher.reset();
        teardown();
      }
  }

  public boolean selfTest()
  {
    int ks;
    Iterator bit;
    for (Iterator kit = keySizes(); kit.hasNext();)
      {
        ks = ((Integer) kit.next()).intValue();
        for (bit = blockSizes(); bit.hasNext();)
          if (! testSymmetry(ks, ((Integer) bit.next()).intValue()))
            return false;
      }
    return true;
  }

  public abstract Object clone();

  /** The initialisation phase of the concrete mode implementation. */
  public abstract void setup();

  /** The termination phase of the concrete mode implementation. */
  public abstract void teardown();

  public abstract void encryptBlock(byte[] in, int i, byte[] out, int o);

  public abstract void decryptBlock(byte[] in, int i, byte[] out, int o);

  private boolean testSymmetry(int ks, int bs)
  {
    try
      {
        IMode mode = (IMode) this.clone();
        byte[] iv = new byte[cipherBlockSize]; // all zeroes
        byte[] k = new byte[ks];
        int i;
        for (i = 0; i < ks; i++)
          k[i] = (byte) i;
        int blockCount = 5;
        int limit = blockCount * bs;
        byte[] pt = new byte[limit];
        for (i = 0; i < limit; i++)
          pt[i] = (byte) i;
        byte[] ct = new byte[limit];
        byte[] cpt = new byte[limit];
        Map map = new HashMap();
        map.put(KEY_MATERIAL, k);
        map.put(CIPHER_BLOCK_SIZE, Integer.valueOf(cipherBlockSize));
        map.put(STATE, Integer.valueOf(ENCRYPTION));
        map.put(IV, iv);
        map.put(MODE_BLOCK_SIZE, Integer.valueOf(bs));
        mode.reset();
        mode.init(map);
        for (i = 0; i < blockCount; i++)
          mode.update(pt, i * bs, ct, i * bs);
        mode.reset();
        map.put(STATE, Integer.valueOf(DECRYPTION));
        mode.init(map);
        for (i = 0; i < blockCount; i++)
          mode.update(ct, i * bs, cpt, i * bs);
        return Arrays.equals(pt, cpt);
      }
    catch (Exception x)
      {
        x.printStackTrace(System.err);
        return false;
      }
  }
}
