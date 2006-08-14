/* Fortuna.java -- The Fortuna PRNG.
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


package gnu.javax.crypto.prng;

import gnu.java.security.Registry;
import gnu.java.security.hash.HashFactory;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.prng.BasePRNG;
import gnu.java.security.prng.LimitReachedException;
import gnu.java.security.prng.RandomEvent;
import gnu.java.security.prng.RandomEventListener;
import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.security.InvalidKeyException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;

/**
 * The Fortuna continuously-seeded pseudo-random number generator. This
 * generator is composed of two major pieces: the entropy accumulator and the
 * generator function. The former takes in random bits and incorporates them
 * into the generator's state. The latter takes this base entropy and generates
 * pseudo-random bits from it.
 * <p>
 * There are some things users of this class <em>must</em> be aware of:
 * <dl>
 * <dt>Adding Random Data</dt>
 * <dd>This class does not do any polling of random sources, but rather
 * provides an interface for adding random events. Applications that use this
 * code <em>must</em> provide this mechanism. We use this design because an
 * application writer who knows the system he is targeting is in a better
 * position to judge what random data is available.</dd>
 * <dt>Storing the Seed</dt>
 * <dd>This class implements {@link Serializable} in such a way that it writes
 * a 64 byte seed to the stream, and reads it back again when being
 * deserialized. This is the extent of seed file management, however, and those
 * using this class are encouraged to think deeply about when, how often, and
 * where to store the seed.</dd>
 * </dl>
 * <p>
 * <b>References:</b>
 * <ul>
 * <li>Niels Ferguson and Bruce Schneier, <i>Practical Cryptography</i>, pp.
 * 155--184. Wiley Publishing, Indianapolis. (2003 Niels Ferguson and Bruce
 * Schneier). ISBN 0-471-22357-3.</li>
 * </ul>
 */
public class Fortuna
    extends BasePRNG
    implements Serializable, RandomEventListener
{
  private static final long serialVersionUID = 0xFACADE;
  private static final int SEED_FILE_SIZE = 64;
  private static final int NUM_POOLS = 32;
  private static final int MIN_POOL_SIZE = 64;
  private final Generator generator;
  private final IMessageDigest[] pools;
  private long lastReseed;
  private int pool;
  private int pool0Count;
  private int reseedCount;
  public static final String SEED = "gnu.crypto.prng.fortuna.seed";

  public Fortuna()
  {
    super(Registry.FORTUNA_PRNG);
    generator = new Generator(CipherFactory.getInstance(Registry.RIJNDAEL_CIPHER),
                              HashFactory.getInstance(Registry.SHA256_HASH));
    pools = new IMessageDigest[NUM_POOLS];
    for (int i = 0; i < NUM_POOLS; i++)
      pools[i] = HashFactory.getInstance(Registry.SHA256_HASH);
    lastReseed = 0;
    pool = 0;
    pool0Count = 0;
    buffer = new byte[256];
  }

  public void setup(Map attributes)
  {
    lastReseed = 0;
    reseedCount = 0;
    pool = 0;
    pool0Count = 0;
    generator.init(attributes);
    try
      {
        fillBlock();
      }
    catch (LimitReachedException shouldNotHappen)
      {
        throw new RuntimeException(shouldNotHappen);
      }
  }

  public void fillBlock() throws LimitReachedException
  {
    if (pool0Count >= MIN_POOL_SIZE
        && System.currentTimeMillis() - lastReseed > 100)
      {
        reseedCount++;
        byte[] seed = new byte[0];
        for (int i = 0; i < NUM_POOLS; i++)
          if (reseedCount % (1 << i) == 0)
            generator.addRandomBytes(pools[i].digest());
        lastReseed = System.currentTimeMillis();
        pool0Count = 0;
      }
    generator.nextBytes(buffer);
  }

  public void addRandomByte(byte b)
  {
    pools[pool].update(b);
    if (pool == 0)
      pool0Count++;
    pool = (pool + 1) % NUM_POOLS;
  }

  public void addRandomBytes(byte[] buf, int offset, int length)
  {
    pools[pool].update(buf, offset, length);
    if (pool == 0)
      pool0Count += length;
    pool = (pool + 1) % NUM_POOLS;
  }

  public void addRandomEvent(RandomEvent event)
  {
    if (event.getPoolNumber() < 0 || event.getPoolNumber() >= pools.length)
      throw new IllegalArgumentException("pool number out of range: "
                                         + event.getPoolNumber());
    pools[event.getPoolNumber()].update(event.getSourceNumber());
    pools[event.getPoolNumber()].update((byte) event.getData().length);
    pools[event.getPoolNumber()].update(event.getData());
    if (event.getPoolNumber() == 0)
      pool0Count += event.getData().length;
  }

  // Reading and writing this object is equivalent to storing and retrieving
  // the seed.

  private void writeObject(ObjectOutputStream out) throws IOException
  {
    byte[] seed = new byte[SEED_FILE_SIZE];
    try
      {
        generator.nextBytes(seed);
      }
    catch (LimitReachedException shouldNeverHappen)
      {
        throw new Error(shouldNeverHappen);
      }
    out.write(seed);
  }

  private void readObject(ObjectInputStream in) throws IOException
  {
    byte[] seed = new byte[SEED_FILE_SIZE];
    in.readFully(seed);
    generator.addRandomBytes(seed);
  }

  /**
   * The Fortuna generator function. The generator is a PRNG in its own right;
   * Fortuna itself is basically a wrapper around this generator that manages
   * reseeding in a secure way.
   */
  public static class Generator
      extends BasePRNG
      implements Cloneable
  {
    private static final int LIMIT = 1 << 20;
    private final IBlockCipher cipher;
    private final IMessageDigest hash;
    private final byte[] counter;
    private final byte[] key;
    private boolean seeded;

    public Generator(final IBlockCipher cipher, final IMessageDigest hash)
    {
      super(Registry.FORTUNA_GENERATOR_PRNG);
      this.cipher = cipher;
      this.hash = hash;
      counter = new byte[cipher.defaultBlockSize()];
      buffer = new byte[cipher.defaultBlockSize()];
      int keysize = 0;
      for (Iterator it = cipher.keySizes(); it.hasNext();)
        {
          int ks = ((Integer) it.next()).intValue();
          if (ks > keysize)
            keysize = ks;
          if (keysize >= 32)
            break;
        }
      key = new byte[keysize];
    }

    public byte nextByte()
    {
      byte[] b = new byte[1];
      nextBytes(b, 0, 1);
      return b[0];
    }

    public void nextBytes(byte[] out, int offset, int length)
    {
      if (! seeded)
        throw new IllegalStateException("generator not seeded");
      int count = 0;
      do
        {
          int amount = Math.min(LIMIT, length - count);
          try
            {
              super.nextBytes(out, offset + count, amount);
            }
          catch (LimitReachedException shouldNeverHappen)
            {
              throw new Error(shouldNeverHappen);
            }
          count += amount;
          for (int i = 0; i < key.length; i += counter.length)
            {
              fillBlock();
              int l = Math.min(key.length - i, cipher.currentBlockSize());
              System.arraycopy(buffer, 0, key, i, l);
            }
          resetKey();
        }
      while (count < length);
      fillBlock();
      ndx = 0;
    }

    public void addRandomByte(byte b)
    {
      addRandomBytes(new byte[] { b });
    }

    public void addRandomBytes(byte[] seed, int offset, int length)
    {
      hash.update(key);
      hash.update(seed, offset, length);
      byte[] newkey = hash.digest();
      System.arraycopy(newkey, 0, key, 0, Math.min(key.length, newkey.length));
      resetKey();
      incrementCounter();
      seeded = true;
    }

    public void fillBlock()
    {
      if (! seeded)
        throw new IllegalStateException("generator not seeded");
      cipher.encryptBlock(counter, 0, buffer, 0);
      incrementCounter();
    }

    public void setup(Map attributes)
    {
      seeded = false;
      Arrays.fill(key, (byte) 0);
      Arrays.fill(counter, (byte) 0);
      byte[] seed = (byte[]) attributes.get(SEED);
      if (seed != null)
        addRandomBytes(seed);
      fillBlock();
    }

    /**
     * Resets the cipher's key. This is done after every reseed, which combines
     * the old key and the seed, and processes that throigh the hash function.
     */
    private void resetKey()
    {
      try
        {
          cipher.reset();
          cipher.init(Collections.singletonMap(IBlockCipher.KEY_MATERIAL, key));
        }
      // We expect to never get an exception here.
      catch (InvalidKeyException ike)
        {
          throw new Error(ike);
        }
      catch (IllegalArgumentException iae)
        {
          throw new Error(iae);
        }
    }

    /**
     * Increment `counter' as a sixteen-byte little-endian unsigned integer by
     * one.
     */
    private void incrementCounter()
    {
      for (int i = 0; i < counter.length; i++)
        {
          counter[i]++;
          if (counter[i] != 0)
            break;
        }
    }
  }
}
