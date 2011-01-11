/* ICMRandomSpi.java --
   Copyright (C) 2001, 2002, 2006  Free Software Foundation, Inc.

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


package gnu.javax.crypto.jce.prng;

import gnu.java.security.Configuration;
import gnu.java.security.Registry;
import gnu.java.security.jce.prng.SecureRandomAdapter;
import gnu.java.security.prng.LimitReachedException;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.prng.ICMGenerator;

import java.math.BigInteger;
import java.security.SecureRandomSpi;
import java.util.HashMap;
import java.util.Random;
import java.util.logging.Logger;

/**
 * An <em>Adapter</em> class around {@link ICMGenerator} to allow using this
 * algorithm as a JCE {@link java.security.SecureRandom}.
 */
public class ICMRandomSpi
    extends SecureRandomSpi
{
  private static final Logger log = Logger.getLogger(ICMRandomSpi.class.getName());
  /** Class-wide prng to generate random material for the underlying prng. */
  private static final ICMGenerator prng; // blank final
  static
    {
      prng = new ICMGenerator();
      resetLocalPRNG();
    }

  // error messages
  private static final String MSG = "Exception while setting up an "
                                    + Registry.ICM_PRNG + " SPI: ";
  private static final String RETRY = "Retry...";
  private static final String LIMIT_REACHED_MSG = "Limit reached: ";
  private static final String RESEED = "Re-seed...";
  /** Our underlying prng instance. */
  private ICMGenerator adaptee = new ICMGenerator();

  // default 0-arguments constructor

  private static void resetLocalPRNG()
  {
    if (Configuration.DEBUG)
      log.entering(ICMRandomSpi.class.getName(), "resetLocalPRNG");
    HashMap attributes = new HashMap();
    attributes.put(ICMGenerator.CIPHER, Registry.AES_CIPHER);
    byte[] key = new byte[128 / 8]; // AES default key size
    Random rand = new Random(System.currentTimeMillis());
    rand.nextBytes(key);
    attributes.put(IBlockCipher.KEY_MATERIAL, key);
    int aesBlockSize = 128 / 8; // AES block size in bytes
    byte[] offset = new byte[aesBlockSize];
    rand.nextBytes(offset);
    attributes.put(ICMGenerator.OFFSET, offset);
    int ndxLen = 0; // the segment length
    // choose a random value between 1 and aesBlockSize / 2
    int limit = aesBlockSize / 2;
    while (ndxLen < 1 || ndxLen > limit)
      ndxLen = rand.nextInt(limit + 1);
    attributes.put(ICMGenerator.SEGMENT_INDEX_LENGTH, Integer.valueOf(ndxLen));
    byte[] index = new byte[ndxLen];
    rand.nextBytes(index);
    attributes.put(ICMGenerator.SEGMENT_INDEX, new BigInteger(1, index));
    prng.setup(attributes);
    if (Configuration.DEBUG)
      log.exiting(ICMRandomSpi.class.getName(), "resetLocalPRNG");
  }

  public byte[] engineGenerateSeed(int numBytes)
  {
    return SecureRandomAdapter.getSeed(numBytes);
  }

  public void engineNextBytes(byte[] bytes)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineNextBytes");
    if (! adaptee.isInitialised())
      this.engineSetSeed(engineGenerateSeed(32));
    while (true)
      {
        try
          {
            adaptee.nextBytes(bytes, 0, bytes.length);
            break;
          }
        catch (LimitReachedException x)
          { // reseed the generator
            if (Configuration.DEBUG)
              {
                log.fine(LIMIT_REACHED_MSG + String.valueOf(x));
                log.fine(RESEED);
              }
            resetLocalPRNG();
          }
      }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineNextBytes");
  }

  public void engineSetSeed(byte[] seed)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "engineSetSeed");
    // compute the total number of random bytes required to setup adaptee
    int materialLength = 0;
    materialLength += 16; // key material size
    materialLength += 16; // offset size
    materialLength += 8; // index size == half of an AES block
    byte[] material = new byte[materialLength];
    // use as much as possible bytes from the seed
    int materialOffset = 0;
    int materialLeft = material.length;
    if (seed.length > 0)
      { // copy some bytes into key and update indices
        int lenToCopy = Math.min(materialLength, seed.length);
        System.arraycopy(seed, 0, material, 0, lenToCopy);
        materialOffset += lenToCopy;
        materialLeft -= lenToCopy;
      }
    if (materialOffset > 0) // generate the rest
      {
        while (true)
          {
            try
              {
                prng.nextBytes(material, materialOffset, materialLeft);
                break;
              }
            catch (IllegalStateException x)
              { // should not happen
                throw new InternalError(MSG + String.valueOf(x));
              }
            catch (LimitReachedException x)
              {
                if (Configuration.DEBUG)
                  {
                    log.fine(MSG + String.valueOf(x));
                    log.fine(RETRY);
                  }
              }
          }
      }
    // setup the underlying adaptee instance
    HashMap attributes = new HashMap();
    // use AES cipher with 128-bit block size
    attributes.put(ICMGenerator.CIPHER, Registry.AES_CIPHER);
    // use an index the size of quarter of an AES block
    attributes.put(ICMGenerator.SEGMENT_INDEX_LENGTH, Integer.valueOf(4));
    // specify the key
    byte[] key = new byte[16];
    System.arraycopy(material, 0, key, 0, 16);
    attributes.put(IBlockCipher.KEY_MATERIAL, key);
    // specify the offset
    byte[] offset = new byte[16];
    System.arraycopy(material, 16, offset, 0, 16);
    attributes.put(ICMGenerator.OFFSET, offset);
    // specify the index
    byte[] index = new byte[4];
    System.arraycopy(material, 32, index, 0, 4);
    attributes.put(ICMGenerator.SEGMENT_INDEX, new BigInteger(1, index));
    adaptee.init(attributes);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "engineSetSeed");
  }
}
