/* UMacRandomSpi.java --
   Copyright (C) 2001, 2002, 2006, 2010  Free Software Foundation, Inc.

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
import gnu.java.security.prng.LimitReachedException;
import gnu.java.security.jce.prng.SecureRandomAdapter;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.prng.UMacGenerator;

import java.security.SecureRandomSpi;
import java.util.HashMap;
import java.util.Random;
import java.util.logging.Logger;

/**
 * An <em>Adapter</em> class around {@link UMacGenerator} to allow using this
 * algorithm as a JCE {@link java.security.SecureRandom}.
 */
public class UMacRandomSpi
    extends SecureRandomSpi
{
  private static final Logger log = Configuration.DEBUG ?
                    Logger.getLogger(UMacRandomSpi.class.getName()) : null;

  /** Class-wide prng to generate random material for the underlying prng. */
  private static final UMacGenerator prng; // blank final
  static
    {
      prng = new UMacGenerator();
      resetLocalPRNG();
    }
  // error messages
  private static final String MSG = "Exception while setting up a "
                                    + Registry.UMAC_PRNG + " SPI: ";
  private static final String RETRY = "Retry...";
  /** Our underlying prng instance. */
  private UMacGenerator adaptee = new UMacGenerator();

  // default 0-arguments constructor

  private static void resetLocalPRNG()
  {
    HashMap attributes = new HashMap();
    attributes.put(UMacGenerator.CIPHER, Registry.AES_CIPHER);
    byte[] key = new byte[128 / 8]; // AES default key size
    Random rand = new Random(System.currentTimeMillis());
    rand.nextBytes(key);
    attributes.put(IBlockCipher.KEY_MATERIAL, key);
    int index = rand.nextInt() & 0xFF;
    attributes.put(UMacGenerator.INDEX, Integer.valueOf(index));
    prng.setup(attributes);
  }

  public byte[] engineGenerateSeed(int numBytes)
  {
    return SecureRandomAdapter.getSeed(numBytes);
  }

  public void engineNextBytes(byte[] bytes)
  {
    if (! adaptee.isInitialised())
      engineSetSeed(engineGenerateSeed(32));
    while (true)
      {
        try
          {
            adaptee.nextBytes(bytes, 0, bytes.length);
            break;
          }
        catch (LimitReachedException x)
          { // reseed the generator
            resetLocalPRNG();
          }
      }
  }

  public void engineSetSeed(byte[] seed)
  {
    // compute the total number of random bytes required to setup adaptee
    int materialLength = 0;
    materialLength += 16; // key material size
    materialLength++; // index size
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
            catch (IllegalStateException x) // should not happen
              {
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
    attributes.put(UMacGenerator.CIPHER, Registry.AES_CIPHER);
    // specify the key
    byte[] key = new byte[16];
    System.arraycopy(material, 0, key, 0, 16);
    attributes.put(IBlockCipher.KEY_MATERIAL, key);
    // use a 1-byte index
    attributes.put(UMacGenerator.INDEX, Integer.valueOf(material[16] & 0xFF));
    adaptee.init(attributes);
  }
}
