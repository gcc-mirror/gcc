/* KDF.java --
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.sasl.srp;

import gnu.java.security.Registry;
import gnu.java.security.prng.LimitReachedException;
import gnu.java.security.util.PRNG;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.prng.UMacGenerator;

import java.util.HashMap;

/**
 * The SASL-SRP KDF implementation, which is also used, depending on how it was
 * instantiated, as a secure Pseudo Random Number Generator.
 */
public class KDF
{
  private static final int AES_BLOCK_SIZE = 16; // default block size for AES
  private static final int AES_KEY_SIZE = 16; // default key size for the AES
  private static final byte[] buffer = new byte[1];
  /** Our default source of randomness. */
  private static final PRNG prng = PRNG.getInstance();
  /** The underlying UMAC Generator instance. */
  private UMacGenerator umac = null;

  /**
   * Constructs an instance of the <code>KDF</code> initialised with the
   * designated shared secret bytes.
   *
   * @param keyMaterial the SASL SRP shared secret (K) bytes.
   */
  private KDF(final byte[] keyMaterial, final int ndx)
  {
    super();

    final HashMap map = new HashMap();
    map.put(UMacGenerator.CIPHER, Registry.AES_CIPHER);
    map.put(UMacGenerator.INDEX, Integer.valueOf(ndx));
    map.put(IBlockCipher.CIPHER_BLOCK_SIZE, Integer.valueOf(AES_BLOCK_SIZE));
    final byte[] key = new byte[AES_KEY_SIZE];
    System.arraycopy(keyMaterial, 0, key, 0, AES_KEY_SIZE);
    map.put(IBlockCipher.KEY_MATERIAL, key);
    umac = new UMacGenerator();
    umac.init(map);
  }

  /**
   * A Factory mehod that returns an instance of a <code>KDF</code> based on
   * supplied seed data.
   *
   * @param K the SASL SRP shared secret for a <code>KDF</code> to be used for
   *          <i>CALG</i> and <i>IALG</i> setup. <code>null</code> otherwise.
   * @return an instance of a <code>KDF</code>.
   */
  static final KDF getInstance(final byte[] K)
  {
    int ndx = -1;
    final byte[] keyMaterial;
    if (K != null)
      {
        keyMaterial = K;
        ndx = 0;
      }
    else
      {
        keyMaterial = new byte[AES_BLOCK_SIZE];
        while (ndx < 1 || ndx > 255)
          ndx = (byte) nextByte();
      }
    return new KDF(keyMaterial, ndx);
  }

  private static synchronized final int nextByte()
  {
    prng.nextBytes(buffer);
    return (buffer[0] & 0xFF);
  }

  /**
   * Returns a designated number of bytes suitable for use in the SASL SRP
   * mechanism.
   *
   * @param length the number of bytes needed.
   * @return a byte array containing the generated/selected bytes.
   */
  public synchronized byte[] derive(final int length)
  {
    final byte[] result = new byte[length];
    try
      {
        umac.nextBytes(result, 0, length);
      }
    catch (IllegalStateException x) // should not happen
      {
        x.printStackTrace(System.err);
      }
    catch (LimitReachedException x) // idem
      {
        x.printStackTrace(System.err);
      }
    return result;
  }
}
