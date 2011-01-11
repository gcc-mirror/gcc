/* ModeFactory.java --
   Copyright (C) 2001, 2002, 2004, 2006 Free Software Foundation, Inc.

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

import gnu.java.security.Registry;

import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * A <i>Factory</i> to instantiate block cipher modes of operations.
 */
public class ModeFactory
    implements Registry
{
  private static Set names;

  /** Trivial constructor to enforce Singleton pattern. */
  private ModeFactory()
  {
    super();
  }

  /**
   * Returns an instance of a block cipher mode of operations given its name and
   * characteristics of the underlying block cipher.
   *
   * @param mode the case-insensitive name of the mode of operations.
   * @param cipher the case-insensitive name of the block cipher.
   * @param cipherBlockSize the block size, in bytes, of the underlying cipher.
   * @return an instance of the block cipher algorithm, operating in a given
   *         mode of operations, or <code>null</code> if none found.
   * @exception InternalError if either the mode or the underlying block cipher
   *              implementation does not pass its self-test.
   */
  public static IMode getInstance(String mode, String cipher,
                                  int cipherBlockSize)
  {
    if (mode == null || cipher == null)
      return null;

    mode = mode.trim();
    cipher = cipher.trim();
    IBlockCipher cipherImpl = CipherFactory.getInstance(cipher);
    if (cipherImpl == null)
      return null;

    return getInstance(mode, cipherImpl, cipherBlockSize);
  }

  public static IMode getInstance(String mode, IBlockCipher cipher,
                                  int cipherBlockSize)
  {
    // ensure that cipherBlockSize is valid for the chosen underlying cipher
    boolean ok = false;
    for (Iterator it = cipher.blockSizes(); it.hasNext();)
      {
        ok = (cipherBlockSize == ((Integer) it.next()).intValue());
        if (ok)
          break;
      }
    if (! ok)
      throw new IllegalArgumentException("cipherBlockSize");
    IMode result = null;
    if (mode.equalsIgnoreCase(ECB_MODE))
      result = new ECB(cipher, cipherBlockSize);
    else if (mode.equalsIgnoreCase(CTR_MODE))
      result = new CTR(cipher, cipherBlockSize);
    else if (mode.equalsIgnoreCase(ICM_MODE))
      result = new ICM(cipher, cipherBlockSize);
    else if (mode.equalsIgnoreCase(OFB_MODE))
      result = new OFB(cipher, cipherBlockSize);
    else if (mode.equalsIgnoreCase(CBC_MODE))
      result = new CBC(cipher, cipherBlockSize);
    else if (mode.equalsIgnoreCase(CFB_MODE))
      result = new CFB(cipher, cipherBlockSize);
    else if (mode.equalsIgnoreCase(EAX_MODE))
      result = new EAX(cipher, cipherBlockSize);

    if (result != null && ! result.selfTest())
      throw new InternalError(result.name());

    return result;
  }

  /**
   * Returns a {@link Set} of names of mode supported by this <i>Factory</i>.
   *
   * @return a {@link Set} of mode names (Strings).
   */
  public static final Set getNames()
  {
    synchronized (ModeFactory.class)
      {
        if (names == null)
          {
            HashSet hs = new HashSet();
            hs.add(ECB_MODE);
            hs.add(CTR_MODE);
            hs.add(ICM_MODE);
            hs.add(OFB_MODE);
            hs.add(CBC_MODE);
            hs.add(CFB_MODE);
            hs.add(EAX_MODE);
            names = Collections.unmodifiableSet(hs);
          }
      }
    return names;
  }
}
