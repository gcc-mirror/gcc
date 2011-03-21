/* KeyWrappingAlgorithmFactory.java -- FIXME: briefly describe file purpose
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

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * A Factory class for the Key Wrapping Algorithm implementations.
 */
public class KeyWrappingAlgorithmFactory
{
  /** Names of Key Wrapping Algorihms cached for speed. */
  private static Set names;

  /** Trivial constructor to enforce Singleton pattern. */
  private KeyWrappingAlgorithmFactory()
  {
    super();
  }

  /**
   * Returns an instance of a key-wrapping algorithm given its name.
   *
   * @param name the case-insensitive name of the key-wrapping algorithm.
   * @return an instance of the designated key-wrapping algorithm, or
   *         <code>null</code> if none was found.
   * @exception InternalError if the implementation does not pass its self-test.
   */
  public static final IKeyWrappingAlgorithm getInstance(String name)
  {
    if (name == null)
      return null;
    name = name.trim();
    IKeyWrappingAlgorithm result = null;
    if (name.equalsIgnoreCase(Registry.AES_KWA)
        || name.equalsIgnoreCase(Registry.AES128_KWA)
        || name.equalsIgnoreCase(Registry.AES192_KWA)
        || name.equalsIgnoreCase(Registry.AES256_KWA)
        || name.equalsIgnoreCase(Registry.RIJNDAEL_KWA))
      result = new AESKeyWrap();
    else if (name.equalsIgnoreCase(Registry.TRIPLEDES_KWA)
        || name.equalsIgnoreCase(Registry.DESEDE_KWA))
      result = new TripleDESKeyWrap();

    return result;
  }

  /**
   * Returns a {@link Set} of key wrapping algorithm names supported by this
   * <i>Factory</i>.
   *
   * @return a {@link Set} of key wrapping algorithm names (Strings).
   */
  public static synchronized final Set getNames()
  {
    if (names == null)
      {
        HashSet hs = new HashSet();
        hs.add(Registry.AES_KWA);
        hs.add(Registry.AES128_KWA);
        hs.add(Registry.AES192_KWA);
        hs.add(Registry.AES256_KWA);
        hs.add(Registry.RIJNDAEL_KWA);
        hs.add(Registry.TRIPLEDES_KWA);
        hs.add(Registry.DESEDE_KWA);
        names = Collections.unmodifiableSet(hs);
      }
    return names;
  }
}
