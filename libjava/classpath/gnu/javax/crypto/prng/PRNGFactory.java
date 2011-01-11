/* PRNGFactory.java --
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


package gnu.javax.crypto.prng;

import gnu.java.security.Registry;
import gnu.java.security.prng.IRandom;
import gnu.javax.crypto.mac.HMacFactory;
import gnu.javax.crypto.mac.IMac;
import gnu.javax.crypto.mac.MacFactory;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * A Factory to instantiate pseudo random number generators.
 */
public class PRNGFactory
    implements Registry
{
  /** Trivial constructor to enforce <i>Singleton</i> pattern. */
  private PRNGFactory()
  {
  }

  /**
   * Returns an instance of a padding algorithm given its name.
   *
   * @param prng the case-insensitive name of the PRNG.
   * @return an instance of the pseudo-random number generator.
   * @exception InternalError if the implementation does not pass its self-
   *              test.
   */
  public static IRandom getInstance(String prng)
  {
    if (prng == null)
      return null;
    prng = prng.trim();
    IRandom result = null;
    if (prng.equalsIgnoreCase(ARCFOUR_PRNG) || prng.equalsIgnoreCase(RC4_PRNG))
      result = new ARCFour();
    else if (prng.equalsIgnoreCase(ICM_PRNG))
      result = new ICMGenerator();
    else if (prng.equalsIgnoreCase(UMAC_PRNG))
      result = new UMacGenerator();
    else if (prng.toLowerCase().startsWith(PBKDF2_PRNG_PREFIX))
      {
        String macName = prng.substring(PBKDF2_PRNG_PREFIX.length());
        IMac mac = MacFactory.getInstance(macName);
        if (mac == null)
          return null;
        result = new PBKDF2(mac);
      }

    if (result != null)
      return result;

    return gnu.java.security.prng.PRNGFactory.getInstance(prng);
  }

  /**
   * Returns a {@link Set} of names of padding algorithms supported by this
   * <i>Factory</i>.
   *
   * @return a {@link Set} of pseudo-random number generator algorithm names
   *         (Strings).
   */
  public static Set getNames()
  {
    HashSet hs = new HashSet(gnu.java.security.prng.PRNGFactory.getNames());
    hs.add(ICM_PRNG);
    hs.add(UMAC_PRNG);
    // add all hmac implementations as candidate PBKDF2 ones too
    for (Iterator it = HMacFactory.getNames().iterator(); it.hasNext();)
      hs.add(PBKDF2_PRNG_PREFIX + ((String) it.next()));
    return Collections.unmodifiableSet(hs);
  }
}
