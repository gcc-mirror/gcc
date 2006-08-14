/* MacFactory.java -- 
   Copyright (C) 2001, 2002, 2006 Free Software Foundation, Inc.

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

import gnu.java.security.Registry;
import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * A <i>Factory</i> that instantiates instances of every supported Message
 * Authentication Code algorithms, including all <i>HMAC</i> algorithms.
 */
public class MacFactory
    implements Registry
{
  private static Set names;

  /** Trivial constructor to enforce <i>Singleton</i> pattern. */
  private MacFactory()
  {
    super();
  }

  /**
   * Returns an instance of a <i>MAC</i> algorithm given its name.
   * 
   * @param name the name of the MAC algorithm.
   * @return an instance of the <i>MAC</i> algorithm, or <code>null</code> if
   *         none can be constructed.
   * @exception InternalError if the implementation does not pass its self-test.
   */
  public static IMac getInstance(String name)
  {
    if (name == null)
      return null;

    name = name.trim();
    name = name.toLowerCase();
    if (name.startsWith(HMAC_NAME_PREFIX))
      return HMacFactory.getInstance(name);

    if (name.startsWith(OMAC_PREFIX))
      {
        name = name.substring(OMAC_PREFIX.length());
        IBlockCipher cipher = CipherFactory.getInstance(name);
        if (cipher == null)
          return null;
        return new OMAC(cipher);
      }
    IMac result = null;
    if (name.equalsIgnoreCase(UHASH32))
      result = new UHash32();
    else if (name.equalsIgnoreCase(UMAC32))
      result = new UMac32();
    else if (name.equalsIgnoreCase(TMMH16))
      result = new TMMH16();

    if (result != null && ! result.selfTest())
      throw new InternalError(result.name());

    return result;
  }

  /**
   * Returns a {@link Set} of names of <i>MAC</i> algorithms supported by this
   * <i>Factory</i>.
   * 
   * @return a {@link Set} of MAC names (Strings).
   */
  public static final Set getNames()
  {
    synchronized (MacFactory.class)
      {
        if (names == null)
          {
            HashSet hs = new HashSet();
            hs.addAll(HMacFactory.getNames());
            hs.add(UHASH32);
            hs.add(UMAC32);
            hs.add(TMMH16);
            for (Iterator it = CipherFactory.getNames().iterator(); it.hasNext();)
              hs.add(OMAC_PREFIX + it.next());

            names = Collections.unmodifiableSet(hs);
          }
      }
    return names;
  }
}
