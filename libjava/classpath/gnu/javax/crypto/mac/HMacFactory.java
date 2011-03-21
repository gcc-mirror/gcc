/* HMacFactory.java --
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
import gnu.java.security.hash.HashFactory;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * A <i>Factory</i> to instantiate Keyed-Hash Message Authentication Code
 * (HMAC) algorithm instances.
 */
public class HMacFactory
    implements Registry
{
  /** Trivial constructor to enforce <i>Singleton</i> pattern. */
  private HMacFactory()
  {
    super();
  }

  /**
   * Return an instance of a <i>HMAC</i> algorithm given the name of its
   * underlying hash function, prefixed with the literal defined in
   * {@link Registry#HMAC_NAME_PREFIX}.
   *
   * @param name the fully qualified name of the underlying algorithm: composed
   *          as the concatenation of a literal prefix (see
   *          {@link Registry#HMAC_NAME_PREFIX}) and the name of the underlying
   *          hash algorithm.
   * @return an instance of the <i>HMAC</i> algorithm, or <code>null</code>
   *         if none can be constructed.
   * @exception InternalError if the implementation does not pass its self-test.
   */
  public static IMac getInstance(String name)
  {
    if (name == null)
      return null;

    name = name.trim();
    name = name.toLowerCase();
    if (! name.startsWith(HMAC_NAME_PREFIX))
      return null;

    // strip the prefix
    name = name.substring(HMAC_NAME_PREFIX.length()).trim();
    IMac result = new HMac(HashFactory.getInstance(name));
    if (result != null && ! result.selfTest())
      throw new InternalError(result.name());

    return result;
  }

  /**
   * <p>
   * Returns a {@link java.util.Set} of names of <i>HMAC</i> algorithms
   * supported by this <i>Factory</i>.
   * </p>
   *
   * @return a {@link java.util.Set} of HMAC algorithm names (Strings).
   */
  public static final Set getNames()
  {
    Set hashNames = HashFactory.getNames();
    HashSet hs = new HashSet();
    for (Iterator it = hashNames.iterator(); it.hasNext();)
      hs.add(HMAC_NAME_PREFIX + ((String) it.next()));

    return Collections.unmodifiableSet(hs);
  }
}
