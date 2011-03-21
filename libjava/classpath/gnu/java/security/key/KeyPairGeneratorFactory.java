/* KeyPairGeneratorFactory.java --
   Copyright 2001, 2002, 2003, 2006 Free Software Foundation, Inc.

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


package gnu.java.security.key;

import gnu.java.security.Registry;
import gnu.java.security.key.dss.DSSKeyPairGenerator;
import gnu.java.security.key.rsa.RSAKeyPairGenerator;

import java.lang.reflect.Constructor;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * A Factory to instantiate asymmetric keypair generators.
 */
public class KeyPairGeneratorFactory
{
  /** Trivial constructor to enforce Singleton pattern. */
  private KeyPairGeneratorFactory()
  {
    super();
  }

  /**
   * Returns an instance of a keypair generator given its name.
   *
   * @param name the case-insensitive key generator name.
   * @return an instance of the keypair generator, or <code>null</code> if
   *         none found.
   */
  public static IKeyPairGenerator getInstance(String name)
  {
    if (name == null)
      return null;

    name = name.trim();
    IKeyPairGenerator result = null;
    if (name.equalsIgnoreCase(Registry.DSA_KPG)
        || name.equalsIgnoreCase(Registry.DSS_KPG))
      result = new DSSKeyPairGenerator();
    else if (name.equalsIgnoreCase(Registry.RSA_KPG))
      result = new RSAKeyPairGenerator();
    else if (name.equalsIgnoreCase(Registry.DH_KPG))
      result = makeInstance("gnu.javax.crypto.key.dh.GnuDHKeyPairGenerator");
    else if (name.equalsIgnoreCase(Registry.SRP_KPG))
      result = makeInstance("gnu.javax.crypto.key.srp6.SRPKeyPairGenerator");

    return result;
  }

  /**
   * Returns a {@link Set} of keypair generator names supported by this
   * <i>Factory</i>. Those keypair generators may be used in conjunction with
   * the digital signature schemes with appendix supported by this library.
   *
   * @return a {@link Set} of keypair generator names (Strings).
   */
  public static final Set getNames()
  {
    HashSet hs = new HashSet();
    hs.add(Registry.DSS_KPG);
    hs.add(Registry.DSA_KPG);
    hs.add(Registry.RSA_KPG);
    hs.add(Registry.DH_KPG);
    hs.add(Registry.SRP_KPG);
    return Collections.unmodifiableSet(hs);
  }

  private static IKeyPairGenerator makeInstance(String clazz)
  {
    try
      {
        Class c = Class.forName(clazz);
        Constructor ctor = c.getConstructor(new Class[0]);
        return (IKeyPairGenerator) ctor.newInstance(new Object[0]);
      }
    catch (Exception x)
      {
        throw new IllegalArgumentException(
            "strong crypto key pair generator not available: " + clazz, x);
      }
  }
}
