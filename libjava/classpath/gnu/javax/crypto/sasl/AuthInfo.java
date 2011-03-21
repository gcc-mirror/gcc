/* AuthInfo.java --
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


package gnu.javax.crypto.sasl;

import gnu.java.security.Registry;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.StringTokenizer;

/**
 * A static class for creating {@link IAuthInfoProvider} providers. It
 * transparently locates and uses any provider instances, based on the value
 * assigned to the System property with the key
 * <code>gnu.crypto.sasl.auth.info.provider.pkgs</code>. If more than one is
 * specified they SHOULD be separated with a vertical bar character. Please note
 * that the GNU provider is always added last to the list, disregarding whether
 * it was mentioned or not in the value of that property, or if it that property
 * was not defined.
 */
public class AuthInfo
{
  private static final ArrayList factories = new ArrayList();
  static
    {
      IAuthInfoProviderFactory ours = new AuthInfoProviderFactory();
      // if SASL_AUTH_INFO_PROVIDER_PKGS is defined then parse it
      String clazz;
      String pkgs = System.getProperty(Registry.SASL_AUTH_INFO_PROVIDER_PKGS,
                                       null);
      if (pkgs != null)
        {
          for (StringTokenizer st = new StringTokenizer(pkgs, "|"); st.hasMoreTokens();)
            {
              clazz = st.nextToken().trim();
              if (! "gnu.javax.crypto.sasl".equals(clazz))
                {
                  clazz += ".AuthInfoProviderFactory";
                  try
                    {
                      IAuthInfoProviderFactory factory =
                          (IAuthInfoProviderFactory) Class.forName(clazz).newInstance();
                      factories.add(factory);
                    }
                  catch (ClassCastException ignored)
                    {
                    }
                  catch (ClassNotFoundException ignored)
                    {
                    }
                  catch (InstantiationException ignored)
                    {
                    }
                  catch (IllegalAccessException ignored)
                    {
                    }
                }
            }
        }
      // always add ours last; unless it's already there
      if (!factories.contains(ours))
        factories.add(ours);
    }

  /** Trivial constructor to enforce Singleton pattern. */
  private AuthInfo()
  {
    super();
  }

  /**
   * A convenience method to return the authentication information provider for
   * a designated SASL mechnanism. It goes through all the installed provider
   * factories, one at a time, and attempts to return a new instance of the
   * provider for the designated mechanism. It stops at the first factory
   * returning a non-null provider.
   *
   * @param mechanism the name of a SASL mechanism.
   * @return an implementation that provides {@link IAuthInfoProvider} for that
   *         mechanism; or <code>null</code> if none found.
   */
  public static IAuthInfoProvider getProvider(String mechanism)
  {
    for (Iterator it = factories.iterator(); it.hasNext();)
      {
        IAuthInfoProviderFactory factory = (IAuthInfoProviderFactory) it.next();
        IAuthInfoProvider result = factory.getInstance(mechanism);
        if (result != null)
          return result;
      }
    return null;
  }
}
