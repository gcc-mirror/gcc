/* ProviderUtil.java -- Security Provider related utilities
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


package gnu.classpath.tools.common;

import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.Provider;
import java.security.Security;
import java.util.logging.Logger;

/**
 * A <i>Helper</i> class containing general purpose utlity methods dealing with
 * installing and removing <i>Security Providers</i> at runtime.
 */
public abstract class ProviderUtil
{
  private static final Logger log = Logger.getLogger(ProviderUtil.class.getName());

  // default 0-arguments constructor

  // Class methods
  // --------------------------------------------------------------------------

  /**
   * Attempt to (a) instantiate, and (b) add a designated {@link Provider} by
   * inserting at at the top of the list of <i>Security Providers</i> already
   * present at runtime, only if it is not already installed.
   * <p>
   * <b>IMPORTANT</b>: This method overrides the security check usually carried
   * out by the security manager when inserting a new {@link Provider}.
   *
   * @param providerClass a fully qualified, non-null, class name of a
   *          <i>Security Provider</i> to add if it is not already installed.
   * @return an instance of {@link SecurityProviderInfo} referencing the
   *         {@link Provider} instance created with the designated class name,
   *         and its position in the underlying JVM runtime.
   */
  public static final SecurityProviderInfo addProvider(String providerClass)
  {
    log.entering(ProviderUtil.class.getName(), "addProvider", providerClass);

    Provider provider = null;
    try
      {
        provider = (Provider) Class.forName(providerClass.trim()).newInstance();
      }
    catch (InstantiationException x)
    {
      log.fine("InstantiationException while creating [" + providerClass
               + "]. Ignore");
    }
  catch (IllegalAccessException x)
    {
      log.fine("IllegalAccessException while creating [" + providerClass
               + "]. Ignore");
    }
  catch (ClassNotFoundException x)
    {
      log.fine("ClassNotFoundException while creating [" + providerClass
               + "]. Ignore");
    }

    int position = provider != null ? addProvider(provider) : -1;
    SecurityProviderInfo result = new SecurityProviderInfo(provider, position);

    log.exiting(ProviderUtil.class.getName(), "addProvider", result);
    return result;
  }

  /**
   * Attempt to add the designated {@link Provider} by inserting at at the top
   * of the list of <i>Security Providers</i> already present at runtime, only
   * if it is not already installed.
   * <p>
   * <b>IMPORTANT</b>: This method overrides the security check usually carried
   * out by the security manager when inserting a new {@link Provider}.
   *
   * @param provider a non-null <i>Security Provider</i> to add if it is not
   *          already installed.
   * @return the new position of the designated provider in the list if it was
   *         not already present, or <code>-1</code> if it was already
   *         installed.
   */
  public static final int addProvider(final Provider provider)
  {
    log.entering(ProviderUtil.class.getName(), "addProvider", provider);

    Integer actualPosition = (Integer) AccessController.doPrivileged(new PrivilegedAction()
    {
      public Object run()
      {
        int result = Security.insertProviderAt(provider, 1);
        return Integer.valueOf(result);
      }
    });

    int result = actualPosition.intValue();
    log.fine("Provider [" + provider.getName() + "] installed? " + (result != - 1));

    log.exiting(ProviderUtil.class.getName(), "addProvider", actualPosition);
    return result;
  }

  /**
   * Remove a designated <i>Security Provider</i>.
   * <p>
   * <b>IMPORTANT</b>: This method overrides the security check usually carried
   * out by the security manager when removing a {@link Provider}.
   *
   * @param providerName the name of the {@link Provider} to remove.
   */
  public static final void removeProvider(final String providerName)
  {
    log.entering(ProviderUtil.class.getName(), "removeProvider", providerName);

    AccessController.doPrivileged(new PrivilegedAction()
    {
      public Object run()
      {
        Security.removeProvider(providerName);
        return null;
      }
    });

    log.exiting(ProviderUtil.class.getName(), "removeProvider");
  }
}
