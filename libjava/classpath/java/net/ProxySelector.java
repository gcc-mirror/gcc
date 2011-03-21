/* ProxySelector.java -- A proxy selector class
   Copyright (C) 2006  Free Software Foundation, Inc.

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

package java.net;

import gnu.java.net.DefaultProxySelector;

import java.io.IOException;
import java.util.List;

/**
 * Class for handling proxies for different connections.
 *
 * @since 1.5
 */
public abstract class ProxySelector
{
  /**
   * Default proxy selector.
   */
  private static ProxySelector defaultSelector = new DefaultProxySelector();

  /**
   * Creates a new <code>ProxySelector</code> object.
   */
  public ProxySelector()
  {
    // Do nothing here.
  }

  /**
   * Returns the default proxy selector.
   *
   * @return the default proxy selector
   *
   * @throws SecurityException If a security manager is installed and it
   * denies NetPermission("getProxySelector")
   */
  public static ProxySelector getDefault()
  {
    SecurityManager sm = System.getSecurityManager();

    if (sm != null)
      sm.checkPermission(new NetPermission("getProxySelector"));

    return defaultSelector;
  }

  /**
   * Sets the default proxy selector.
   *
   * @param selector the defualt proxy selector
   *
   * @throws SecurityException If a security manager is installed and it
   * denies NetPermission("setProxySelector")
   */
  public static void setDefault(ProxySelector selector)
  {
    SecurityManager sm = System.getSecurityManager();

    if (sm != null)
      sm.checkPermission(new NetPermission("setProxySelector"));

    defaultSelector = selector;
  }

  /**
   * Signals to the selector that a proxy was no available.
   *
   * @throws IllegalArgumentException If one argument is null
   */
  public abstract void connectFailed(URI uri, SocketAddress address,
                                     IOException exception);

  /**
   * Returns the list of proxy settings for a given URI.
   *
   * @return list of proxy settings
   *
   * @throws IllegalArgumentException If uri is null
   */
  public abstract List<Proxy> select(URI uri);
}
