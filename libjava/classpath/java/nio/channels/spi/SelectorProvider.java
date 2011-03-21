/* SelectorProvider.java
   Copyright (C) 2002, 2003, 2004  Free Software Foundation, Inc.

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

package java.nio.channels.spi;

import gnu.java.nio.SelectorProviderImpl;

import java.io.IOException;
import java.nio.channels.Channel;
import java.nio.channels.DatagramChannel;
import java.nio.channels.Pipe;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;


/**
 * @author Michael Koch
 * @since 1.4
 */
public abstract class SelectorProvider
{
  private static SelectorProvider systemDefaultProvider;

  /**
   * Initializes the selector provider.
   *
   * @exception SecurityException If a security manager has been installed and
   * it denies @see RuntimePermission ("selectorProvider").
   */
  protected SelectorProvider()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(new RuntimePermission("selectorProvider"));
  }

  /**
   * Opens a datagram channel.
   *
   * @return a new datagram channel object
   *
   * @exception IOException if an error occurs
   */
  public abstract DatagramChannel openDatagramChannel()
    throws IOException;

  /**
   * Opens a pipe.
   *
   * @return a new pipe object
   *
   * @exception IOException if an error occurs
   */
  public abstract Pipe openPipe() throws IOException;

  /**
   * Opens a selector.
   *
   * @return a new selector object
   *
   * @exception IOException if an error occurs
   */
  public abstract AbstractSelector openSelector() throws IOException;

  /**
   * Opens a server socket channel.
   *
   * @return a new server socket channel object
   *
   * @exception IOException if an error occurs
   */
  public abstract ServerSocketChannel openServerSocketChannel()
    throws IOException;

  /**
   * Opens a socket channel.
   *
   * @return a new socket channel object
   *
   * @exception IOException if an error occurs
   */
  public abstract SocketChannel openSocketChannel() throws IOException;

  /**
   * Returns the inherited channel of the VM.
   *
   * @return the inherited channel of the VM
   *
   * @throws IOException If an I/O error occurs
   * @throws SecurityException If an installed security manager denies access
   *         to RuntimePermission("inheritedChannel")
   *
   * @since 1.5
   */
  public Channel inheritedChannel()
    throws IOException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        sm.checkPermission(new RuntimePermission("inheritedChannel"));
      }
    // Implementation note: The default implementation can't do much more
    // than return null. If a VM can provide something more useful it should
    // install its own SelectorProvider (maybe a subclass of this one) to
    // return something more useful.
    return null;
  }

  /**
   * Returns the system-wide default selector provider for this invocation
   * of the Java virtual machine.
   *
   * @return the default seletor provider
   */
  public static synchronized SelectorProvider provider()
  {
    if (systemDefaultProvider == null)
      {
        String propertyValue =
          System.getProperty("java.nio.channels.spi.SelectorProvider");

        if (propertyValue == null || propertyValue.equals(""))
          systemDefaultProvider = new SelectorProviderImpl();
        else
          {
            try
              {
                systemDefaultProvider =
                  (SelectorProvider) Class.forName(propertyValue)
                                          .newInstance();
              }
            catch (Exception e)
              {
                System.err.println("Could not instantiate class: "
                                   + propertyValue);
                systemDefaultProvider = new SelectorProviderImpl();
              }
          }
      }

    return systemDefaultProvider;
  }
}
