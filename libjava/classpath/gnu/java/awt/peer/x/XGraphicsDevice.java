/* XGraphicsDevice.java -- GraphicsDevice for X
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

package gnu.java.awt.peer.x;

import gnu.classpath.SystemProperties;
import gnu.x11.Display;

import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.lang.reflect.Constructor;
import java.net.Socket;

/**
 * This class represents an X Display. The actual connection is established
 * lazily when it is first needed.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class XGraphicsDevice
  extends GraphicsDevice
{

  private XGraphicsConfiguration defaultConfiguration;

  /**
   * The X display associated with the XGraphicsDevice. This is established
   * when {@link #getDisplay} is first called.
   */
  private Display display;

  /**
   * The display name from which the display will be initialized.
   */
  private Display.Name displayName;

  /**
   * The event pump for this X Display.
   */
  private XEventPump eventPump;

  /**
   * Creates a new XGraphicsDevice.
   */
  XGraphicsDevice(Display.Name dn)
  {
    displayName = dn;
  }

  public int getType()
  {
    return TYPE_RASTER_SCREEN;
  }

  public String getIDstring()
  {
    // TODO: Implement this.
    throw new UnsupportedOperationException("Not yet implemented.");
  }

  public GraphicsConfiguration[] getConfigurations()
  {
    // TODO: Implement this.
    throw new UnsupportedOperationException("Not yet implemented.");
  }

  public GraphicsConfiguration getDefaultConfiguration()
  {
    if (defaultConfiguration == null)
      defaultConfiguration = new XGraphicsConfiguration(this);
    return defaultConfiguration;
  }

  /**
   * Returns the X Display associated with this XGraphicsDevice.
   * This establishes the connection to the X server on the first invocation.
   *
   * @return the X Display associated with this XGraphicsDevice
   */
  Display getDisplay()
  {
    if (display == null)
      {
        if (displayName.hostname.equals(""))
          displayName.hostname = "localhost";
        if (XToolkit.DEBUG)
          System.err.println("connecting to : " + displayName);
        // Try to connect via unix domain sockets when host == localhost.
        if ((displayName.hostname.equals("localhost")
             || displayName.hostname.equals(""))
          && SystemProperties.getProperty("gnu.xawt.no_local_sockets") == null)
          {
            Socket socket = createLocalSocket();
            if (socket != null)
              {
                display = new Display(socket, "localhost",
                                      displayName.display_no,
                                      displayName.screen_no);
              }
          }

        // The following happens when we are configured to use plain sockets,
        // when the connection is probably remote or when we couldn't load
        // the LocalSocket class stuff.
        if (display == null)
          display = new Display(displayName);

        eventPump = new XEventPump(display);
      }
    return display;
  }

  XEventPump getEventPump()
  {
    return eventPump;
  }

  /**
   * Tries to load the LocalSocket class and initiate a connection to the 
   * local X server.
   */
  private Socket createLocalSocket()
  {
    Socket socket = null;
    try
      {
        // TODO: Is this 100% ok?
        String sockPath = "/tmp/.X11-unix/X" + displayName.display_no;
        Class localSocketAddressClass =
          Class.forName("gnu.java.net.local.LocalSocketAddress");
        Constructor localSocketAddressConstr =
          localSocketAddressClass.getConstructor(new Class[]{ String.class });
        Object addr =
          localSocketAddressConstr.newInstance(new Object[]{ sockPath });
        Class localSocketClass =
          Class.forName("gnu.java.net.local.LocalSocket");
        Constructor localSocketConstructor =
          localSocketClass.getConstructor(new Class[]{localSocketAddressClass});
        Object localSocket =
          localSocketConstructor.newInstance(new Object[]{ addr });
        socket = (Socket) localSocket;
      }
    catch (Exception ex)
      {
        // Whatever goes wrong here, we return null.
      }
    return socket;
  }
}
