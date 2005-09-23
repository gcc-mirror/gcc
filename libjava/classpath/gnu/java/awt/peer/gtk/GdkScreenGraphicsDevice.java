/* GdkScreenGraphicsDevice.java -- information about a screen device
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Dimension;
import java.awt.DisplayMode;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.Toolkit;

public class GdkScreenGraphicsDevice extends GraphicsDevice
{
  GdkGraphicsEnvironment env;

  public GdkScreenGraphicsDevice (GdkGraphicsEnvironment e)
  {    
    super ();
    env = e;
  }

  public int getType ()
  {
    return GraphicsDevice.TYPE_RASTER_SCREEN;
  }

  public String getIDstring ()
  {
    // FIXME: query X for this string
    return "default GDK device ID string";
  }

  public GraphicsConfiguration[] getConfigurations ()
  {
    // FIXME: query X for the list of possible configurations
    return new GraphicsConfiguration [] { new GdkGraphicsConfiguration(this) };
  }

  public GraphicsConfiguration getDefaultConfiguration ()
  {
    
    // FIXME: query X for default configuration
    return new GdkGraphicsConfiguration(this);
  }


  /**
   * Returns the current display mode of this device, or null if unknown.
   *
   * @return the current display mode
   * @see #setDisplayMode(DisplayMode)
   * @see #getDisplayModes()
   * @since 1.4
   */
  public DisplayMode getDisplayMode()
  {
    // determine display mode
    Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
    DisplayMode mode = new DisplayMode(dim.width, dim.height, 0,
				       DisplayMode.REFRESH_RATE_UNKNOWN);
    return mode;
  }

  /**
   * This device does not yet support fullscreen exclusive mode, so this
   * returns <code>false</code>.
   *
   * @return <code>false</code>
   * @since 1.4
   */
  public boolean isFullScreenSupported()
  {
    return false;
  }

}
