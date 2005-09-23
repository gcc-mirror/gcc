/* QtScreenDevice.java -- Wrapper on a Qt screen Widget
   Copyright (C)  2005  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.qt;

import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.DisplayMode;
import java.awt.GraphicsConfigTemplate;
import java.awt.Rectangle;
import java.awt.Window;

public class QtScreenDevice extends GraphicsDevice
{
  private long nativeObject;
  private int id;
  private String IDstring;
  QtScreenDeviceConfiguration config;

  public QtScreenDevice(int id)
  {
    this.id = id;
    IDstring = "QtScreen" + id;
    init( id );
    config = new QtScreenDeviceConfiguration(this);
  }

  public native void init( int id );
  public native void dispose();

  // Package-private methods used by QtScreenDeviceConfiguration
  native Rectangle getBounds();
  native int getDpiX();
  native int getDpiY();
  native int depth();

  // ****************** Public methods ***********************

  public GraphicsConfiguration getBestConfiguration(GraphicsConfigTemplate gct)
  {
    return config;
  }

  public GraphicsConfiguration[] getConfigurations()
  {
    return new GraphicsConfiguration[]{ config };
  }

  public GraphicsConfiguration getDefaultConfiguration()
  {
    return config;
  }

  public String getIDstring()
  {
    return IDstring;
  }

  public int getType()
  {
    return TYPE_RASTER_SCREEN;
  }

  public boolean isDisplayChangeSupported()
  {
    return false;
  }

  public boolean isFullScreenSupported()
  {
    return false;
  }

  public void setDisplayMode(DisplayMode dm)
  {
  }

  public void setFullScreenWindow(Window w)
  {
  }
}

