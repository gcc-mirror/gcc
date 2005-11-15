/* Copyright (C) 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.GraphicsDevice;
import java.awt.GraphicsConfiguration;

public class XGraphicsDevice extends GraphicsDevice
{
  private int id;
  private String IDstring;
  private GraphicsConfiguration[] configs;

  public int getType()
  {
    return TYPE_RASTER_SCREEN;
  }

  public XGraphicsDevice(int id, XToolkit toolkit)
  {
    this.id = id;
    IDstring = "XGraphicsDevice " + id;
    configs = new GraphicsConfiguration [1];
    configs[0] = toolkit.getDefaultXGraphicsConfiguration();
  }

  public String getIDstring()
  {
    return IDstring;
  }

  public GraphicsConfiguration[] getConfigurations()
  {
    return configs;
  }

  public GraphicsConfiguration getDefaultConfiguration()
  {
    return configs[0];
  }

  public boolean isDisplayChangeSupported()
  {
    return false;
  }

  public boolean isFullScreenSupported()
  {
    return false;
  }
}

