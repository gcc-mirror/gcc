/* Copyright (C) 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.GraphicsEnvironment;
import java.awt.GraphicsDevice;
import java.awt.Graphics2D;
import java.awt.Font;
import java.awt.image.BufferedImage;
import java.util.Locale;

public class XGraphicsEnvironment extends GraphicsEnvironment
{
  private XToolkit toolkit;
  private XGraphicsDevice [] devices;

  XGraphicsEnvironment (XToolkit toolkit)
  {
    this.toolkit = toolkit;
    devices = new XGraphicsDevice [1];
    devices [0] = new XGraphicsDevice (0,toolkit);
  }

  public GraphicsDevice[] getScreenDevices ()
  {
    return devices;
  }

  public GraphicsDevice getDefaultScreenDevice ()
  {
    return devices [0];
  }

  public Graphics2D createGraphics (BufferedImage image)
  {
    throw new UnsupportedOperationException ("createGraphics not implemented yet in " + this.getClass ().getName ());
  }

  public Font[] getAllFonts()
  {
    throw new UnsupportedOperationException ("getAllFonts not implemented yet in " + this.getClass ().getName ());
  }

  public String[] getAvailableFontFamilyNames (Locale l)
  {
    throw new UnsupportedOperationException ("getAvailableFontFamilyNames not implemented yet in " + this.getClass ().getName ());
  }

  public String[] getAvailableFontFamilyNames ()
  {
    throw new UnsupportedOperationException ("getAvailableFontFamilyNames not implemented yet in " + this.getClass ().getName ());
  }
}
