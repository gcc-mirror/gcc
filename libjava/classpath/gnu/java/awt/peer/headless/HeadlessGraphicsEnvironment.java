/* HeadlessGraphicsEnvironment.java -- A graphics environment for headless mode 
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


package gnu.java.awt.peer.headless;

import gnu.java.awt.java2d.RasterGraphics;

import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.image.BufferedImage;
import java.awt.image.Raster;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.Locale;

public class HeadlessGraphicsEnvironment
    extends GraphicsEnvironment
{

  public Graphics2D createGraphics(BufferedImage image)
  {
    Graphics2D g2d;
    try
      {
        // Try to get a CairoGraphics (accellerated) when available. Do this
        // via reflection to avoid having a hard compile time dependency.
        Class cairoSurfaceCl =
          Class.forName("gnu.java.awt.peer.gtk.CairoSurface");
        Raster raster = image.getRaster();
        if (cairoSurfaceCl.isInstance(raster))
          {
            Method getGraphicsM = cairoSurfaceCl.getMethod("getGraphics",
                                                           new Class[0]);
            g2d = (Graphics2D) getGraphicsM.invoke(raster, new Object[0]);
          }
        else
          {
            Class bigCl =
              Class.forName("gnu.java.awt.peer.gtk.BufferedImageGraphics");
            Constructor bigC =
              bigCl.getConstructor(new Class[]{BufferedImage.class });
            g2d = (Graphics2D) bigC.newInstance(new Object[]{ image});
          }
      }
    catch (Exception ex)
      {
        g2d = new RasterGraphics(image.getRaster(), image.getColorModel());
      }
    return g2d;
  }

  public Font[] getAllFonts()
  {
    // FIXME: Implement.
    return null;
  }

  public String[] getAvailableFontFamilyNames()
  {
    // FIXME: Implement.
    return null;
  }

  public String[] getAvailableFontFamilyNames(Locale l)
  {
    // FIXME: Implement.
    return null;
  }

  public GraphicsDevice getDefaultScreenDevice()
  {
    throw new HeadlessException();
  }

  public GraphicsDevice[] getScreenDevices()
  {
    throw new HeadlessException();
  }

}
