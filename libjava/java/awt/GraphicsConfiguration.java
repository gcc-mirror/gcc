/* Copyright (C) 2000, 2001, 2002  Free Software Foundation

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

/* Status: Complete, but commented out until we have the required
   GraphicsDevice. */

package java.awt;

import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.geom.AffineTransform;

public abstract class GraphicsConfiguration
{
  // Can't instantiate directly.  Having a protected constructor seems
  // redundant, but that is what the docs specify.
  protected GraphicsConfiguration ()
  {
  }

  /*
  public abstract GraphicsDevice getDevice();
  */

  public abstract BufferedImage createCompatibleImage(int width, int height);
  public abstract BufferedImage createCompatibleImage(int width, int height,
                                                      int transparency);
  public abstract ColorModel getColorModel();
  public abstract ColorModel getColorModel(int transparency);
  public abstract AffineTransform getDefaultTransform();
  public abstract AffineTransform getNormalizingTransform();

  /* @since 1.3 */
  public abstract Rectangle getBounds();
}
