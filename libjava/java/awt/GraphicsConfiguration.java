/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Status: Complete, but commented out until we have the required
   GraphicsDevice. */

package java.awt;

import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.geom.AffineTransform;

public abstract class GraphicsConfiguration
{
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
