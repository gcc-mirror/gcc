/* RasterGraphics.java -- A Graphics2D impl for Rasters
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


package gnu.java.awt.java2d;

import java.awt.GraphicsConfiguration;
import java.awt.image.ColorModel;
import java.awt.image.WritableRaster;

/**
 * A Graphics2D implementation that operates on Raster objects. This is
 * primarily used for BufferedImages, but can theoretically be used on
 * arbitrary WritableRasters.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class RasterGraphics
  extends AbstractGraphics2D
{

  /**
   * The raster on which we operate.
   */
  private WritableRaster raster;

  /**
   * The color model of this Graphics instance.
   */
  private ColorModel colorModel;

  public RasterGraphics(WritableRaster r, ColorModel cm)
  {
    super();
    raster = r;
    colorModel = cm;
    init();
  }

  /**
   * Returns the color model of this Graphics object.
   *
   * @return the color model of this Graphics object
   */
  protected ColorModel getColorModel()
  {
    return colorModel;
  }

  /**
   * Returns a WritableRaster that is used by this class to perform the
   * rendering in. It is not necessary that the target surface immediately
   * reflects changes in the raster. Updates to the raster are notified via
   * {@link AbstractGraphics2D#updateRaster}.
   *
   * @return the destination raster
   */
  protected WritableRaster getDestinationRaster()
  {
    return raster;
  }

  public GraphicsConfiguration getDeviceConfiguration()
  {
    // TODO Auto-generated method stub
    return null;
  }

}
