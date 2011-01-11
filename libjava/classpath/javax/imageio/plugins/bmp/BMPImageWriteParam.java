/* BMPImageWriteParam.java --
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


package javax.imageio.plugins.bmp;

import java.util.Locale;

import javax.imageio.ImageWriteParam;

/**
 * A class to encode images in the BMP format.
 * By default, the data layout is bottom-up, such that the pixels are stored in
 * bottom-up order.
 *
 * The compression scheme can be specified by using setCompressionType()
 * appropriate type string. The compression scheme specified will be honored
 * if it is compatible with the type of image being written. If the
 * compression scheme is not compatible with the type of image being written,
 * then an IOException will be thrown by the BMP image writer. If the
 * compression type is not set, then getCompressionType() will return null.
 * In this case the BMP image writer will select a compression type that
 * supports encoding of the given image without loss of the color resolution.
 *
 * The compression type strings and the image type each supports are:
 * Uncompressed RLE: BI_RGB, image type: <= 8-bits/sample.
 * 8-bit Run Length Encoding: BI_RLE8, image type: <= 8-bits/sample
 * 4-bit Run Length Encoding: BI_RLE4, image type: <= 4-bits/sample
 * Packed data: BI_BITFIELDS, image type: 16 or 32 bits/sample
 *
 * @author Lillian Angel (langel at redhat dot com)
 */
public class BMPImageWriteParam
    extends ImageWriteParam
{

  /**
   * This boolean is true if the data will be written in a topdown manner.
   */
  private boolean topDown;

  /**
   * Compression type strings.
   */
  String rgb = "BI_RGB";
  String rle8 = "BI_RLE8";
  String rle4 = "BI_RLE4";
  String bitfields = "BI_BITFIELDS";

  /**
   * Constants to represent image types.
   */
  static final int BI_RGB = 0;
  static final int BI_RLE8 = 1;
  static final int BI_RLE4 = 2;
  static final int BI_BITFIELDS = 3;

  /**
   * Constructs an <code>BMPImageWriteParam</code> object with default values
   * and a <code>null Locale</code>.
   */
  public BMPImageWriteParam()
  {
    this(null);
  }

  /**
   * Constructs a <code>BMPImageWriteParam</code> set to use a given
   * <code>Locale</code> and with default values for all parameters.
   *
   * @param locale - a <code>Locale</code> to be used to localize compression
   *          type names and quality descriptions, or <code>null</code>.
   */
  public BMPImageWriteParam(Locale locale)
  {
    super(locale);
    topDown = false;
    canWriteCompressed = true;

    compressionTypes = new String[4];
    compressionTypes[BI_RGB] = rgb;
    compressionTypes[BI_RLE8] = rle8;
    compressionTypes[BI_RLE4] = rle4;
    compressionTypes[BI_BITFIELDS] = bitfields;

    compressionType = compressionTypes[BI_RGB];
  }

  /**
   * If set, the data will be written out in a top-down manner, the first
   * scanline being written first.
   *
   * @param topDown - whether the data are written in top-down order.
   */
  public void setTopDown(boolean topDown)
  {
    this.topDown = topDown;
  }

  /**
   * Returns the value of the <code>topDown</code> parameter. The default is
   * false.
   *
   * @return whether the data are written in top-down order.
   */
  public boolean isTopDown()
  {
    return topDown;
  }
}
