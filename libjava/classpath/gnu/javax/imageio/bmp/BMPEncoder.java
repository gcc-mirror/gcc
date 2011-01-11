/* BMPEncoder.java --
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


package gnu.javax.imageio.bmp;

import java.io.IOException;

import javax.imageio.IIOImage;
import javax.imageio.ImageWriteParam;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.stream.ImageOutputStream;

public abstract class BMPEncoder
{

  /**
   * Constructs a new BMPEncoder.
   */
  public BMPEncoder()
  {
    // Nothing to do here.
  }

  /**
   * Determines the coding type of the bitmap and returns the corresponding
   * encoder.
   *
   * @param fh - the file header
   * @param ih - the info header
   * @return the appropriate encoder
   */
  public static BMPEncoder getEncoder(BMPFileHeader fh, BMPInfoHeader ih)
  {
    switch (ih.getCompression())
      {
        case BMPInfoHeader.BI_RGB:
        switch (ih.getBitCount())
          {
          case 32:
            return new EncodeRGB32(fh, ih);

          case 24:
            return new EncodeRGB24(fh, ih);

          case 16:
            return new EncodeRGB16(fh, ih);

          case 8:
            return new EncodeRGB8(fh, ih);

          case 4:
            return new EncodeRGB4(fh, ih);

          case 1:
            return new EncodeRGB1(fh, ih);

          default:
            return null;
          }
      case BMPInfoHeader.BI_RLE4:
        return new EncodeRLE4(fh, ih);

      case BMPInfoHeader.BI_RLE8:
        return new EncodeRLE8(fh, ih);
      default:
        return null;
      }
  }

  /**
   * The image encoder.
   *
   * @param o - the image output stream
   * @param streamMetadata - metadata associated with this stream, or
   * null
   * @param image - an IIOImage containing image data, metadata and
   * thumbnails to be written
   * @param param - image writing parameters, or null
   * @exception IOException if a write error occurs
   */
  public abstract void encode(ImageOutputStream o, IIOMetadata streamMetadata,
                              IIOImage image, ImageWriteParam param)
      throws IOException;
}
