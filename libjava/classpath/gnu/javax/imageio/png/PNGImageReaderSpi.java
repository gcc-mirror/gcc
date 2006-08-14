/* PNGImageReaderSpi.java -- The ImageReader service provider for PNG
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


package gnu.javax.imageio.png;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;

import javax.imageio.ImageReader;
import javax.imageio.spi.ImageReaderSpi;
import javax.imageio.stream.ImageInputStream;

/**
 * The ImageIO ImageReader service provider for PNG images.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class PNGImageReaderSpi
  extends ImageReaderSpi
{

  /**
   * The PNG file signature.
   */
  private static final byte[] SIGNATURE = new byte[]
                              { (byte) 137, 80, 78, 71, 13, 10, 26, 10 };

  private static final String VENDOR_NAME = "GNU";
  static final String VERSION = "1.0";
  static final String READER_CLASSNAME =
                                       "gnu.javax.imageio.png.PNGImageReader";
  static final String[] NAMES = { "Portable Network Graphics" };
  static final String[] SUFFIXES = { ".png" , ".PNG" };
  static final String[] MIME_TYPES = { "image/png" };
  static final String[] WRITER_SPI_NAMES =
    new String[] { "gnu.javax.imageio.png.PNGWriterSpi" };
  static final Class[] INPUT_TYPES = new Class[]{ InputStream.class,
                                                  ImageInputStream.class};
  public PNGImageReaderSpi()
  {
    super(VENDOR_NAME, VERSION, NAMES, SUFFIXES, MIME_TYPES, READER_CLASSNAME,
          INPUT_TYPES, WRITER_SPI_NAMES, false, null, null, null, null, false,
          null, null, null, null);
  }

  /**
   * Determines if the PNG ImageReader can decode the specified input.
   *
   * @param source the source to decode
   */
  public boolean canDecodeInput(Object source) throws IOException
  {
    boolean canDecode = false;
    if (source instanceof ImageInputStream)
      {
        ImageInputStream in = (ImageInputStream) source;
        in.mark();
        canDecode = true;
        for (int i = 0; i < SIGNATURE.length && canDecode; i++)
          {
            byte sig = (byte) in.read();
            if (sig != SIGNATURE[i]) {
              canDecode = false;
            }
          }
        in.reset();
      }
    return canDecode;
  }

  /**
   * Returns a new PNGImageReader instance.
   *
   * @param extension the extension, ignored
   */
  public ImageReader createReaderInstance(Object extension)
    throws IOException
  {
    return new PNGImageReader(this);
  }

  /**
   * Returns a description.
   *
   * @param locale the locale
   */
  public String getDescription(Locale locale)
  {
    return "Portable Network Graphics";
  }

}
