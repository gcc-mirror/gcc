/* BMPImageWriterSpi.java -- 
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

import java.util.Locale;

import javax.imageio.ImageTypeSpecifier;
import javax.imageio.ImageWriter;
import javax.imageio.spi.ImageWriterSpi;

public class BMPImageWriterSpi
    extends ImageWriterSpi
{

  static final String vendorName = "GNU";
  static final String version = "0.1";
  static final String writerClassName = "gnu.javax.imageio.bmp.BMPImageWriter";
  static final String[] names = { "Microsoft Windows BMP" };
  static final String[] suffixes = { ".bmp", ".bm" };
  static final String[] MIMETypes = { "image/bmp", "image/x-windows-bmp" };
  static final String[] readerSpiNames = { "gnu.javax.imageio.bmp.BMPImageReaderSpi" };
  
  static final boolean supportsStandardStreamMetadataFormat = false;
  static final String nativeStreamMetadataFormatName = null;
  static final String nativeStreamMetadataFormatClassName = null;
  static final String[] extraStreamMetadataFormatNames = null;
  static final String[] extraStreamMetadataFormatClassNames = null;
  static final boolean supportsStandardImageMetadataFormat = false;
  static final String nativeImageMetadataFormatName = null;
  static final String nativeImageMetadataFormatClassName = null;
  static final String[] extraImageMetadataFormatNames = null;
  static final String[] extraImageMetadataFormatClassNames = null;
  
  private BMPImageWriter writerInstance;
  
  public BMPImageWriterSpi()
  {
    super(vendorName, version, names, suffixes, MIMETypes, writerClassName,
          STANDARD_OUTPUT_TYPE, readerSpiNames, supportsStandardStreamMetadataFormat,
          nativeStreamMetadataFormatName, nativeStreamMetadataFormatClassName,
          extraStreamMetadataFormatNames, extraStreamMetadataFormatClassNames,
          supportsStandardImageMetadataFormat, nativeImageMetadataFormatName,
          nativeImageMetadataFormatClassName, extraImageMetadataFormatNames,
          extraImageMetadataFormatClassNames);
  }
  
  /**
   * Returns true if the image can be encoded.
   * 
   * @param type - the image type specifier.
   * @return true if image can be encoded, otherwise false.
   */
  public boolean canEncodeImage(ImageTypeSpecifier type)
  {
    if (type == null)
      return false;
    
    BMPInfoHeader ih = writerInstance.infoHeader;
    if (ih != null)
      {
        int compressionType = ih.getCompression();
        int bytes = type.getColorModel().getPixelSize();
        if ((compressionType == BMPInfoHeader.BI_RLE4 && (bytes != 4 || bytes != 8))
            || (compressionType == BMPInfoHeader.BI_RGB && ((bytes != 1
                                                             || bytes != 4
                                                             || bytes != 8
                                                             || bytes != 16
                                                             || bytes != 24 
                                                             || bytes != 32))))
          return false;
      }
    return true;
  }

  /**
   * Creates an instance of ImageWriter using the given extension.
   * 
   * @param extension - the provider that is constructing this image writer, or
   *          null
   */
  public ImageWriter createWriterInstance(Object extension)
  {
    if (extension != null && extension instanceof ImageWriterSpi)
      writerInstance = new BMPImageWriter((ImageWriterSpi) extension);
    else
      writerInstance = new BMPImageWriter(this);
    return writerInstance;
  }
  
  /**
   * Gets the instance of ImageWriter, if already created.
   */
  public BMPImageWriter getWriterInstance()
  {
    if (writerInstance != null)
      return writerInstance;
    return (BMPImageWriter) createWriterInstance(null);
  }

  /**
   * Returns a short description of this service provider that can be
   * presented to a human user.
   *
   * @param locale - the locale for which the description string should
   * be localized.
   */
  public String getDescription(Locale locale)
  {
    return "Microsoft BMP v3";
  }

}
