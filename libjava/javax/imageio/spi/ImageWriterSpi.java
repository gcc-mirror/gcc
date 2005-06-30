/* ImageWriterSpi.java --
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package javax.imageio.spi;

import java.awt.image.RenderedImage;
import java.io.IOException;

import javax.imageio.ImageTypeSpecifier;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageOutputStream;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public abstract class ImageWriterSpi extends ImageReaderWriterSpi
{
  public static final Class[] STANDARD_OUTPUT_TYPE =
    { ImageOutputStream.class };

  protected Class[] outputTypes;
  protected String[] readerSpiNames;

  protected ImageWriterSpi()
  {
    // Do nothing here.
  }

  public ImageWriterSpi(String vendorName, String version, String[] names,
                        String[] suffixes, String[] MIMETypes,
                        String writerClassName, Class[] outputTypes,
                        String[] readerSpiNames,
                        boolean supportsStandardStreamMetadataFormat,
                        String nativeStreamMetadataFormatName,
                        String nativeStreamMetadataFormatClassName,
                        String[] extraStreamMetadataFormatNames,
                        String[] extraStreamMetadataFormatClassNames,
                        boolean supportsStandardImageMetadataFormat,
                        String nativeImageMetadataFormatName,
                        String nativeImageMetadataFormatClassName,
                        String[] extraImageMetadataFormatNames,
                        String[] extraImageMetadataFormatClassNames)
  {
    super(vendorName, version, names, suffixes, MIMETypes, writerClassName,
          supportsStandardStreamMetadataFormat, nativeStreamMetadataFormatName,
          nativeStreamMetadataFormatClassName, extraStreamMetadataFormatNames,
          extraStreamMetadataFormatClassNames, supportsStandardImageMetadataFormat,
          nativeImageMetadataFormatName, nativeImageMetadataFormatClassName,
          extraImageMetadataFormatNames, extraImageMetadataFormatClassNames);

    if (writerClassName == null)
      throw new IllegalArgumentException("writerClassName is null");

    if (outputTypes == null
        || outputTypes.length == 0)
      throw new IllegalArgumentException("outputTypes may not be null or empty");
    
    this.outputTypes = outputTypes;
    this.readerSpiNames = readerSpiNames;    
  }

  public abstract boolean canEncodeImage(ImageTypeSpecifier type);

  public boolean canEncodeImage(RenderedImage image)
  {
    return canEncodeImage (new ImageTypeSpecifier(image));
  }

  public ImageWriter createWriterInstance()
    throws IOException
  {
    return createWriterInstance(null);
  }

  public abstract ImageWriter createWriterInstance(Object extension)
    throws IOException;

  public String[] getImageReaderSpiNames()
  {
    return readerSpiNames;
  }

  public Class[] getOutputTypes()
  {
    return outputTypes;
  }

  public boolean isFormatLossless()
  {
    return true;
  }

  public boolean isOwnWriter(ImageWriter writer)
  {
    if (writer == null)
      throw new IllegalArgumentException("writer may not be null");

    return pluginClassName.equals(writer.getClass().getName());
  }
}
