/* ImageReaderSpi.java --
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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

import java.io.IOException;

import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public abstract class ImageReaderSpi extends ImageReaderWriterSpi
{
  public static final Class[] STANDARD_INPUT_TYPE =
    { ImageInputStream.class };

  protected Class[] inputTypes;
  protected String[] writerSpiNames;

  protected ImageReaderSpi()
  {
    // Do nothing here.
  }

  public ImageReaderSpi(String vendorName, String version, String[] names,
                        String[] suffixes, String[] MIMETypes,
                        String readerClassName, Class[] inputTypes,
                        String[] writerSpiNames,
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
    super(vendorName, version, names, suffixes, MIMETypes, readerClassName,
          supportsStandardStreamMetadataFormat, nativeStreamMetadataFormatName,
          nativeStreamMetadataFormatClassName, extraStreamMetadataFormatNames,
          extraStreamMetadataFormatClassNames, supportsStandardImageMetadataFormat,
          nativeImageMetadataFormatName, nativeImageMetadataFormatClassName,
          extraImageMetadataFormatNames, extraImageMetadataFormatClassNames);

    if (inputTypes == null
        || inputTypes.length == 0)
      throw new IllegalArgumentException("inputTypes may not be null or empty");
    
    this.inputTypes = inputTypes;
    this.writerSpiNames = writerSpiNames;
  }

  public abstract boolean canDecodeInput(Object source)
    throws IOException;

  public ImageReader createReaderInstance()
    throws IOException
  {
    return createReaderInstance(null);
  }

  public abstract ImageReader createReaderInstance(Object extension)
    throws IOException;

  public String[] getImageWriterSpiNames()
  {
    return writerSpiNames;
  }

  public Class[] getInputTypes()
  {
    return inputTypes;
  }

  public boolean isOwnReader(ImageReader reader)
  {
    if (reader == null)
      throw new IllegalArgumentException("reader may not be null");
    
    return pluginClassName.equals(reader.getClass().getName());
  }
}
