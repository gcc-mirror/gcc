/* ImageTranscoder.java -- Image metadata transcoder.
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


package javax.imageio;

import javax.imageio.metadata.IIOMetadata;

/**
 * An ImageTranscoder translates IIOMetadata objects provided by an
 * ImageReader into corresponding IIOMetadata objects that can be
 * understood by a given ImageWriter.
 *
 * Usually an ImageWriter will implement ImageTranscoder directly in
 * which case the conversion methods will return IIOMetadata objects
 * appropriate for this ImageWriter.
 *
 * Independent transcoders are also allowed; they must have knowledge
 * of both the source IIOMetadata provided by the reader and the
 * returned IIOMetadata expected by the writer.
 *
 * @author Michael Koch (konqueror@gmx.de)
 */
public interface ImageTranscoder
{
  /**
   * Converts IIOMetadata from an input reader format, returning an
   * IIOMetadata suitable for use by an image writer.
   *
   * The ImageTypeSpecifier specifies the destination image type.
   *
   * An optional ImageWriteParam argument is available in case the
   * image writing parameters affect the metadata conversion.
   *
   * @param inData the metadata coming from an image reader
   * @param imageType the output image type of the writer
   * @param param the image writing parameters or null
   *
   * @return the converted metadata that should be used by the image
   * writer, or null if this ImageTranscoder has no knowledge of the
   * input metadata
   *
   * @exception IllegalArgumentException if either inData or imageType
   * is null
   */
  IIOMetadata convertImageMetadata(IIOMetadata inData,
                                   ImageTypeSpecifier imageType,
                                   ImageWriteParam param);

  /**
   * Converts IIOMetadata from an input stream format, returning an
   * IIOMetadata suitable for use by an image writer.
   *
   * An optional ImageWriteParam argument is available in case the
   * image writing parameters affect the metadata conversion.
   *
   * @param inData the metadata coming from an input image stream
   * @param param the image writing parameters or null
   *
   * @return the converted metadata that should be used by the image
   * writer, or null if this ImageTranscoder has no knowledge of the
   * input metadata
   *
   * @exception IllegalArgumentException if inData is null
   */
  IIOMetadata convertStreamMetadata(IIOMetadata inData,
                                    ImageWriteParam param);
}
