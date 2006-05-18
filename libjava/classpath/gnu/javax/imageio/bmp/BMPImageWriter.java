/* BMPImageWriter.java -- 
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
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.spi.ImageWriterSpi;
import javax.imageio.stream.ImageOutputStream;

public class BMPImageWriter
    extends ImageWriter
{
  protected BMPEncoder encoder;
  protected BMPFileHeader fileHeader;
  protected BMPInfoHeader infoHeader;

  /**
   * Construct an bmp image writer.
   * 
   * @param originatingProvider - the provider that is constructing this image
   *          writer, or null
   */
  protected BMPImageWriter(ImageWriterSpi originatingProvider)
  {
    super(originatingProvider);
    encoder = null;
    fileHeader = null;
    infoHeader = null;
  }
  
  /**
   * Convert IIOMetadata from an input reader format, returning an IIOMetadata
   * suitable for use by an image writer. The ImageTypeSpecifier specifies the
   * destination image type. An optional ImageWriteParam argument is available
   * in case the image writing parameters affect the metadata conversion.
   * 
   * @param inData - the metadata coming from an image reader
   * @param imageType - the output image type of the writer
   * @param param - the image writing parameters or null
   * @return the converted metadata that should be used by the image writer, or
   *         null if this ImageTranscoder has no knowledge of the input metadata
   * @exception IllegalArgumentException if either inData or imageType is null
   */
  public IIOMetadata convertImageMetadata(IIOMetadata inData,
                                          ImageTypeSpecifier imageType,
                                          ImageWriteParam param)
  {
    // FIXME: Support metadata.
    if (inData == null || imageType == null)
      throw new IllegalArgumentException("IIOMetadata and ImageTypeSpecifier cannot be null.");
    return null;
  }
  
  /**
   * Convert IIOMetadata from an input stream format, returning an
   * IIOMetadata suitable for use by an image writer.
   *
   * An optional ImageWriteParam argument is available in case the
   * image writing parameters affect the metadata conversion.
   *
   * @param inData - the metadata coming from an input image stream
   * @param param - the image writing parameters or null
   * @return the converted metadata that should be used by the image
   * writer, or null if this ImageTranscoder has no knowledge of the
   * input metadata
   *
   * @exception IllegalArgumentException if inData is null
   */
  public IIOMetadata convertStreamMetadata (IIOMetadata inData,
                                 ImageWriteParam param)
  {
    // FIXME: Support metadata.
    if (inData == null)
      throw new IllegalArgumentException("IIOMetadata cannot be null.");
    return null;
  }
  
  /**
   * Get a metadata object appropriate for encoding an image specified
   * by the given image type specifier and optional image write
   * parameters.
   *
   * @param imageType - an image type specifier
   * @param param - image writing parameters, or null
   * @return a metadata object appropriate for encoding an image of
   * the given type with the given parameters
   */
  public IIOMetadata getDefaultImageMetadata (ImageTypeSpecifier imageType, ImageWriteParam param)
  {
    // FIXME: Support metadata.
    return null;
  }
  
  /**
   * Get a metadata object appropriate for encoding the default image
   * type handled by this writer, optionally considering image write
   * parameters.
   *
   * @param param - image writing parameters, or null
   * @return a metadata object appropriate for encoding an image of
   * the default type with the given parameters
   */
  public IIOMetadata getDefaultStreamMetadata (ImageWriteParam param)
  {
    // FIXME: Support metadata.
    return null;
  }
  
  /**
   * Write an image stream, including thumbnails and metadata to the
   * output stream.  The output must have been set prior to this
   * method being called.  Metadata associated with the stream may be
   * supplied, or it can be left null.  IIOImage may contain raster
   * data if this writer supports rasters, or it will contain a
   * rendered image.  Thumbnails are resized if need be.  Image
   * writing parameters may be specified to affect writing, or may be
   * left null.
   *
   * @param streamMetadata - metadata associated with this stream, or
   * null
   * @param image - an IIOImage containing image data, metadata and
   * thumbnails to be written
   * @param param - image writing parameters, or null
   * @exception IOException if a write error occurs
   * @throws BMPException if the encoder has not been initialized.
   */
  public void write(IIOMetadata streamMetadata, IIOImage image,
                    ImageWriteParam param) throws IOException, BMPException
  {
    checkStream();
    ImageOutputStream out = (ImageOutputStream) output;
    fileHeader = new BMPFileHeader(out, image);
    infoHeader = new BMPInfoHeader(out, image, param);
    encoder = BMPEncoder.getEncoder(fileHeader, infoHeader);
    
    if (encoder != null)
      encoder.encode(out, streamMetadata, image, param);
    else
      throw new BMPException("Encoder has not been initialized.");
    out.close();
  }
  
  /**
   * Checks the output stream.
   * 
   * @throws IOException if there is an error with the output stream
   */
  private void checkStream() throws IOException
  {
    if (!(output instanceof ImageOutputStream))
      throw new IllegalStateException("Output not an ImageOutputStream.");
    if (output == null)
      throw new IllegalStateException("No output stream.");
  }
}
