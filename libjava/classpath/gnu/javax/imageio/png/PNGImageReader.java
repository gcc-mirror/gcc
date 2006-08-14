/* PNGImageReader.java -- The ImageIO ImageReader for PNG
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

import gnu.javax.imageio.IIOInputStream;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;

import javax.imageio.ImageReadParam;
import javax.imageio.ImageReader;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.stream.ImageInputStream;

/**
 * The ImageIO ImageReader for PNG images.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class PNGImageReader
  extends ImageReader
{

  /**
   * The PNG file.
   */
  private PNGFile pngFile;

  /**
   * The decoded image.
   */
  private BufferedImage image;

  /**
   * The supported image types for PNG.
   */
  private ArrayList imageTypes;

  /**
   * Creates a new instance.
   *
   * @param spi the corresponding ImageReaderSpi
   */
  public PNGImageReader(PNGImageReaderSpi spi)
  {
    super(spi);
  }

  /**
   * Returns the height of the image.
   */
  public int getHeight(int imageIndex)
    throws IOException
  {
    checkIndex(imageIndex);
    readImage();
    return image.getHeight();
  }

  /**
   * Returns the width of the image.
   *
   * @param imageIndex the index of the image
   *
   * @return the width of the image
   */
  public int getWidth(int imageIndex) throws IOException
  {
    checkIndex(imageIndex);
    readImage();
    return image.getWidth();
  }

  /**
   * Returns the image types for the image.
   *
   * @see ImageReader#getImageTypes(int)
   */
  public Iterator getImageTypes(int imageIndex)
    throws IOException
  {
    checkIndex(imageIndex);
    readImage();
    if (imageTypes == null)
      {
        imageTypes = new ArrayList();
        imageTypes.add(new ImageTypeSpecifier(image.getColorModel(),
                                              image.getSampleModel()));
      }
    return imageTypes.iterator();
  }

  /**
   * Returns the number of images in the stream.
   *
   * @return the number of images in the stream
   *
   * @see ImageReader#getNumImages(boolean)
   */
  public int getNumImages(boolean allowSearch)
    throws IOException
  {
    return 1;
  }

  /**
   * Reads the image.
   *
   * @param imageIndex the index of the image to read
   * @param param additional parameters
   */
  public BufferedImage read(int imageIndex, ImageReadParam param)
    throws IOException
  {
    checkIndex(imageIndex);
    readImage();
    return image;
  }

  /**
   * Sets the input and checks the input parameter.
   *
   * @see ImageReader#setInput(Object, boolean, boolean)
   */
  public void setInput(Object input, 
                       boolean seekForwardOnly, 
                       boolean ignoreMetadata) 
  {
    super.setInput(input, seekForwardOnly, ignoreMetadata);
    if (! (input instanceof InputStream || input instanceof ImageInputStream))
      throw new IllegalArgumentException("Input not an ImageInputStream");
  }

  public IIOMetadata getImageMetadata(int imageIndex)
    throws IOException
  {
    // TODO: Not (yet) supported.
    checkIndex(imageIndex);
    return null;
  }

  public IIOMetadata getStreamMetadata()
    throws IOException
  {
    // TODO: Not (yet) supported.
    return null;
  }

  /**
   * Checks the image indexa and throws and IndexOutOfBoundsException if
   * appropriate.
   *
   * @param index the index to check
   */
  private void checkIndex(int index)
  {
    if (index > 0)
      throw new IndexOutOfBoundsException("Image index out of bounds");
  }

  /**
   * Makes sure that the image is read.
   *
   * @throws IOException if something goes wrong
   */
  private void readImage()
    throws IOException
  {
    if (pngFile == null)
      {
        if (input instanceof InputStream)
          pngFile = new PNGFile((InputStream) input);
        else if (input instanceof ImageInputStream)
          pngFile = new PNGFile(new IIOInputStream((ImageInputStream) input));
        else
          assert false : "Must not happen";
      }

    if (pngFile != null && image == null)
      {
        image = pngFile.getBufferedImage();
      }
  }
}
