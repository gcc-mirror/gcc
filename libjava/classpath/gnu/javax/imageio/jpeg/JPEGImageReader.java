/* JPEGImageReader.java --
   Copyright (C)  2006  Free Software Foundation, Inc.

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

package gnu.javax.imageio.jpeg;

import java.io.IOException;
import javax.imageio.*;
import javax.imageio.spi.*;
import javax.imageio.metadata.*;
import javax.imageio.stream.ImageInputStream;
import java.util.Iterator;
import java.awt.image.BufferedImage;

public class JPEGImageReader extends ImageReader
{
  JPEGDecoder decoder;

  protected JPEGImageReader(ImageReaderSpi originatingProvider)
  {
    super(originatingProvider);
    System.out.println("JPEGIMAGEREADER!!!");
  }

  // Abstract ImageReader methods.
  public int getHeight(int imageIndex)
    throws IOException
  {
    checkIndex(imageIndex);
    decodeStream();
    return decoder.getHeight();
  }

  public IIOMetadata getImageMetadata(int imageIndex)
    throws IOException
  {
    // FIXME: handle metadata
    checkIndex(imageIndex);
    return null;
  }

  public Iterator getImageTypes(int imageIndex)
    throws IOException
  {
    return null;
  }

  public int getNumImages(boolean allowSearch)
    throws IOException
  {
    return 1;
  }

  public IIOMetadata getStreamMetadata()
    throws IOException
  {
    // FIXME: handle metadata
    return null;
  }

  public int getWidth(int imageIndex)
    throws IOException
  {
    checkIndex(imageIndex);
    decodeStream();
    return decoder.getWidth();
  }

  public BufferedImage read(int imageIndex, ImageReadParam param)
    throws IOException
  {
    checkIndex(imageIndex);
    decodeStream();
    return decoder.getImage();
  }

  // private helper methods
  private void checkIndex(int imageIndex)
    throws IndexOutOfBoundsException
  {
    if (imageIndex != 0)
      throw new IndexOutOfBoundsException();
  }

  private void checkStream() throws IOException
  {
    if (!(input instanceof ImageInputStream)) 
      throw new IllegalStateException("Input not an ImageInputStream.");
    if(input == null)
      throw new IllegalStateException("No input stream.");
  }

  private void decodeStream()
    throws IOException, IIOException
  {
    System.out.println("DECONDING 1");
    if (decoder != null)
      return;

    System.out.println("DECONDING 2");
    checkStream();

    System.out.println("DECONDING 3");
    decoder = new JPEGDecoder((ImageInputStream)input);
    System.out.println("DECONDING 4");
    decoder.decode();
  }
}
