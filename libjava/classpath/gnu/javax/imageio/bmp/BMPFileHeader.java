/* BMPFileHeader.java --
   Copyright (C)  2005  Free Software Foundation, Inc.

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

import java.awt.image.RenderedImage;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import javax.imageio.IIOImage;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.ImageOutputStream;

public class BMPFileHeader {
    /** Header signature, always 'BM' */
    private final static short bfType = 0x424d;

    /** Bitmap file size, in bytes. */
    protected long bfSize;
    
    /** Offset from the beginning of the file to the bitmap data */
    protected long bfOffBits;

    /** BITMAPFILEHEADER is 14 bytes */
    public static final int SIZE = 14;
    private static final int BITMAPINFOHEADER_SIZE = 40;
    
    /**
     * Creates the header from an input stream, which is not closed.
     * 
     * @throws IOException if an I/O error occured.
     * @throws BMPException if the header was invalid
     */
    public BMPFileHeader(ImageInputStream in) throws IOException, BMPException {
        byte[] data = new byte[SIZE];

	if (in.read(data) != SIZE)
	    throw new IOException("Couldn't read header.");
	ByteBuffer buf = ByteBuffer.wrap(data);

	if(buf.getShort(0) != bfType)
	    throw new BMPException("Not a BMP file.");

	buf.order(ByteOrder.LITTLE_ENDIAN);

	// get size (keep unsigned)
	bfSize = ((long)buf.getInt(2) & (0xFFFFFFFF));

	// Two reserved shorts are here, and should be zero,
	// perhaps they should be tested to be zero, but I don't
	// feel this strictness is necessary.
	
	bfOffBits = ((long)buf.getInt(10) & (0xFFFFFFFF));
    }
    
    /**
     * Creates the header from an output stream, which is not closed.
     * 
     * @param out - the image output stream
     * @param im - the image
     * @throws IOException if an I/O error occured.
     */
  public BMPFileHeader(ImageOutputStream out, IIOImage im) throws IOException
  {
    RenderedImage img = im.getRenderedImage();
    int w = img.getWidth();
    int h = img.getHeight();
    
    bfOffBits = SIZE + BITMAPINFOHEADER_SIZE;
    bfSize = ((w * h) * 3) + ((4 - ((w * 3) % 4)) * h) + bfOffBits;

    write(out);
  }

    /**
     * Writes the header to an output stream, which is not closed or flushed.
     * 
     * @throws IOException if an I/O error occured.
     */
    public void write(ImageOutputStream out) throws IOException {
	ByteBuffer buf = ByteBuffer.allocate(SIZE);
	buf.putShort(0, bfType); // ID
	buf.putInt(2, (int)(bfSize & (0xFFFFFFFF))); // size
	buf.putInt(6, 0); // 4 reserved bytes set to zero
	buf.putInt(7, (int)(bfOffBits & (0xFFFFFFFF))); // size
	out.write(buf.array());
    }
    
    /**
     * Sets the file size
     */
    public void setSize(long size){
	bfSize = size;
    }

    /**
     * Sets the bitmap offset within the file
     */
    public void setOffset(long offset){
	bfOffBits = offset;
    }

    /**
     * Gets the file size
     */
    public long getSize(){
	return bfSize;
    }

    /**
     * Gets the bitmap offset within the file
     */
    public long getOffset(){
	return bfOffBits;
    }
}


