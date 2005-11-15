/* BMPInfoHeader.java --
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

import java.io.IOException;
import javax.imageio.stream.ImageInputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.awt.Dimension;

public class BMPInfoHeader {
    /** Size of the bitmap info header*/
    private int biSize; 

    /** Pixel width of the bitmap */
    private int biWidth; 

    /** Pixel height of the bitmap */
    private int biHeight;

    /** Number of bitplanes = 1 */
    private short biPlanes;

    /** Number of bpp = 1,4,8,24 */
    private short biBitCount;

    /** Compression type, RGB8, RLE8, RLE4, BITFIELDS */
    private int biCompression;

    /** Byte size of the uncompressed bitmap, can be 0. */
    private int biSizeImage;

    /** X resolution, dots per meter */
    private int biXPelsPerMeter;

    /** Y resolution, dots per meter */
    private int biYPelsPerMeter;

    /** Number of colors used (palette only, can be 0 for all) */
    private int biClrUsed;

    /** Number of 'important' colors, 0 for all */
    private int biClrImportant;

    /** BITMAPINFOHEADER is 40 bytes */
    public static final int SIZE = 40;

    /**
     * Compression types
     */
    public static final int BI_RGB = 0;
    public static final int BI_RLE8 = 1;
    public static final int BI_RLE4 = 2;
    public static final int BI_BITFIELDS = 3;

    /**
     * Creates the header from an input stream, which is not closed.
     * @throws IOException if an I/O error occured.
     * @throws BMPException if the header was invalid
     */
    public BMPInfoHeader(ImageInputStream in) throws IOException, BMPException {
        byte[] data = new byte[SIZE];
	    
	if (in.read(data) != SIZE)
	    throw new IOException("Couldn't read header.");
	ByteBuffer buf = ByteBuffer.wrap(data);
	buf.order(ByteOrder.LITTLE_ENDIAN);

	int n;
	if((n=buf.getInt()) != SIZE)
	    throw new BMPException("Invalid BITMAPINFOHEADER size: "+n);

	biWidth = buf.getInt();
	biHeight = buf.getInt();
	biPlanes = buf.getShort();
	setBitCount(buf.getShort());
	setCompression(buf.getInt());
	biSizeImage = buf.getInt();
	biXPelsPerMeter = buf.getInt();
	biYPelsPerMeter = buf.getInt();
	biClrUsed = buf.getInt();
	biClrImportant = buf.getInt();
    }

    public void setBitCount(short bitcount) throws BMPException {
	switch(bitcount){
	case 1:
	case 4:
	case 8:
	case 16:
	case 24:
	case 32:
	    biBitCount = bitcount;
	    break;

	default:
	    throw new BMPException("Invalid number of bits per pixel: "+
				   bitcount);
	}
    }

    public short getBitCount() { return biBitCount; }

    public void setCompression(int compression) throws BMPException {
	switch(compression){
	case BI_RLE8:
	    if(getBitCount() != 8)
		throw new BMPException("Invalid number of bits per pixel.");
	    biCompression = compression;
	    break;
	case BI_RLE4:
	    if(getBitCount() != 4)
		throw new BMPException("Invalid number of bits per pixel.");
	    biCompression = compression;
	    break;

	case BI_RGB:
	case BI_BITFIELDS:
	    biCompression = compression;
	    break;

	default:
	    throw new BMPException("Unknown bitmap compression type.");
	}
    }

    public int getNumberOfPaletteEntries(){
	if(biClrUsed == 0)
	    switch(biBitCount){
	    case 1:
		return 2;
	    case 4:
		return 16;
	    case 8:
		return 256;

	    default: // should not happen
		return 0;
	    }

	return biClrUsed;
    }

    public int getCompression(){
	return biCompression;
    }

    public Dimension getSize(){
	return new Dimension(biWidth, biHeight);
    }

    public int getWidth(){
	return biWidth;
    }

    public int getHeight(){
	return biHeight;
    }

    public void setSize(Dimension d){
	biWidth = (int)d.getWidth();
	biHeight = (int)d.getHeight();
    }

}
