/* BMPDecoder.java --
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
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.awt.image.ColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.BufferedImage;

public abstract class BMPDecoder {

    protected BMPInfoHeader infoHeader;
    protected BMPFileHeader fileHeader;
    protected long offset;
    
    public BMPDecoder(BMPFileHeader fh, BMPInfoHeader ih){
	fileHeader = fh;
	infoHeader = ih;
	offset = BMPFileHeader.SIZE + BMPInfoHeader.SIZE;
    }

    /**
     * Determines the coding type of the bitmap and returns the corresponding
     * decoder.
     */
    public static BMPDecoder getDecoder(BMPFileHeader fh, BMPInfoHeader ih){
	switch(ih.getCompression()){
	case BMPInfoHeader.BI_RGB: // uncompressed RGB
	    switch(ih.getBitCount()){
	    case 32:
		return new DecodeBF32(fh, ih, true);

	    case 24:
		return new DecodeRGB24(fh, ih);

	    case 16:
		return new DecodeBF16(fh, ih, true);

	    case 8:
		return new DecodeRGB8(fh, ih);

	    case 4:
		return new DecodeRGB4(fh, ih);

	    case 1:
		return new DecodeRGB1(fh, ih);

	    default:
		return null;
	    }

	case BMPInfoHeader.BI_RLE8:
	    return new DecodeRLE8(fh, ih);

	case BMPInfoHeader.BI_RLE4:
	    return new DecodeRLE4(fh, ih);

	case BMPInfoHeader.BI_BITFIELDS:
	    switch(ih.getBitCount()){
	    case 16:
		return new DecodeBF16(fh, ih, false);

	    case 32:
		return new DecodeBF32(fh, ih, false);

	    default:
		return null;
	    }
		
	default:
	    return null;
	}
    }

    /**
     * The image decoder.
     */
    public abstract BufferedImage decode(ImageInputStream in) 
	throws IOException, BMPException;

    /**
     * Reads r,g,b bit masks from an inputstream
     */
    protected int[] readBitMasks(ImageInputStream in) throws IOException {
	int[] bitmasks = new int[3];
	byte[] temp = new byte[12];
	if(in.read(temp) != 12)
	    throw new IOException("Couldn't read bit masks.");
	offset += 12;

	ByteBuffer buf = ByteBuffer.wrap(temp);
	buf.order(ByteOrder.LITTLE_ENDIAN);
	bitmasks[0] = buf.getInt();
	bitmasks[1] = buf.getInt();
	bitmasks[2] = buf.getInt();
	return bitmasks;
    }

    /**
     * Reads an N-color palette from an inputstream in RGBQUAD format and 
     * returns an equivalent ColorModel object
     */
    protected IndexColorModel readPalette(ImageInputStream in) throws IOException {
	int N = infoHeader.getNumberOfPaletteEntries();
	byte[] r = new byte[N];
	byte[] g = new byte[N];
	byte[] b = new byte[N];
	for(int i=0;i<N;i++){
	    byte[] RGBquad = new byte[4];
	    if(in.read(RGBquad) != 4)
		throw new IOException("Error reading palette information.");
	    // RGBQUAD structure is b,g,r,0
	    r[i] = RGBquad[2];
	    g[i] = RGBquad[1];
	    b[i] = RGBquad[0];
	}
	
	offset += 4*N;
	return new IndexColorModel(8, N, r, g, b);
    }

    /**
     * Read bytes to the start of the image data
     */
    protected void skipToImage(ImageInputStream in) throws IOException {
	byte[] d = new byte[1];
	long n = fileHeader.getOffset() - offset;
	for(int i=0;i<n;i++)
	    in.read(d);
    }
}
