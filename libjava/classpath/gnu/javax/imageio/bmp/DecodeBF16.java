/* DecodeBF16.java --
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
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DirectColorModel;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferUShort;
import java.awt.image.SinglePixelPackedSampleModel;
import java.awt.image.SampleModel;
import java.awt.Dimension;

public class DecodeBF16 extends BMPDecoder {
    private int[] bitmasks;
    private boolean useDefaultMasks;

    public DecodeBF16(BMPFileHeader fh, BMPInfoHeader ih, 
		      boolean udm){
	super(fh,ih);

	useDefaultMasks = udm;
	if(useDefaultMasks) // 5-6-5 mask, B,G,R
	    bitmasks = new int[] { 0x00F800, 0x0007E0, 0x00001F };
    }

    public BufferedImage decode(ImageInputStream in) throws IOException, BMPException {
	if(!useDefaultMasks)
	    bitmasks = readBitMasks(in);
	skipToImage(in);

	Dimension d = infoHeader.getSize();
	int h = (int)d.getHeight();
	int w = (int)d.getWidth();

	// BMP scanlines are padded to dword offsets
	int scansize = (w + (w&1)) << 1;
	short[] data = new short[w*h];

	for(int y=h-1;y>=0;y--){
	    byte[] scanline = new byte[scansize];
	    if(in.read(scanline) != scansize)
		throw new IOException("Couldn't read image data.");

	    for(int x=0;x<w;x++)
		data[x + y*w] = (short)((scanline[x*2] & (0xFF)) | 
				      ((scanline[x*2+1] & (0xFF)) << 8));
	}

	ColorModel cm = new DirectColorModel(16,
					     bitmasks[0], bitmasks[1], bitmasks[2]);
	SampleModel sm = new SinglePixelPackedSampleModel(DataBuffer.TYPE_USHORT, 
							  w, h, 
							  bitmasks);
	DataBuffer db = new DataBufferUShort(data, w*h, 0);
	WritableRaster raster = Raster.createWritableRaster(sm, db, null);
	return new BufferedImage(cm, raster, false, null);
    }

}



