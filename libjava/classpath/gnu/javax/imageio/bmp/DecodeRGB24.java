/* DecodeRGB24.java --
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
import java.awt.image.BufferedImage;
import java.awt.Dimension;

public class DecodeRGB24 extends BMPDecoder {

    public DecodeRGB24(BMPFileHeader fh, BMPInfoHeader ih){
	super(fh, ih);
    }

    public BufferedImage decode(ImageInputStream in) throws IOException, BMPException {
	skipToImage(in);
        
	Dimension d = infoHeader.getSize();
	int h = (int)d.getHeight();
	int w = (int)d.getWidth();
	BufferedImage image = new BufferedImage(w, h, 
						BufferedImage.TYPE_INT_RGB);
	// BMP scanlines are padded to dword offsets
	int scansize = ((w*3 & 3) != 0)? w*3 + 4 - (w*3 & 3): w*3;
	int[] data = new int[w*h];

	for(int y=h-1;y>=0;y--){
	    byte[] scanline = new byte[scansize];
	    if(in.read(scanline) != scansize)
		throw new IOException("Couldn't read image data.");

	    for(int x=0;x<w;x++)
		data[x + y*w] = scanline[x*3] + 
		    (scanline[x*3+1] << 8) +
		    (scanline[x*3+2] << 16);
	}
	image.setRGB(0, 0, w, h, data, 0, w);
	return image;
    }

}
