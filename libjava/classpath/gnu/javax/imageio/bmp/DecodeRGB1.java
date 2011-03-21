/* DecodeRGB1.java --
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
import java.awt.image.IndexColorModel;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.SampleModel;
import java.awt.Dimension;

public class DecodeRGB1 extends BMPDecoder {

    public DecodeRGB1(BMPFileHeader fh, BMPInfoHeader ih){
        super(fh, ih);
    }

    public BufferedImage decode(ImageInputStream in)
        throws IOException, BMPException {

        IndexColorModel palette = readPalette(in);
        skipToImage(in);

        Dimension d = infoHeader.getSize();
        int h = (int)d.getHeight();
        int w = (int)d.getWidth();
        int size = (w*h)>>3;

        int scansize = w>>3;
        byte[] data = new byte[size];

        for(int y=h-1;y>=0;y--){
            // Scanlines are padded to dword boundries
            int readsize = scansize;
            if((readsize & 3) != 0) readsize += (4 - (scansize & 3));

            byte[] scanline = new byte[readsize];
            if(in.read(scanline) != readsize)
                throw new IOException("Couldn't read image data.");

            for(int x=0;x<scansize;x++)
                data[x + y*scansize] = scanline[x];
        }

        SampleModel sm = new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE,
                                                         w, h, 1);

        DataBuffer db = new DataBufferByte(data, size, 0);
        WritableRaster raster = Raster.createWritableRaster(sm, db, null);

        return new BufferedImage(palette, raster, false, null);
    }

}
