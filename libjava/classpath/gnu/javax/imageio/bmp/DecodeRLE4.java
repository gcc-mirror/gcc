/* DecodeRLE4.java --
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

public class DecodeRLE4 extends BMPDecoder {

    public DecodeRLE4(BMPFileHeader fh, BMPInfoHeader ih){
        super(fh, ih);
    }

    /**
     * RLE control codes
     */
    private static final byte ESCAPE = (byte)0;
    private static final byte EOL = (byte)0; // end of line
    private static final byte EOB = (byte)1; // end of bitmap
    private static final byte DELTA = (byte)2; // delta

    public BufferedImage decode(ImageInputStream in) throws IOException, BMPException {
        IndexColorModel palette = readPalette(in);
        skipToImage(in);

        Dimension d = infoHeader.getSize();
        int h = (int)d.getHeight();
        int w = (int)d.getWidth();

        byte[] data = uncompress(w, h, in);
        SampleModel sm = new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE,
                                                         w, h, 4);

        DataBuffer db = new DataBufferByte(data, w*h, 0);
        WritableRaster raster = Raster.createWritableRaster(sm, db, null);

        return new BufferedImage(palette, raster, false, null);
    }

    private byte[] uncompress(int w, int h, ImageInputStream in)
        throws BMPException, IOException {
        byte[] cmd = new byte[2];
        byte[] data = new byte[w*h>>1];
        int offIn = 0;
        int x=0,y=0;

        // width in bytes
        w += (w&1);
        w = w >> 1;

        try {
            while(((x>>1) + y*w) < w*h){
                if(in.read(cmd) != 2)
                    throw new IOException("Error reading compressed data.");

                if(cmd[0] == ESCAPE){
                    switch(cmd[1]){
                    case EOB: // end of bitmap
                        return data;
                    case EOL: // end of line
                        x = 0;
                        y++;
                        break;
                    case DELTA: // delta
                        if(in.read(cmd) != 2)
                            throw new IOException("Error reading compressed data.");
                        int dx = cmd[0] & (0xFF);
                        int dy = cmd[1] & (0xFF);
                        x += dx;
                        y += dy;
                        break;

                    default:
                        // decode a literal run
                        int length = cmd[1] & (0xFF);

                        // size of run, which is word aligned.
                        int bytesize = length;
                        bytesize += (bytesize & 1);
                        bytesize >>= 1;
                        bytesize += (bytesize & 1);

                        byte[] run = new byte[bytesize];
                        if(in.read(run) != bytesize)
                            throw new IOException("Error reading compressed data.");

                        if((x&1) == 0){
                            length += (length&1);
                            length >>= 1;
                            System.arraycopy(run, 0, data, ((x>>1) + w*(h-y-1)),
                                             length);
                        } else {
                            for(int i=0;i<length;i++){
                                if((i&1) == 0) // copy high to low
                                    data[((x+i)>>1) + w*(h-y-1)]
                                        |= ((run[i>>1]&0xF0) >> 4);
                                else  // copy low to high
                                    data[((x+i)>>1) + w*(h-y-1)]
                                        |= ((run[i>>1]&0x0F) << 4);
                            }
                        }
                        x += cmd[1] & (0xFF);
                        break;
                    }
                } else {
                    // decode a byte run
                    int length = cmd[0] & (0xFF);
                    if((x&1) == 0){
                        length += (length&1);
                        length >>= 1;
                        for(int i=0;i<length;i++)
                            data[(h-y-1)*w + i + (x >> 1)] = cmd[1];
                    } else {
                        for(int i=0;i<length;i++){
                            if((i&1) == 0) // copy high to low
                                data[((x+i)>>1) + w*(h-y-1)]
                                    |= ((cmd[1]&0xF0) >> 4);
                            else  // copy low to high
                                data[((x+i)>>1) + w*(h-y-1)]
                                    |= ((cmd[1]&0x0F) << 4);
                        }
                    }
                    x += cmd[0] & (0xFF);
                }
            }
            return data;
        } catch(ArrayIndexOutOfBoundsException e){
            throw new BMPException("Invalid RLE data.");
        }
    }
}
