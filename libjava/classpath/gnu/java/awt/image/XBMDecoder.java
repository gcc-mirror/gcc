/* XBMDecoder.java -- Decodes X-bitmaps
   Copyright (C) 1999, 2004  Free Software Foundation, Inc.

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


package gnu.java.awt.image;

import java.awt.image.ColorModel;
import java.awt.image.ImageConsumer;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.StringTokenizer;
import java.util.Vector;

public class XBMDecoder extends ImageDecoder
{
  BufferedReader reader;
  static final ColorModel cm = ColorModel.getRGBdefault ();
  static final int black = 0xff000000;
  static final int transparent = 0x00000000;
  static final int masktable[] = { 0x01, 0x02, 0x04, 0x08,
                                   0x10, 0x20, 0x40, 0x80 };

  public XBMDecoder (String filename)
  {
    super (filename);
  }

  public XBMDecoder (URL url)
  {
    super (url);
  }

  public void produce (Vector v, InputStream is) throws IOException
  {
    reader = new BufferedReader (new InputStreamReader (is));
    int width = -1, height = -1;

    for (int i = 0; i < 2; i++)
      {
        String line = reader.readLine ();
        StringTokenizer st = new StringTokenizer (line);

        st.nextToken ();                // #define
        st.nextToken ();                // name_[width|height]
        if (i == 0)
          width = Integer.parseInt (st.nextToken (), 10);
        else
          height = Integer.parseInt (st.nextToken (), 10);
      }

    for (int i = 0; i < v.size (); i++)
      {
        ImageConsumer ic = (ImageConsumer) v.elementAt (i);

        ic.setDimensions (width, height);
        ic.setColorModel (cm);
        ic.setHints (ImageConsumer.COMPLETESCANLINES
                     | ImageConsumer.SINGLEFRAME
                     | ImageConsumer.SINGLEPASS
                     | ImageConsumer.TOPDOWNLEFTRIGHT);
      }

    /* skip to the byte array */
    while (reader.read () != '{') { }

    /* loop through each scanline */
    for (int line = 0; line < height; line++)
      {
        int scanline[] = getScanline (reader, width);

        for (int i = 0; i < v.size (); i++)
          {
            ImageConsumer ic = (ImageConsumer) v.elementAt (i);
            ic.setPixels (0, 0 + line, width, 1, cm, scanline, 0, width);
          }
      }

    /* tell each ImageConsumer that we're finished */
    for (int i = 0; i < v.size (); i++)
      {
        ImageConsumer ic = (ImageConsumer) v.elementAt (i);
        ic.imageComplete (ImageConsumer.STATICIMAGEDONE);
      }
  }

  public static int[] getScanline (Reader in, int len) throws IOException
  {
    char byteStr[] = new char[2];
    int scanline[] = new int[len];
    int x = 0;

    while (x < len)
      {
        int ch = in.read ();
        if (ch == '0')
          {
            in.read ();         // 'x'

            byteStr[0] = (char) in.read ();
            byteStr[1] = (char) in.read ();

            int byteVal = Integer.parseInt (new String (byteStr), 16);

            for (int i = 0; i < 8; i++, x++)
              {
                if (x == len)   // condition occurs if bitmap is padded
                  return scanline;

                scanline[x] = ((byteVal & masktable[i]) != 0) ?
                               black : transparent;
              }
          }
      }

    return scanline;
  }
}
