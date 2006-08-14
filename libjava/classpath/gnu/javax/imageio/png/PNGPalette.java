/* PNGPalette.java --
   Copyright (C) 2006 Free Software Foundation

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

import java.awt.color.ColorSpace;
import java.awt.image.IndexColorModel;

/**
 * A PNG Palette chunk.
 */
public class PNGPalette extends PNGChunk 
{ 
  private int[] red,green,blue;

  protected PNGPalette( int type, byte[] data, int crc ) throws PNGException
  {
    super( type, data, crc );
    double l = data.length;
    l /= 3.0;
    // Check if it's divisible by 3. (Yuck.)
    if( l - Math.floor(l) != 0.0 )
      throw new PNGException("Invalid size of palette chunk.");
    int nEntries = (int)l;

    red = new int[ nEntries ];
    green = new int[ nEntries ];
    blue = new int[ nEntries ];
    for( int i = 0; i < nEntries; i++ )
      {
	red[i] = (data[ i * 3 ] & 0xFF);
	green[i] = (data[ i * 3 + 1 ] & 0xFF);
	blue[i] = (data[ i * 3 + 2] & 0xFF);
      }
  }

  public PNGPalette( IndexColorModel cm )
  {
    super( TYPE_PALETTE );
    int n = cm.getMapSize();
    data = new byte[ n * 3 ];
    red = new int[ n ];
    green = new int[ n ];
    blue = new int[ n ];
    for(int i = 0; i < n; i++ )
      {
	red[i] = data[i * 3] = (byte)cm.getRed(i);
	green[i] = data[i * 3 + 1] = (byte)cm.getGreen(i);
	blue[i] = data[i * 3 + 2] = (byte)cm.getBlue(i);
      }
  }

  public IndexColorModel getPalette( ColorSpace cs )
  {
    int nc = red.length;
    byte[] r = new byte[nc];
    byte[] g = new byte[nc];
    byte[] b = new byte[nc];

    if( cs == null )
      {
	for(int i = 0; i < nc; i ++ )
	  {
	    r[i] = (byte)red[i];
	    g[i] = (byte)green[i];
	    b[i] = (byte)blue[i];
	  }
      }
    else
      {
	for(int i = 0; i < nc; i ++ )
	  {
	    float[] in = new float[3];
	    in[0] = (((float)red[i]) / 255f);
	    in[1] = (((float)green[i]) / 255f);
	    in[2] = (((float)blue[i]) / 255f);
	    float[] out = cs.toRGB( in );
	    r[i] = (byte)( Math.round(out[0] * 255.0) );
	    g[i] = (byte)( Math.round(out[1] * 255.0) );
	    b[i] = (byte)( Math.round(out[2] * 255.0) );
	  }
      }	
    return new IndexColorModel(8, nc, r, g, b);
  }

  public String toString()
  {
    String s = "PNG Palette:\n";
    for( int i = 0; i < red.length; i++)
      s = s + "Index " + i + ": ["+ red[i] +", "+green[i]+", "+blue[i]+"]\n";
    return s;
  }
}
