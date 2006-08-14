/* PNGPhys.java --
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

/**
 * A PNG "pHYS" chunk - pixel physical dimensions
 */
public class PNGPhys extends PNGChunk 
{ 
  long x, y;
  double ratio;
  boolean usesRatio;

  protected PNGPhys( int type, byte[] data, int crc ) throws PNGException
  {
    super( type, data, crc );
    if( data.length < 9 )
      throw new PNGException("Unexpected size of pHYS chunk.");
    x = ((data[0] & 0xFF) << 24) | ( (data[1] & 0xFF) << 16 ) | 
      ((data[2] & 0xFF) << 8) | (data[3] & 0xFF);
    y = ((data[4] & 0xFF) << 24) | ( (data[5] & 0xFF) << 16 ) | 
      ((data[6] & 0xFF) << 8) | (data[7] & 0xFF);
    if(data[8] == 0)
      {
	ratio = ((double)x)/((double)y);
	usesRatio = true;
      }
  }

  public PNGPhys( double ratio )
  {
    super( TYPE_PHYS );

    this.ratio = ratio;
    usesRatio = true;

    if( ratio < 1.0 )
      {
	y = 0xFFFFFFFF;
	x = (long)(0xFFFFFFFFL * ratio);
      }
    else
      {
	x = 0xFFFFFFFF;
	y = (long)(0xFFFFFFFFL * ratio);
      }
    makeData();
  }

  public PNGPhys( int x, int y )
  {
    super( TYPE_PHYS );
    usesRatio = false;
    this.x = x;
    this.y = y;
    makeData();
  }

  private void makeData()
  {
    data = new byte[ 9 ];
    byte[] a = getInt( (int)x );
    byte[] b = getInt( (int)y );
    data[0] = a[0]; data[1] = a[1]; data[2] = a[2]; data[3] = a[3]; 
    data[4] = b[0]; data[5] = b[1]; data[6] = b[2]; data[7] = b[3]; 
    data[7] = (usesRatio) ? 0 : (byte)0xFF;
  }

  public String toString()
  {
    String s = "PNG Physical pixel size chunk.";
    if( usesRatio )
      return s + " Aspect ratio (x/y): " + ratio;
    else
      return s + " " + x + " by " + y + " pixels per meter. (x, y).";
  }
}
