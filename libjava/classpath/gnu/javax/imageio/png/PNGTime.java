/* PNGTime.java --
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

import java.util.Date;

/**
 * A PNG tIME chunk.
 */
public class PNGTime extends PNGChunk
{
  private Date date;

  protected PNGTime( int type, byte[] data, int crc ) throws PNGException
  {
    super( type, data, crc );
    if( data.length < 7 )
      throw new PNGException("Unexpectedly short time chunk. ("+data.length+" bytes)");

    // PNG value is absolute (2006, not 106 or 06), java is from 1900.
    int year = ( (data[0] & 0xFF) << 8 ) | (data[1] & 0xFF);
    int month = (data[2] & 0xFF); // java counts from 0. PNG from 1.
    int day = (data[3] & 0xFF);
    int hour = (data[4] & 0xFF);
    int minute = (data[5] & 0xFF);
    int second = (data[6] & 0xFF);
    date = new Date( year - 1900, month - 1, day, hour, minute, second );
  }

  public PNGTime( Date d )
  {
    super( TYPE_TIME );
    data = new byte[ 7 ];
    int tmp = d.getYear() + 1900;
    data[0] = (byte)((tmp & 0xFF00) >> 8);
    data[1] = (byte)(tmp & 0x00FF);
    data[2] = (byte)(d.getMonth() + 1);
    data[3] = (byte)(d.getDay());
    data[4] = (byte)(d.getHours());
    data[5] = (byte)(d.getMinutes());
    data[6] = (byte)(d.getSeconds());
  }

  public String toString()
  {
    return "PNG Time chunk: "+date;
  }
}
