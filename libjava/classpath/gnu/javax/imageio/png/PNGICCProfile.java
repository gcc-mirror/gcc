/* PNGICCProfile.java --
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

import java.awt.color.ICC_Profile;
import java.awt.color.ICC_ColorSpace;
import java.awt.color.ColorSpace;
import java.io.UnsupportedEncodingException;
import java.io.IOException;
import java.io.ByteArrayInputStream;
import java.util.zip.InflaterInputStream;
import java.util.zip.Deflater;

/**
 * A PNG iCCP (ICC Profile) chunk.
 */
public class PNGICCProfile extends PNGChunk
{
  private String name;
  private ICC_Profile profile;
  // A generic profile name to use "ICC Profile"
  private static final byte[] genericName = new byte[]
  { 0x49, 0x43, 0x43, 0x20, 0x50, 0x72, 0x6f, 0x66, 0x69, 0x6c, 0x65 };

  protected PNGICCProfile( int type, byte[] data, int crc ) throws PNGException
  {
    super( type, data, crc );
    int i = 0;
    while( data[i++] != 0 )
      ;

    try
      {
        name = new String(data, 0, i, "8859_1");
      }
    catch(UnsupportedEncodingException e)
      {
        name = ""; // shouldn't really happen.
      }
    if( data[i++] != 0 )
      throw new PNGException("Can't handle nonzero compression types with iCCP chunks.");
    try
      {
        ByteArrayInputStream bos = new ByteArrayInputStream( data, i,
                                                             data.length - i );
        profile = ICC_Profile.getInstance( new InflaterInputStream( bos ) );
      }
    catch(IOException ioe)
      {
        throw new PNGException("Couldn't read iCCP profile chunk.");
      }
    System.out.println("Got profile:"+profile);
  }

  public PNGICCProfile( ICC_Profile profile )
  {
    super( TYPE_PROFILE );
    this.profile = profile;
    byte[] profData = profile.getData();
    byte[] outData = new byte[ profData.length * 2 ];
    Deflater deflater = new Deflater();
    deflater.setInput( profData );
    deflater.finish();
    int n = deflater.deflate( outData );
    data = new byte[ n + 11 + 2 ];
    System.arraycopy(genericName, 0, data, 0, 11 );
    data[11] = data[12] = 0; // null separator and compression type.
    // Copy compressed data
    System.arraycopy(outData, 0, data, 13, n );
  }

  public ColorSpace getColorSpace()
  {
    return new ICC_ColorSpace( profile );
  }

  public String toString()
  {
    return "PNG ICC Profile, name: "+name;
  }
}
