/* PNGFilter.java -- PNG image filters.
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
 * A utility class of static methods implementing the PNG filtering algorithms.
 */
public class PNGFilter
{

  public static final byte FILTER_NONE = 0;
  public static final byte FILTER_SUB = 1;
  public static final byte FILTER_UP = 2;
  public static final byte FILTER_AVERAGE = 3;
  public static final byte FILTER_PAETH = 4;

  /**
   * Return whether a filter should be used or FILTER_NONE,
   * following the recommendations in the PNG spec.
   */
  public static boolean useFilter( PNGHeader header )
  {
    switch( header.getColorType() )
      {
      case PNGHeader.INDEXED:
	return false;
	
      case PNGHeader.GRAYSCALE:
      case PNGHeader.RGB:
	if( header.bytesPerPixel() <= 1 )
	  return false;
      case PNGHeader.GRAYSCALE_WITH_ALPHA:
      case PNGHeader.RGB_WITH_ALPHA:
      default:
	return true;
      }
  }

  /**
   * Heuristic for adaptively choosing a filter, following the scheme
   * suggested in the PNG spec.
   * @return a fiter type.
   */
  public static byte chooseFilter( byte[] scanline, byte[] lastScanline, 
				  int bpp)
    
  {
    long[] values = new long[5];
    int idx = 0;
    for( int i = 0; i < 5; i++ )
      {
	byte[] filtered = filterScanline((byte)i, scanline, lastScanline, bpp);
	values[i] = 0;
	for(int j = 0; j < filtered.length; j++ )
	  values[i] += (int)(filtered[j] & 0xFF);
	if( values[ idx ] > values[i] )
	  idx = i;
      }
    return (byte)idx;
  }

  /**
   * Filter a scanline.
   */
  public static byte[] filterScanline( byte filtertype, byte[] scanline, 
				       byte[] lastScanline, int bpp)
  {
    int stride = scanline.length;
    byte[] out = new byte[ stride ];
    switch( filtertype )
      {
      case FILTER_SUB:
	for( int i = 0; i < bpp; i++)
	  out[ i ] = scanline[ i ];
	
	for( int i = bpp; i < stride; i++ )
	  out[i] = (byte)(scanline[ i ] - 
			  scanline[ i - bpp ]);
	break;

      case FILTER_UP:
	for( int i = 0; i < stride; i++ )
	  out[ i ] = (byte)(scanline[ i ] - lastScanline[ i ]);
	break;

      case FILTER_AVERAGE:
	for( int i = 0; i < bpp; i++)
	  out[ i ] = (byte)((scanline[ i ] & 0xFF) - ((lastScanline[ i ] & 0xFF) >> 1));
	for( int i = bpp; i < stride; i++ )
	  out[ i ] = (byte)((scanline[ i ] & 0xFF) - 
			    (((scanline[ i - bpp ] & 0xFF) + 
			      (lastScanline[ i ] & 0xFF)) >> 1));
	break;

      case FILTER_PAETH:
	for( int i = 0; i < stride; i++ )
	  {
	    int x;
	    {
	      int a, b, c;
	      if( i >= bpp )
		{
		  a = (scanline[ i - bpp ] & 0xFF); // left
		  c = (lastScanline[ i - bpp ] & 0xFF); // upper-left
		}
	      else
		a = c = 0;
	      b = (lastScanline[ i ] & 0xFF); // up
	      
	      int p = (a + b - c);        // initial estimate
	      // distances to a, b, c
	      int pa = (p > a) ? p - a : a - p; 
	      int pb = (p > b) ? p - b : b - p; 
	      int pc = (p > c) ? p - c : c - p; 
	      // return nearest of a,b,c,
	      // breaking ties in order a,b,c.
	      if( pa <= pb && pa <= pc ) x = a;
	      else { if( pb <= pc ) x = b;
		else x = c;
	      }
	    }
	    out[ i ] = (byte)(scanline[ i ] - x);
	  }
	break;
      default:
      case FILTER_NONE:
	return scanline;
      }
    return out;
  }

  /**
   * Unfilter a scanline.
   */
  public static byte[] unFilterScanline( int filtertype, byte[] scanline, 
					 byte[] lastScanline, int bpp)
  {
    int stride = scanline.length;
    byte[] out = new byte[ stride ];
    switch( filtertype )
      {

      case FILTER_NONE:
	System.arraycopy( scanline, 0, out, 0, stride );
	break;
	
      case FILTER_SUB:
	for( int i = 0; i < bpp; i++)
	  out[ i ] = scanline[ i ];
	
	for( int i = bpp; i < stride; i++ )
	  out[ i ] = (byte)(scanline[ i ] + 
			    out[ i - bpp ]);
	break;

      case FILTER_UP:
	for( int i = 0; i < stride; i++ )
	  out[ i ] = (byte)(scanline[ i ] + lastScanline[ i ]);
	break;

      case FILTER_AVERAGE:
	for( int i = 0; i < bpp; i++)
	  out[ i ] = (byte)((scanline[ i ] & 0xFF) + ((lastScanline[ i ] & 0xFF) >> 1));
	for( int i = bpp; i < stride; i++ )
	  out[ i ] = (byte)((scanline[ i ] & 0xFF) + 
			    (((out[ i - bpp ] & 0xFF) + (lastScanline[ i ] & 0xFF)) >> 1));
	break;

      case FILTER_PAETH:
	for( int i = 0; i < stride; i++ )
	  {
	    int x;
	    {
	      int a, b, c;
	      if( i >= bpp )
		{
		  a = (out[ i - bpp ] & 0xFF); // left
		  c = (lastScanline[ i - bpp ] & 0xFF); // upper-left
		}
	      else
		a = c = 0;
	      b = (lastScanline[ i ] & 0xFF); // up
	      
	      int p = (a + b - c);        // initial estimate
	      // distances to a, b, c
	      int pa = (p > a) ? p - a : a - p; 
	      int pb = (p > b) ? p - b : b - p; 
	      int pc = (p > c) ? p - c : c - p; 
	      // return nearest of a,b,c,
	      // breaking ties in order a,b,c.
	      if( pa <= pb && pa <= pc ) x = a;
	      else { if( pb <= pc ) x = b;
		else x = c;
	      }
	    }
	    out[ i ] = (byte)(scanline[ i ] + x);
	  }
	break;
      }
    return out;
  }
}