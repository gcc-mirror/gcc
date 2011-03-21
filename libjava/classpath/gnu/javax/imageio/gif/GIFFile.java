/* GIFFile.java -- GIF decoder
   Copyright (C) 2006  Free Software Foundation, Inc.

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

package gnu.javax.imageio.gif;

import java.io.IOException;
import java.io.InputStream;
import java.util.Vector;

/**
 * GIFFile - reads a GIF file.
 *
 * This class only does the bare minimum work, and returns the data in raw
 * formats (described below). The class is J2ME compatible, and hopefully
 * we can keep it that way without any significant overhead.
 *
 * @author Sven de Marothy.
 */
public class GIFFile
{
  // "NETSCAPE2.0" - identifier
  private final static byte[] nsBlock = new byte[]
  {0x4e, 0x45, 0x54, 0x53, 0x43, 0x41, 0x50, 0x45, 0x32, 0x2e, 0x30 };

  /**
   * Block identifiers
   */
  private final static int EXTENSION = 0x21;
  private final static int LOCAL = 0x2C;
  private final static int TERMINATOR = 0x3B;

  /**
   * Extension block types
   */
  private final static int EXTENSION_COMMENT = 254;
  private final static int EXTENSION_GCONTROL = 249;
  private final static int EXTENSION_APPLICATION = 255;

  /**
   * Undraw commands for animation.
   */
  private final static int UNDRAW_OVERWRITE = 1;
  private final static int UNDRAW_RESTORE_BACKGROUND = 2;
  private final static int UNDRAW_RESTORE_PREVIOUS = 3;

  /**
   * Image position and dimensions (images may be partial)
   */
  private int x, y, width, height;

  /**
   * Global dimensions
   */
  private int globalWidth, globalHeight;

  /**
   * Background color index.
   */
  private byte bgIndex;

  /**
   * Number of colors
   */
  private int nColors;

  /**
   * Global palette, if any
   */
  private byte[] globalPalette;

  /**
   * Any
   */
  private boolean hasGlobalColorMap;

  /**
   * Local palette, if any (used if available)
   */
  private byte[] localPalette;

  /**
   * Interlaced GIF or not?
   */
  private boolean interlaced;

  /**
   * Has transparency?
   */
  private boolean hasTransparency;

  /**
   * Undraw mode (animations)
   */
  private int undraw;

  /**
   * Transparent index;
   */
  private int transparentIndex;

  /**
   * The uncompressed raster
   */
  private byte[] raster;

  /**
   * The compressed data (freed after uncompressing)
   */
  private byte[] compressedData;

  /**
   * Frame delay in 100ths of a second ( centiseconds, metrically )
   */
  private int duration;

  /**
   * Indices used during decompression
   */
  private int dataBlockIndex;

  /**
   * The file comment , if a comment block exists.
   */
  private String comment;

  /**
   * Fields used by getBits()
   */
  private int remainingBits = 0;
  private int currentBits = 0;

  /**
   * Netscape animation extension
   */
  private boolean isLooped = false;

  /** Number of loops, 0 = infinite */
  private int loops;

  /**
   * Additional frames if it's an animated GIF.
   */
  private Vector animationFrames;

  /**
   * Loads the file from an input stream, which is not closed.
   * @throws IOException if an I/O error occured.
   * @throws GIFException if some file parsing error occured
   */
  public GIFFile(InputStream in) throws IOException, GIFException
  {
    // Validate the signature
    if( !readSignature( in ) )
      throw new GIFException("Invalid GIF signature.");

    {
      byte[] data = new byte[7];
      if (in.read(data) != 7)
        throw new IOException("Couldn't read global descriptor.");

      globalWidth = ((data[1] & 0xFF) << 8) | (data[0] & 0xFF);
      globalHeight = ((data[3] & 0xFF) << 8) | (data[2] & 0xFF);
      byte flags = data[4];
      bgIndex = data[5];
      nColors = (1 << (( flags & 0x07) + 1));
      hasGlobalColorMap = ((flags & 0x80) != 0);
    }

    if( hasGlobalColorMap )
      {
        globalPalette = new byte[ nColors * 3 ];
        if( in.read( globalPalette ) != nColors * 3 )
          throw new IOException("Couldn't read color map.");
      }

    int c = in.read();
    while( c == EXTENSION )
      {
        readExtension( in );
        c = in.read();
      }

    if( c != LOCAL )
      throw new GIFException("Extension blocks not followed by a local descriptor ("+c+")");

    loadImage( in );
    c = in.read();

    if( c == TERMINATOR ) // Not an animated GIF.
      return;

    // Load animation frames. Just quit if an error occurs instead
    // of throwing an exception.
    animationFrames = new Vector();
    try
      {
        while( c != TERMINATOR )
          {
            animationFrames.add( new GIFFile( this, in, c ) );
            c = in.read();
          }
      }
    catch(IOException ioe)
      {
      }
    catch(GIFException gife)
      {
      }
  }

  /**
   * Constructor for additional animation frames.
   */
  private GIFFile(GIFFile parent, InputStream in, int c)
    throws IOException, GIFException
  {
    // Copy global properties.
    globalWidth = parent.globalWidth;
    globalHeight = parent.globalHeight;
    nColors = parent.nColors;
    globalPalette = parent.globalPalette;
    hasGlobalColorMap = parent.hasGlobalColorMap;
    interlaced = parent.interlaced;
    comment = parent.comment;
    isLooped = parent.isLooped;
    loops = parent.loops;

    while( c == EXTENSION )
    {
      readExtension( in );
      c = in.read();
    }

    if( c != LOCAL )
      throw new GIFException("Extension blocks not followed by a local descriptor ("+c+")");

    loadImage( in );
  }

  /**
   * Reads a GIF file signature from an inputstream and checks it.
   *
   * @param in - the stream (reads 6 bytes, does not close or reset).
   * @return true if the signature is a valid GIF signature.
   * @throws IOException if the signature could not be read.
   */
  public static boolean readSignature( InputStream in ) throws IOException
  {
    byte[] data = new byte[6];
    if (in.read(data) != 6)
      throw new IOException("Couldn't read signature.");

    if( data[0] != 0x47 || data[1] != 0x49 || data[2] != 0x46 ||
        data[3] != 0x38 ) // GIF8
      return false;

    if( (data[4] != 0x39 && data[4] != 0x37) || // 7 | 9
        (data[5] != 0x61 && data[5] != 0x62) ) // 'a' or 'b'
      return false;
    return true;
  }


  /**
   * Loads the image local descriptor and then loads/decodes the image raster,
   * and then performs any necessary postprocessing like deinterlacing.
   */
  private void loadImage(InputStream in)
    throws IOException, GIFException
  {
    readLocal( in );

    try
      {
        decodeRaster( in );
      }
    catch(ArrayIndexOutOfBoundsException aioobe)
      {
        throw new GIFException("Error decompressing image.");
      }

    if( interlaced )  // Clean up
      deinterlace();
    packPixels();
  }

  /**
   * Pack the pixels if it's a 2, 4 or 16 color image.
   * While GIF may support any number of colors from 2-256, we won't bother
   * trying to pack pixels not resulting in even byte boundaries.
   * (AWT doesn't support that anyway, and most apps do the same.)
   */
  private void packPixels()
  {
    if( nColors != 2 && nColors != 4 && nColors != 16 )
      return;

    int nbits = 1;
    int ppbyte = 8;
    if( nColors == 4 )
      {
        nbits = 2;
        ppbyte = 4;
      }
    else if( nColors == 16 )
      {
        nbits = 4;
        ppbyte = 2;
      }

    int rem = (width & (ppbyte - 1));
    int w = ( rem == 0 ) ? (width / ppbyte) :
      ((width + ppbyte - rem) / ppbyte);
    byte[] nr = new byte[ w * height ];
    for(int j = 0; j < height; j++)
      {
        for(int i = 0; i < width - ppbyte; i += ppbyte)
          for(int k = 0; k < ppbyte; k++)
            nr[ j * w + (i / ppbyte) ] |= (byte)((raster[ width * j + i + k ]
                                                  << (8 - nbits * (1 + k))));
        for(int i = 0; i < rem; i++)
          nr[ j * w + w - 1 ] |= (byte)((raster[ width * j + width - rem + i ]
                                         << (nbits * (rem - i))));
      }
    raster = nr;
  }

  /**
   * Returns the (global) width
   */
  public int getWidth()
  {
    return width;
  }

  /**
   * Returns the image height
   */
  public int getHeight()
  {
    return height;
  }

  /**
   * Returns the # of colors.
   */
  public int getNColors()
  {
    return nColors;
  }

  /**
   * Returns whether the GIF has transparency.
   */
  public boolean hasTransparency()
  {
    return hasTransparency;
  }

  /**
   * Returns the index of the transparent color.
   */
  public int getTransparentIndex()
  {
    return transparentIndex;
  }

  /**
   * Retuns the GIF file comment, or null if none exists.
   */
  public String getComment()
  {
    return comment;
  }

  /**
   * Get duration of the frame for animations.
   */
  public int getDuration()
  {
    return duration;
  }

  /**
   * Deinterlaces the image.
   */
  private void deinterlace()
  {
    byte[] nr = new byte[ width * height ];
    int n = 0;
    for(int i = 0; i < ((height + 7) >> 3); i++)
      {
        System.arraycopy( raster, n, nr, width * i * 8, width );
        n += width;
      }
    for(int i = 0; i < ((height + 3) >> 3); i++)
      {
        System.arraycopy( raster, n, nr, width * ( 8 * i + 4 ), width );
        n += width;
      }
    for(int i = 0; i < (height >> 2); i++)
      {
        System.arraycopy( raster, n, nr, width * (4 * i + 2), width );
        n += width;
      }
    for(int i = 0; i < (height >> 1); i++)
      {
        System.arraycopy( raster, n, nr, width * (2 * i + 1), width );
        n += width;
      }
    raster = nr;
  }

  /**
   * Reads the local descriptor
   */
  private void readLocal(InputStream in) throws IOException
  {
    byte[] data = new byte[9];
    if (in.read(data) != 9)
      throw new IOException("Couldn't read local descriptor.");
    x = ((data[1] & 0xFF) << 8) | (data[0] & 0xFF);
    y = ((data[3] & 0xFF) << 8) | (data[2] & 0xFF);
    width = ((data[5] & 0xFF) << 8) | (data[4] & 0xFF);
    height = ((data[7] & 0xFF) << 8) | (data[6] & 0xFF);
    byte flags = data[8];
    interlaced = (( flags & 0x40 ) != 0);
    if( (flags & 0x80) != 0 )
      { // has a local color map
        int nLocalColors = (1 << (( flags & 0x07) + 1));
        if( !hasGlobalColorMap )
          nColors = nLocalColors;
        localPalette = new byte[ nLocalColors * 3 ];
        if( in.read( localPalette ) != nLocalColors * 3 )
          throw new IOException("Couldn't read color map.");
      }
  }

  /**
   * Returns the image's palette in raw format
   * (r0,g0,b0,r1,g1,b2..r(Ncolors-1),g(Ncolors-1),b(Ncolors-1))
   */
  public byte[] getRawPalette()
  {
    return hasGlobalColorMap ? globalPalette : localPalette;
  }

  /**
   * Returns the image file for animated gifs.
   */
  public GIFFile getImage( int index )
  {
    if( index == 0 )
      return this;
    if( animationFrames == null )
      throw new ArrayIndexOutOfBoundsException("Only one image in file");
    return (GIFFile)animationFrames.elementAt( index - 1 );
  }

  /**
   * Return the image's raw image data.
   * If the color depth is 1,2 or 4 bits per pixel the pixels are packed
   * and the scanlines padded up to the nearest byte if needed.
   */
  public byte[] getRawImage()
  {
    return raster;
  }

  /**
   * Return the number of images in the GIF file
   */
  public int nImages()
  {
    if( animationFrames != null )
      return 1 + animationFrames.size();
    return 1;
  }

  /**
   * Handles extension blocks.
   */
  private void readExtension(InputStream in) throws IOException, GIFException
  {
    int functionCode = in.read();
    byte[] data = readData(in);
    switch( functionCode )
      {
      case EXTENSION_COMMENT: // comment block
        comment = new String(data, "8859_1");
        break;

      case EXTENSION_GCONTROL: // Graphics control extension
        undraw = (data[0] & 0x1C) >> 2;
        // allegedly there can be bad values of this.
        if( undraw < 1 && undraw > 3 ) undraw = 1;
        hasTransparency = ((data[0] & 0x01) == 1);
        transparentIndex = (data[3] & 0xFF);
        duration = ((data[2] & 0xFF) << 8) | (data[1] & 0xFF);
        break;

        // Application extension. We only parse the Netscape animation
        // extension here. Which is the only one most use anyway.
      case EXTENSION_APPLICATION:
        boolean isNS = true;
        for(int i = 0; i < nsBlock.length; i++ )
          if( nsBlock[i] != data[i] )
            isNS = false;
        if( isNS )
          {
            isLooped = true;
            loops = ((data[12] & 0xFF) << 8) | (data[13] & 0xFF);
          }
        break;

      default:
        break;
      }
  }

  /**
   * Reads a series of data blocks and merges them into a single one.
   */
  private byte[] readData(InputStream in) throws IOException
  {
    Vector v = new Vector();
    int totalBytes = 0;

    int n = in.read();
    do
      {
        totalBytes += n;
        byte[] block = new byte[ n ];
        in.read(block);
        v.add(block);
        n = in.read();
      }
    while( n > 0 );

    n = 0;
    byte[] bigBuffer = new byte[ totalBytes ];
    for( int i = 0; i < v.size(); i++ )
      {
        byte[] block = (byte[])v.elementAt(i);
        System.arraycopy(block, 0, bigBuffer, n, block.length);
        n += block.length;
      }
    return bigBuffer;
  }

  /**
   * Loads a compressed image block and decompresses it.
   */
  private void decodeRaster(InputStream in) throws IOException
  {
    int initialCodeSize = in.read();
    compressedData = readData( in );
    dataBlockIndex = 0;

    int rasterIndex = 0; // Index into the raster
    int clearCode = (1 << initialCodeSize); // 256 usually
    int endCode = clearCode + 1; // The stop code.

    raster = new byte[ width * height ];

    int codeSize = initialCodeSize + 1;
    int code = getBits( codeSize ); // = clear
    int nextCode = endCode + 1;

    /*
     * Initialize LZW dictionary
     *
     * First index - code #
     * Second index:
     * 0 = color index
     * 1 = parent (-1 - no parent)
     * 2 = first value
     * 3 - depth
     * The latter two aren't strictly necessary but make things faster, since
     * copying the values forward is faster than going back and looking.
     */
    short[][] dictionary = new short[ 4096 ][ 4 ];

    for(short i = 0; i < nColors; i ++ )
      {
        dictionary[i][0] = i;  // color index
        dictionary[i][1] = -1; // parent
        dictionary[i][2] = i;  // first
        dictionary[i][3] = 1;  // depth
      }

    code = getBits( codeSize ); // get second code
    raster[ rasterIndex++ ] = (byte)dictionary[code][0];
    int old = code;
    code = getBits( codeSize ); // start at the third code
    int c;

    do
      {
        if( code == clearCode )
          {
            codeSize = initialCodeSize + 1;
            nextCode = endCode + 1;
            // get and output second code
            code = getBits( codeSize );
            raster[ rasterIndex++ ] = (byte)dictionary[code][0];
            old = code;
          }
        else
          {
            dictionary[nextCode][1] = (short)old; // parent = old
            dictionary[nextCode][2] = dictionary[old][2]; // first pixel
            dictionary[nextCode][3] = (short)(dictionary[old][3] + 1); // depth

            // appended pixel  = first pixel of c
            if( code < nextCode )
              {
                dictionary[nextCode][0] = dictionary[code][2];
                old = code;
              }
            else // first of old
              {
                dictionary[nextCode][0] = dictionary[old][2];
                old = nextCode;
              }

            c = old;
            // output the code c
            int depth = dictionary[c][3];
            for( int i = depth - 1; i >= 0; i-- )
              {
                raster[ rasterIndex + i ] = (byte)dictionary[c][0];
                c = dictionary[c][1]; // go to parent.
              }
            rasterIndex += depth;
            nextCode ++;

            if( codeSize < 12 && nextCode >= (1 << codeSize) )
              codeSize++;
          }
        code = getBits( codeSize );
      }
    while( code != endCode && dataBlockIndex < compressedData.length );

    compressedData = null; // throw away compressed data.
  }

  /**
   * Returns nbits number of bits (in the LSBs) from compressedData
   */
  private int getBits( int nbits )
  {
    while( nbits > remainingBits )
      {
        int c = (compressedData[ dataBlockIndex++ ] & 0xFF) << remainingBits;
        currentBits |= c;
        remainingBits += 8;
      }
    int rval = (currentBits & ((1 << nbits) - 1));
    currentBits = (currentBits >> nbits);
    remainingBits -= nbits;
    return rval;
  }

  /**
   * Generic exception used by GIFFile to report decoding errors.
   */
  public static class GIFException extends Exception
  {
    public GIFException(String message)
    {
      super(message);
    }
  }
}
