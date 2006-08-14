/* PNGFile.java -- High-level representation of a PNG file.
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

import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;
import java.util.Vector;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;
import java.awt.image.ColorModel;
import java.awt.image.IndexColorModel;
import java.awt.color.ColorSpace;

public class PNGFile 
{
  /**
   * The PNG file signature.
   */
  private static final byte[] signature = new byte[]
  { (byte)137, 80, 78, 71, 13, 10, 26, 10 };

  /**
   * The end chunk in raw form, no need for anything fancy here, it's just
   * 0 bytes of length, the "IEND" tag and its CRC.
   */
  private static final byte[] endChunk = new byte[]
  { 0, 0, 0, 0, (byte)0x49, (byte)0x45, (byte)0x4E, (byte)0x44, 
    (byte)0xAE, (byte)0x42, (byte)0x60, (byte)0x82 };

  /**
   * The loaded data.
   */
  private Vector chunks;

  /**
   * The Header chunk
   */
  private PNGHeader header;

  /**
   * Whether this file has a palette chunk or not.
   */
  private boolean hasPalette;

  /**
   * Image width and height.
   */
  private int width, height;

  /**
   * The decoder, if any.
   */
  private PNGDecoder decoder;

  /**
   * The encoder, if any. (Either this or the above must exist).
   */
  private PNGEncoder encoder;
  
  /**
   * The source of this PNG (if encoding)
   */
  private BufferedImage sourceImage;

  /**
   * Creates a PNGFile object from an InputStream.
   */
  public PNGFile(InputStream in) throws IOException, PNGException
  {
    PNGChunk chunk;
    byte[] fileHdr = new byte[8];
    chunks = new Vector(); 
    hasPalette = false;

    if( in.read( fileHdr ) != 8 )
      throw new IOException("Could not read file header.");
    if( !validateHeader( fileHdr ) )
      throw new PNGException("Invalid file header. Not a PNG file.");

    chunk = PNGChunk.readChunk( in, false );
    if( !(chunk instanceof PNGHeader) )
      throw new PNGException("First chunk not a header chunk.");
    header = (PNGHeader)chunk;
    if( !header.isValidChunk() )
      throw new PNGException("First chunk not a valid header.");
    System.out.println(header);

    decoder = new PNGDecoder( header );
    // Read chunks.
    do
      {
	chunk = PNGChunk.readChunk( in, false );
	/*
	 * We could exit here or output some kind of warning.
	 * But in the meantime, we'll just silently drop invalid chunks.
	 */
	if( chunk.isValidChunk() )
	  {
	    if( chunk instanceof PNGData )
	      decoder.addData( (PNGData)chunk );
	    else // Silently ignore multiple headers, and use only the first.
	      if( chunk.getType() != PNGChunk.TYPE_END )
		{
		  chunks.add( chunk ); 
		  hasPalette |= ( chunk instanceof PNGPalette );
		}
	  }
	else
	  System.out.println("WARNING: Invalid chunk!");
      }
    while( chunk.getType() != PNGChunk.TYPE_END );

    if( header.isIndexed() && !hasPalette )
      throw new PNGException("File is indexed color and has no palette.");

    width = header.getWidth();
    height = header.getHeight();
  }

  /**
   * Creates a PNG file from an existing BufferedImage.
   */
  public PNGFile(BufferedImage bi) throws PNGException
  {
    sourceImage = bi;
    width = bi.getWidth();
    height = bi.getHeight();
    chunks = new Vector();
    encoder = new PNGEncoder( bi );
    header = encoder.getHeader();
    if( header.isIndexed() ) 
      chunks.add( encoder.getPalette() );

    // Do the compression and put the data chunks in the list.
    chunks.addAll( encoder.encodeImage() );
  }

  /**
   * Writes a PNG file to an OutputStream
   */
  public void writePNG(OutputStream out) throws IOException
  {
    out.write( signature ); // write the signature.
    header.writeChunk( out );
    for( int i = 0; i < chunks.size(); i++ )
      {
	PNGChunk chunk = ((PNGChunk)chunks.elementAt(i));
	chunk.writeChunk( out );
      }
    out.write( endChunk );
  }

  /**
   * Check 8 bytes to see if it's a valid PNG header.
   */
  private boolean validateHeader( byte[] hdr )
  {
    if( hdr.length != 8 )
      return false;
    for( int i = 0; i < 8; i++ )
      if( signature[i] != hdr[i] )
	return false;
    return true;
  }

  /**
   * Return a loaded image as a bufferedimage.
   */
  public BufferedImage getBufferedImage()
  {
    if( decoder == null )
      return sourceImage;

    WritableRaster r = decoder.getRaster( header );
    ColorModel cm;
    if( header.isIndexed() )
      {
	PNGPalette pngp = getPalette();
	cm = pngp.getPalette( getColorSpace() );
      }
    else
      cm = decoder.getColorModel( getColorSpace(), 
				  header.getColorType(), 
				  header.getDepth() );
    
    return new BufferedImage(cm, r, false, null);
  } 

  /**
   * Find the palette chunk and return it
   */
  private PNGPalette getPalette()
  {
    for(int i = 0; i < chunks.size(); i++ )
      if( chunks.elementAt(i) instanceof PNGPalette )
	return ((PNGPalette)chunks.elementAt(i));
    return null;
  }

  /**
   * Return the Color space to use, first preference is ICC profile, then
   * a gamma chunk, or returns null for the default sRGB.
   */
  private ColorSpace getColorSpace()
  {
    PNGICCProfile icc = null;
    PNGGamma gamma = null;
    for(int i = 0; i < chunks.size(); i++ )
      {
	if( chunks.elementAt(i) instanceof PNGICCProfile )
	  icc = ((PNGICCProfile)chunks.elementAt(i));
	else if(chunks.elementAt(i) instanceof PNGGamma )
	  gamma = ((PNGGamma)chunks.elementAt(i));
      }

    if( icc != null )
      return icc.getColorSpace();
//     if( gamma != null && !header.isGrayscale())
//       return gamma.getColorSpace( header.isGrayscale() );
    return null;
  }
}
