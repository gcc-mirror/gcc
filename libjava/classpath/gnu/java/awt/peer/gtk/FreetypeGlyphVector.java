/* FreetypeGlyphVector.java
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

package gnu.java.awt.peer.gtk;

import java.awt.Font;
import java.awt.Shape;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphJustificationInfo;
import java.awt.font.GlyphMetrics;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Arrays;

public class FreetypeGlyphVector extends GlyphVector
{
  /**
   * The associated font and its peer.
   */
  private Font font;
  private GdkFontPeer peer; // ATTN: Accessed from native code.

  private Rectangle2D logicalBounds;

  private float[] glyphPositions;
  /**
   * The string represented by this GlyphVector.
   */
  private String s;

  /**
   * The font render context
   */
  private FontRenderContext frc;

  /**
   * The total # of glyphs.
   */
  private int nGlyphs;

  /**
   * The glyph codes
   */
  private int[] glyphCodes;

  /**
   * Glyph transforms. (de facto only the translation is used)
   */
  private AffineTransform[] glyphTransforms;

  private GlyphMetrics[] metricsCache;

  /**
   * Create a glyphvector from a given (Freetype) font and a String.
   */
  public FreetypeGlyphVector(Font f, String s, FontRenderContext frc)
  {
    this(f, s.toCharArray(), 0, s.length(), frc, Font.LAYOUT_LEFT_TO_RIGHT);
  }

  /**
   * Create a glyphvector from a given (Freetype) font and a String.
   */
  public FreetypeGlyphVector(Font f, char[] chars, int start, int len,
                             FontRenderContext frc, int flags)
  {
    this.s = new String(chars, start, len);

    this.font = f;
    this.frc = frc;
    if( !(font.getPeer() instanceof GdkFontPeer ) )
      throw new IllegalArgumentException("Not a valid font.");
    peer = (GdkFontPeer)font.getPeer();

    getGlyphs();
    if( flags == Font.LAYOUT_RIGHT_TO_LEFT )
      {
	// reverse the glyph ordering.
	int[] temp = new int[ nGlyphs ];
	for(int i = 0; i < nGlyphs; i++)
	  temp[ i ] = glyphCodes[ nGlyphs - i - 1];
	glyphCodes = temp;
      }
    performDefaultLayout();
  }

  /**
   * Create a glyphvector from a given set of glyph codes.
   */
  public FreetypeGlyphVector(Font f, int[] codes, FontRenderContext frc)
  {
    this.font = f;
    this.frc = frc;
    if( !(font.getPeer() instanceof GdkFontPeer ) )
      throw new IllegalArgumentException("Not a valid font.");
    peer = (GdkFontPeer)font.getPeer();

    glyphCodes = new int[ codes.length ];
    System.arraycopy(codes, 0, glyphCodes, 0, codes.length);
    nGlyphs = glyphCodes.length;
    performDefaultLayout();
  }

  /**
   * Cloning constructor
   */  
  private FreetypeGlyphVector( FreetypeGlyphVector gv )
  {
    font = gv.font;
    peer = gv.peer;
    frc = gv.frc;
    s = gv.s;
    nGlyphs = gv.nGlyphs;
    logicalBounds = gv.logicalBounds.getBounds2D();

    if( gv.metricsCache != null )
      {
	metricsCache = new GlyphMetrics[ nGlyphs ];
	System.arraycopy(gv.metricsCache, 0, metricsCache, 0, nGlyphs);
      }

    glyphCodes = new int[ nGlyphs ];
    glyphPositions = new float[(nGlyphs + 1) * 2];
    glyphTransforms = new AffineTransform[ nGlyphs ];
    for(int i = 0; i < nGlyphs; i++ )
      {
        glyphTransforms[ i ] = new AffineTransform( gv.glyphTransforms[ i ] );
        glyphCodes[i] = gv.glyphCodes[ i ];
      }
    System.arraycopy(gv.glyphPositions, 0, glyphPositions, 0,
                     glyphPositions.length);
  }

  /**
   * Create the array of glyph codes.
   */
  private void getGlyphs()
  {
    nGlyphs = s.codePointCount( 0, s.length() );
    glyphCodes = new int[ nGlyphs ];
    int[] codePoints = new int[ nGlyphs ];
    int stringIndex = 0;

    for(int i = 0; i < nGlyphs; i++)
      {
	codePoints[i] = s.codePointAt( stringIndex );
        // UTF32 surrogate handling
	if( codePoints[i] != (int)s.charAt( stringIndex ) )
	  stringIndex ++;
	stringIndex ++;

        if (Character.isISOControl(codePoints[i]))
          {
            // Replace with 'hair space'. Should better be 'zero-width space'
            // but that doesn't seem to be supported by default font.
            codePoints[i] = 8202;
          }
      }

   glyphCodes = getGlyphs( codePoints );
  }

  /**
   * Returns the glyph code within the font for a given character
   */
  public native int[] getGlyphs(int[] codepoints);

  /**
   * Returns the kerning of a glyph pair
   */
  private native Point2D getKerning(int leftGlyph, int rightGlyph);

  private native double[] getMetricsNative( int glyphCode );

  private native GeneralPath getGlyphOutlineNative(int glyphIndex);


  public Object clone()
  {
    return new FreetypeGlyphVector( this );
  }

  /**
   * Duh, compares two instances.
   */
  public boolean equals(GlyphVector gv)
  {
    if( ! (gv instanceof FreetypeGlyphVector) )
      return false;

    return (((FreetypeGlyphVector)gv).font.equals(font) && 
	    ((FreetypeGlyphVector)gv).frc.equals(frc)
	    && ((FreetypeGlyphVector)gv).s.equals(s));
  }

  /**
   * Returns the associated Font
   */
  public Font getFont()
  {
    return font;
  }

  /**
   * Returns the associated FontRenderContext
   */
  public FontRenderContext getFontRenderContext()
  {
    return frc;
  }

  /**
   * Layout the glyphs.
   */
  public void performDefaultLayout()
  {
    logicalBounds = null; // invalidate caches.
    glyphTransforms = new AffineTransform[nGlyphs];
    Arrays.fill(glyphTransforms, null);
    glyphPositions = new float[(nGlyphs + 1) * 2];

    GlyphMetrics gm = null;
    float x = 0;
    float y = 0;
    for(int i = 0; i < nGlyphs; i++)
      {
        gm = getGlyphMetrics( i );
        glyphPositions[i*2] = x;
        glyphPositions[i*2 + 1] = y;

        x += gm.getAdvanceX();
        y += gm.getAdvanceY();
        
        if (i != nGlyphs-1)
          {
            Point2D p = getKerning(glyphCodes[i], glyphCodes[i + 1]);
            x += p.getX();
            y += p.getY();
          }
      }
    glyphPositions[nGlyphs * 2] = x;
    glyphPositions[nGlyphs * 2 + 1] = y;
  }

  /**
   * Returns the code of the glyph at glyphIndex;
   */
  public int getGlyphCode(int glyphIndex)
  {
    return glyphCodes[ glyphIndex ];
  }

  /**
   * Returns multiple glyphcodes.
   */
  public int[] getGlyphCodes(int beginGlyphIndex, int numEntries, 
			     int[] codeReturn)
  {
    int[] rval;

    if( codeReturn == null || codeReturn.length < numEntries)
      rval = new int[ numEntries ];
    else
      rval = codeReturn;
    
    System.arraycopy(glyphCodes, beginGlyphIndex, rval, 0, numEntries);

    return rval;
  }

  public Shape getGlyphLogicalBounds(int glyphIndex)
  {
    GlyphMetrics gm = getGlyphMetrics( glyphIndex );
    if( gm == null )
      return null; 
    Rectangle2D r = gm.getBounds2D();
    Point2D p = getGlyphPosition( glyphIndex );
    
    double[] bounds = new double[] {p.getX() + r.getX() - gm.getLSB(),
                                    p.getY() + r.getY(),
                                    p.getX() + r.getX() - gm.getLSB() + gm.getAdvanceX(),
                                    p.getY() + r.getY() + r.getHeight()};
    
    if (glyphTransforms[glyphIndex] != null)
      glyphTransforms[glyphIndex].transform(bounds, 0, bounds, 0, 4);
    
    return new Rectangle2D.Double(bounds[0], bounds[1], bounds[2] - bounds[0],
                                  bounds[3] - bounds[1]);
  }

  /*
   * FIXME: Not all glyph types are supported.
   * (The JDK doesn't really seem to do so either)
   */
  public void setupGlyphMetrics()
  {
    metricsCache = new GlyphMetrics[ nGlyphs ];

    for(int i = 0; i < nGlyphs; i++)
      {
	GlyphMetrics gm = (GlyphMetrics)
	  peer.getGlyphMetrics( glyphCodes[ i ] );
	if( gm == null )
	  {
	    double[] val = getMetricsNative( glyphCodes[ i ] );
	    if( val == null )
	      gm = null;
	    else
	      {
		gm = new GlyphMetrics( true, 
				       (float)val[1], 
				       (float)val[2], 
				       new Rectangle2D.Double
				       ( val[3], val[4], 
					 val[5], val[6] ),
				       GlyphMetrics.STANDARD );
		peer.putGlyphMetrics( glyphCodes[ i ], gm );
	      }
	  }
	metricsCache[ i ] = gm;
      }
  }

  /**
   * Returns the metrics of a single glyph.
   */
  public GlyphMetrics getGlyphMetrics(int glyphIndex)
  {
    if( metricsCache == null )
      setupGlyphMetrics();

    return metricsCache[ glyphIndex ];
  }

  /**
   * Returns the outline of a single glyph.
   */
  public Shape getGlyphOutline(int glyphIndex)
  {
    GeneralPath gp = getGlyphOutlineNative( glyphCodes[ glyphIndex ] );
    if (glyphTransforms[glyphIndex] != null)
      gp.transform( glyphTransforms[glyphIndex]);
    
    return gp;
  }

  /**
   * Returns the position of a single glyph.
   */
  public Point2D getGlyphPosition(int glyphIndex)
  {
    return new Point2D.Float(glyphPositions[glyphIndex*2],
                             glyphPositions[glyphIndex*2 + 1]);
  }

  /**
   * Returns the positions of multiple glyphs.
   */
  public float[] getGlyphPositions(int beginGlyphIndex, int numEntries, 
				   float[] positionReturn)
  {
    if (positionReturn == null || positionReturn.length < (numEntries * 2))
      positionReturn = new float[numEntries*2];
    
    System.arraycopy(glyphPositions, beginGlyphIndex*2, positionReturn, 0,
                     numEntries*2);
    return positionReturn;
  }

  /**
   * Returns the transform of a glyph.
   */
  public AffineTransform getGlyphTransform(int glyphIndex)
  {
    return glyphTransforms[glyphIndex];
  }

  /**
   * Returns the visual bounds of a glyph
   * May be off by a pixel or two due to hinting/rasterization.
   */
  public Shape getGlyphVisualBounds(int glyphIndex)
  {
    return getGlyphOutline( glyphIndex ).getBounds2D();
  }

  /**
   * Return the logical bounds of the whole thing.
   */
  public Rectangle2D getLogicalBounds()
  {
    if( nGlyphs == 0 )
      return new Rectangle2D.Double(0, 0, 0, 0);
    if( logicalBounds != null )
      return logicalBounds;

    Rectangle2D rect = (Rectangle2D)getGlyphLogicalBounds( 0 );
    AffineTransform tx = new AffineTransform();
    for( int i = 1; i < nGlyphs; i++ )
      {
        Rectangle2D r2 = (Rectangle2D)getGlyphLogicalBounds( i );
        
        rect = rect.createUnion( r2 );
      }

    logicalBounds = rect;
    return rect;
  }

  /**
   * Returns the number of glyphs.
   */
  public int getNumGlyphs()
  {
    return glyphCodes.length;
  }

  /**
   * Returns the outline of the entire GlyphVector.
   */
  public Shape getOutline()
  {
    GeneralPath path = new GeneralPath();
    AffineTransform tx = new AffineTransform();
    for( int i = 0; i < getNumGlyphs(); i++ )
      {
        Shape outline = getGlyphOutline(i);
        tx.setToTranslation(glyphPositions[i*2], glyphPositions[i*2 +1]);
        outline = tx.createTransformedShape(outline);
        path.append(outline, false);
      }
    return path;
  }

  /**
   * TODO: 
   * FreeType does not currently have an API for the JSTF table. We should 
   * probably get the table ourselves from FT and pass it to some parser 
   * which the native font peers will need.
   */
  public GlyphJustificationInfo getGlyphJustificationInfo(int glyphIndex)
  {
    return null;
  }

  /**
   * Returns the outline of the entire vector, drawn at (x,y).
   */
  public Shape getOutline(float x, float y)
  {
    AffineTransform tx = AffineTransform.getTranslateInstance( x, y );
    GeneralPath gp = (GeneralPath)getOutline();
    gp.transform( tx );
    return gp;
  }

  /**
   * Returns the visual bounds of the entire GlyphVector.
   * May be off by a pixel or two due to hinting/rasterization.
   */
  public Rectangle2D getVisualBounds()
  {
    return getOutline().getBounds2D();
  }

  /**
   * Sets the position of a glyph.
   */
  public void setGlyphPosition(int glyphIndex, Point2D newPos)
  {
    glyphPositions[glyphIndex*2] = (float)(newPos.getX());
    glyphPositions[glyphIndex*2 + 1] = (float)(newPos.getY());
    logicalBounds = null;
  }

  /**
   * Sets the transform of a single glyph.
   */
  public void setGlyphTransform(int glyphIndex, AffineTransform newTX)
  {
    logicalBounds = null;
    glyphTransforms[glyphIndex] = newTX;
  }
}
