/* GdkGlyphVector.java -- Glyph vector object
   Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.

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
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphJustificationInfo;
import java.awt.font.GlyphMetrics;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

public class GdkGlyphVector extends GlyphVector
{

  /* We use a simple representation for glyph vectors here. Glyph i
   * consumes 8 doubles:
   *
   *      logical x: extents[ 10*i     ]
   *      logical y: extents[ 10*i + 1 ]
   *  logical width: extents[ 10*i + 2 ]
   * logical height: extents[ 10*i + 3 ]
   *
   *       visual x: extents[ 10*i + 4 ]
   *       visual y: extents[ 10*i + 5 ]
   *   visual width: extents[ 10*i + 6 ]
   *  visual height: extents[ 10*i + 7 ]
   *
   *   origin pos x: extents[ 10*i + 8 ]
   *   origin pos y: extents[ 10*i + 9 ]
   * 
   * as well as one int, code[i], representing the glyph code in the font.
   */

  double [] extents;
  int [] codes;

  Font font;
  FontRenderContext fontRenderContext;

  Rectangle2D allLogical;
  Rectangle2D allVisual;

  public GdkGlyphVector(double[] extents, int[] codes, Font font, FontRenderContext frc)
  {
    this.extents = extents;
    this.codes = codes;
    this.font = font;
    this.fontRenderContext = frc;

    allLogical = new Rectangle2D.Double();
    allVisual = new Rectangle2D.Double();
    
    for (int i = 0; i < codes.length; ++i)
      {
        allLogical.add (new Rectangle2D.Double(extents[10*i    ] + extents[10*i + 8],
                                               extents[10*i + 1] + extents[10*i + 9],
                                               extents[10*i + 2],
                                               extents[10*i + 3]));

        allVisual.add (new Rectangle2D.Double(extents[10*i + 4] + extents[10*i + 8],
                                              extents[10*i + 5] + extents[10*i + 9],
                                              extents[10*i + 6],
                                              extents[10*i + 7]));
      }
  }

  /* 
     geometric notes:

     the FRC contains a mapping from points -> pixels.

     typographics points are typically 1/72 of an inch.

     pixel displays are often around 72 dpi.

     so the FRC can get away with using an identity transform on a screen,
     often. behavior is documented by sun to fall back to an identity
     transform if the internal transformation is null.

     coordinates coming up from pango are expressed as floats -- in device
     space, so basically pixels-with-fractional-bits -- derived from their
     storage format in pango (1024ths of pixels). 

     it is not clear from the javadocs whether the results of methods like
     getGlyphPositions ought to return coordinates in device space, or
     "point" space, or what. for now I'm returning them in device space.
     
   */

  public double[] getExtents() 
  {
    return extents;
  }

  public int[] getCodes()
  {
    return codes;
  }

  public Font getFont () 
  { 
    return font; 
  }

  public FontRenderContext getFontRenderContext () 
  { 
    return fontRenderContext; 
  }

  public int getGlyphCharIndex (int glyphIndex) 
  { 
    // FIXME: currently pango does not provide glyph-by-glyph
    // reverse mapping information, so we assume a broken 1:1
    // glyph model here. This is plainly wrong.
    return glyphIndex;
  }

  public int[] getGlyphCharIndices (int beginGlyphIndex, 
                                    int numEntries,
                                    int[] codeReturn)
  {
    int ix[] = codeReturn;
    if (ix == null)
      ix = new int[numEntries];

    for (int i = 0; i < numEntries; i++)
      ix[i] = getGlyphCharIndex (beginGlyphIndex + i);
    return ix;
  }

  public int getGlyphCode (int glyphIndex) 
  { 
    return codes[glyphIndex];
  }

  public int[] getGlyphCodes (int beginGlyphIndex, int numEntries,
                              int[] codeReturn)
  {
    if (codeReturn == null)
      codeReturn = new int[numEntries];

    System.arraycopy(codes, beginGlyphIndex, codeReturn, 0, numEntries);
    return codeReturn;
  }

  public Shape getGlyphLogicalBounds (int i)
  {
    return new Rectangle2D.Double (extents[8*i], extents[8*i + 1],
                                   extents[8*i + 2], extents[8*i + 3]);
  }
    
  public GlyphMetrics getGlyphMetrics (int i)
  {
    // FIXME: pango does not yield vertical layout information at the
    // moment.

    boolean is_horizontal = true;
    double advanceX = extents[8*i + 2]; // "logical width" == advanceX 
    double advanceY = 0; 
   
    return new GlyphMetrics (is_horizontal, 
                             (float) advanceX, (float) advanceY, 
                             (Rectangle2D) getGlyphVisualBounds(i), 
                             GlyphMetrics.STANDARD);
  }

  public Shape getGlyphOutline (int glyphIndex)
  {
    throw new UnsupportedOperationException ();      
  }

  public Shape getGlyphOutline (int glyphIndex, float x, float y)
  {
    throw new UnsupportedOperationException ();
  }

  public Rectangle getGlyphPixelBounds (int i, 
                                        FontRenderContext renderFRC,
                                        float x, float y)
  {
    return new Rectangle((int) x, (int) y,
                         (int) extents[8*i + 6], (int) extents[8*i + 7]);
  }
    
  public Point2D getGlyphPosition (int i)
  {
    return new Point2D.Double (extents[10*i + 8], 
                               extents[10*i + 9]);
  }

  public float[] getGlyphPositions (int beginGlyphIndex,
                                    int numEntries,
                                    float[] positionReturn)
  {
    float fx[] = positionReturn;
    if (fx == null)
      fx = new float[numEntries * 2];

    for (int i = 0; i < numEntries; ++i)
      {
        fx[2*i    ] = (float) extents[10*i + 8];
        fx[2*i + 1] = (float) extents[10*i + 9];
      }
    return fx;
  }

  public AffineTransform getGlyphTransform (int glyphIndex)
  {
    // Glyphs don't have independent transforms in these simple glyph
    // vectors; docs specify null is an ok return here.
    return null;  
  }
    
  public Shape getGlyphVisualBounds (int i)
  {
    return new Rectangle2D.Double(extents[8*i + 4], extents[8*i + 5],
                                  extents[8*i + 6], extents[8*i + 7]);
  }
    
  public int getLayoutFlags ()
  {
    return 0;
  }

  public Rectangle2D getLogicalBounds ()
  {
    return allLogical;
  }

  public int getNumGlyphs ()
  {
    return codes.length;
  }

  public Shape getOutline ()
  {
    throw new UnsupportedOperationException ();      
  }

  public Rectangle getPixelBounds (FontRenderContext renderFRC,
                                   float x, float y)
  {
    return new Rectangle((int)x, 
                         (int)y, 
                         (int) allVisual.getWidth(),
                         (int) allVisual.getHeight());
  }
    
  public Rectangle2D getVisualBounds ()
  {
    return allVisual;
  }

  public void performDefaultLayout ()
  {
  }
    
  public void setGlyphPosition (int i, Point2D newPos)
  {
    extents[8*i    ] = newPos.getX();
    extents[8*i + 1] = newPos.getY();

    extents[8*i + 4] = newPos.getX();
    extents[8*i + 5] = newPos.getY();
  }

  public void setGlyphTransform (int glyphIndex,
                                 AffineTransform newTX)
  {
    // not yet.. maybe not ever?
    throw new UnsupportedOperationException ();      
  }

  public boolean equals(GlyphVector gv)
  {
    if (gv == null)
      return false;

    if (! (gv instanceof GdkGlyphVector))
      return false;

    GdkGlyphVector ggv = (GdkGlyphVector) gv;

    if ((ggv.codes.length != this.codes.length)
        || (ggv.extents.length != this.extents.length))
      return false;
    
    if ((ggv.font == null && this.font != null)
        || (ggv.font != null && this.font == null)
        || (!ggv.font.equals(this.font)))
      return false;

    if ((ggv.fontRenderContext == null && this.fontRenderContext != null)
        || (ggv.fontRenderContext != null && this.fontRenderContext == null)
        || (!ggv.fontRenderContext.equals(this.fontRenderContext)))
      return false;

    for (int i = 0; i < ggv.codes.length; ++i)
      if (ggv.codes[i] != this.codes[i])
        return false;

    for (int i = 0; i < ggv.extents.length; ++i)
      if (ggv.extents[i] != this.extents[i])
        return false;

    return true;
  }

  public GlyphJustificationInfo getGlyphJustificationInfo(int idx)
  {
    throw new UnsupportedOperationException ();      
  }

  public Shape getOutline(float x, float y)    
  {
    throw new UnsupportedOperationException ();      
  }

}
