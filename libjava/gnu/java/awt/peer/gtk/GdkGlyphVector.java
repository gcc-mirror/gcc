/* GdkGlyphVector.java -- Glyph vector object
   Copyright (C) 2003 Free Software Foundation, Inc.

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
   Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.

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

import java.awt.*;
import java.awt.font.*;
import java.awt.geom.*;
import java.io.InputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.Locale;
import java.util.Map;
import java.util.StringTokenizer;
import java.text.CharacterIterator;
import java.text.AttributedCharacterIterator;
import gnu.classpath.Configuration;

public class GdkGlyphVector extends GlyphVector 
{

  static 
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("gtkpeer");
      }
    initStaticState ();
  }
  native static void initStaticState ();
  private final int native_state = GtkGenericPeer.getUniqueInteger ();

  private Font font;
  private FontRenderContext ctx;
    
  private native void initState (GdkClasspathFontPeer peer, FontRenderContext ctx);
  private native void setChars (String s);
  private native void setGlyphCodes (int codes[]);
  private native void dispose ();
  private native int glyphCode (int idx);
  private native int numGlyphs ();
  private native int glyphCharIndex (int idx);
  private native double[] allLogicalExtents ();
  private native double[] allInkExtents ();
  private native double[] glyphLogicalExtents (int idx);
  private native double[] glyphInkExtents (int idx);
  private native boolean glyphIsHorizontal (int idx);
  private native boolean isEqual (GdkGlyphVector ggv);


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


  public GdkGlyphVector (Font f, GdkClasspathFontPeer peer, FontRenderContext c, String s)
  {
    font = f;
    ctx = c;
    initState (peer, ctx);
    setChars (s);
  }

  public GdkGlyphVector (Font f, GdkClasspathFontPeer peer, FontRenderContext c, int codes[])
  {
    font = f;
    ctx = c;
    initState (peer, ctx);
    setGlyphCodes (codes);
  }

  protected void finalize ()
  {
    dispose ();
  }

  public Font getFont () 
  { 
    return font; 
  }

  public FontRenderContext getFontRenderContext () 
  { 
    return ctx; 
  }

  public int getGlyphCharIndex (int glyphIndex) 
  { 
    return glyphCharIndex (glyphIndex); 
  }

  public int[] getGlyphCharIndices (int beginGlyphIndex, 
                                    int numEntries,
                                    int[] codeReturn)
  {
    int ix[] = codeReturn;
    if (ix == null)
      ix = new int[numEntries];

    for (int i = 0; i < numEntries; i++)
      ix[i] = glyphCharIndex (beginGlyphIndex + i);
    return ix;
  }

  public int getGlyphCode (int glyphIndex) 
  { 
    return glyphCode (glyphIndex); 
  }

  public int[] getGlyphCodes (int beginGlyphIndex, int numEntries,
                              int[] codeReturn)
  {
    int ix[] = codeReturn;
    if (ix == null)
      ix = new int[numEntries];

    for (int i = 0; i < numEntries; i++)
      ix[i] = glyphCode (beginGlyphIndex + i);
    return ix;
  }

  public Shape getGlyphLogicalBounds (int glyphIndex)
  {
    double extents[] = glyphLogicalExtents (glyphIndex);
    return new Rectangle2D.Double (extents[0], extents[1],
                                   extents[2], extents[3]);
  }
    
  public GlyphMetrics getGlyphMetrics (int glyphIndex)
  {
    double extents[] = glyphLogicalExtents (glyphIndex);
    Rectangle2D log_bounds = new Rectangle2D.Double (extents[0], extents[1],
                                                     extents[2], extents[3]);

    extents = glyphInkExtents (glyphIndex);
    Rectangle2D ink_bounds = new Rectangle2D.Double (extents[0], extents[1],
                                                     extents[2], extents[3]);
      
    boolean is_horizontal = glyphIsHorizontal (glyphIndex);

    return new GlyphMetrics (is_horizontal,
                             (float)(log_bounds.getWidth() + log_bounds.getX()), 
                             (float)(log_bounds.getHeight() + log_bounds.getY()),
                             ink_bounds, GlyphMetrics.STANDARD);
  }

  public Shape getGlyphOutline (int glyphIndex)
  {
    throw new UnsupportedOperationException ();      
  }

  public Shape getGlyphOutline (int glyphIndex, float x, float y)
  {
    throw new UnsupportedOperationException ();
  }

  public Rectangle getGlyphPixelBounds (int glyphIndex, 
                                        FontRenderContext renderFRC,
                                        float x, float y)
  {
    double extents[] = glyphInkExtents(glyphIndex);
    return new Rectangle ((int)x, (int)y, (int)extents[2], (int)extents[3]);
  }
    
  public Point2D getGlyphPosition (int glyphIndex)
  {
    float[] ret = new float[2 * (glyphIndex + 1)];
    getGlyphPositions (0, glyphIndex + 1, ret);
    return new Point2D.Float (ret[2 * glyphIndex], 
                              ret[2 * glyphIndex + 1]);
  }

  public float[] getGlyphPositions (int beginGlyphIndex,
                                    int numEntries,
                                    float[] positionReturn)
  {
    float fx[] = positionReturn;
    if (fx == null)
      fx = new float[numEntries * 2];


    float x = 0.0f;
    float y = 0.0f;
    for (int i = 0; i < numEntries; ++i)
      {
        boolean is_horizontal = glyphIsHorizontal (beginGlyphIndex + i);
        double log_extents[] = glyphLogicalExtents (beginGlyphIndex + i);
        fx[2*i]     = x + (float)log_extents[0]; // x offset
        fx[2*i + 1] = y + (float)log_extents[1]; // y offset
        if (is_horizontal)
          x += (float)log_extents[2]; // x advance ("logical width") in pango-ese
        else
          y += (float)log_extents[3]; // y advance ("logical height") in pango-ese
      }
    return fx;
  }

  public AffineTransform getGlyphTransform (int glyphIndex)
  {
    // glyphs don't have independent transforms in these simple glyph
    // vectors; docs specify null is an ok return here.
    return null;  
  }
    
  public Shape getGlyphVisualBounds (int glyphIndex)
  {
    double extents[] = glyphInkExtents (glyphIndex);
    return new Rectangle2D.Double (extents[0], extents[1], 
                                   extents[2], extents[3]);
  }
    
  public int getLayoutFlags ()
  {
    return 0;
  }

  public Rectangle2D getLogicalBounds ()
  {
    double extents[] = allLogicalExtents ();
    return new Rectangle2D.Double (extents[0], extents[1], 
                                   extents[2], extents[3]);
  }

  public int getNumGlyphs ()
  {
    return numGlyphs ();
  }

  public Shape getOutline ()
  {
    throw new UnsupportedOperationException ();      
  }

  public Rectangle getPixelBounds (FontRenderContext renderFRC,
                                   float x, float y)
  {
    double extents[] = allInkExtents();
    return new Rectangle ((int)x, (int)y, 
                          (int)extents[2], (int)extents[3]);
  }
    
  public Rectangle2D getVisualBounds ()
  {
    double extents[] = allInkExtents();
    return new Rectangle2D.Double (extents[0], extents[1], 
                                   extents[2], extents[3]);
  }

  public void performDefaultLayout ()
  {
  }
    
  public void setGlyphPosition (int glyphIndex, Point2D newPos)
  {
    // should we be ok twiddling pango's structure here?
    throw new UnsupportedOperationException ();      
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

    GdkGlyphVector ggv = (GdkGlyphVector)gv;
    return isEqual(ggv);
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
