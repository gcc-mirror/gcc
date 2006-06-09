/* GlyphVector.java
   Copyright (C) 2002 Free Software Foundation, Inc.

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


package java.awt.font;

import java.awt.Font;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

/**
 * @author Lillian Angel (langel at redhat dot com)
 * @author Michael Koch
 */
public abstract class GlyphVector implements Cloneable
{
  public static final int FLAG_COMPLEX_GLYPHS = 8;
  public static final int FLAG_HAS_POSITION_ADJUSTMENTS = 2;
  public static final int FLAG_HAS_TRANSFORMS = 1;
  public static final int FLAG_MASK = 15;
  public static final int FLAG_RUN_RTL = 4;

  /**
   * Constructs a <code>GlyphVector</code> object.
   */
  public GlyphVector ()
  {
  }

  public abstract boolean equals (GlyphVector set);

  public abstract Font getFont ();

  public abstract FontRenderContext getFontRenderContext ();
    
  public int getGlyphCharIndex (int glyphIndex)
  {
    return glyphIndex;
  }
    
  public int[] getGlyphCharIndices(int beginGlyphIndex, int numEntries,
                                   int[] codeReturn)
  {
    if (codeReturn == null)
      codeReturn = new int[numEntries];

    int i = 0;
    int j = beginGlyphIndex;
    while (j < numEntries)
      codeReturn[i++] = getGlyphCharIndex(j++);
    
    return codeReturn;
  }
    
  public abstract int getGlyphCode (int glyphIndex);

  public abstract int[] getGlyphCodes (int beginGlyphIndex, int numEntries,
                                       int[] codeReturn);

  public abstract GlyphJustificationInfo getGlyphJustificationInfo
    (int glyphIndex);

  public abstract Shape getGlyphLogicalBounds (int glyphIndex);

  public abstract GlyphMetrics getGlyphMetrics (int glyphIndex);

  public abstract Shape getGlyphOutline (int glyphIndex);

  public Shape getGlyphOutline(int glyphIndex, float x, float y)
  {
    Shape s = getGlyphOutline(glyphIndex);
    
    // This is the only way to translate the origin of a shape
    AffineTransform at = AffineTransform.getTranslateInstance(x, y);
    return at.createTransformedShape(s);
  }

  public Rectangle getGlyphPixelBounds(int index, FontRenderContext renderFRC,
                                       float x, float y)
  {
    Rectangle bounds = new Rectangle();
    Rectangle2D rect = getGlyphVisualBounds(index).getBounds2D();
    
    bounds.x = (int) (rect.getX() + x);
    bounds.y = (int) (rect.getY() + y);
    bounds.width = (int) rect.getMaxX() - bounds.x;
    bounds.height = (int) rect.getMaxY() - bounds.y;
    
    return bounds;
  }

  public abstract Point2D getGlyphPosition (int glyphIndex);

  public abstract float[] getGlyphPositions (int beginGlyphIndex,
                                             int numEntries,
                                             float[] positionReturn);

  public abstract AffineTransform getGlyphTransform (int glyphIndex);

  public abstract Shape getGlyphVisualBounds (int glyphIndex);

  public int getLayoutFlags()
  {
    return 0;
  }

  public abstract Rectangle2D getLogicalBounds ();

  public abstract int getNumGlyphs ();
  
  public abstract Shape getOutline ();

  public abstract Shape getOutline (float x, float y);

  public Rectangle getPixelBounds (FontRenderContext renderFRC,
                                   float x, float y)
  {
    Rectangle bounds = new Rectangle();
    Rectangle2D rect = getVisualBounds();
    
    bounds.x = (int) (rect.getX() + x);
    bounds.y = (int) (rect.getY() + y);
    bounds.width = (int) rect.getMaxX() - bounds.x;
    bounds.height = (int) rect.getMaxY() - bounds.y;
    
    return bounds;
  }

  public abstract Rectangle2D getVisualBounds ();

  public abstract void performDefaultLayout ();

  public abstract void setGlyphPosition (int glyphIndex, Point2D newPos);

  public abstract void setGlyphTransform (int glyphIndex,
                                          AffineTransform newTX);
}
