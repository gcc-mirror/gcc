/* TextLayout.java
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


package java.awt.font;

import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.text.AttributedCharacterIterator;
import java.util.Map;

/**
 * @author Michael Koch
 */
public final class TextLayout implements Cloneable
{
  public static final CaretPolicy DEFAULT_CARET_POLICY = new CaretPolicy ();

  public static class CaretPolicy
  {
    public CaretPolicy ()
    {
      // Do nothing here.
    }

    public TextHitInfo getStrongCaret (TextHitInfo hit1, TextHitInfo hit2,
                                       TextLayout layout)
    {
      throw new Error ("not implemented");
    }
  }

  private FontRenderContext fontRenderContext;
  
  public TextLayout (AttributedCharacterIterator text, FontRenderContext frc)
  {
    // FIXME
    this.fontRenderContext = frc;
  }

  public TextLayout (String string, Font font, FontRenderContext frc) 
  {
    // FIXME
    this.fontRenderContext = frc;
  }

  public TextLayout (String string, Map attributes, FontRenderContext frc) 
  {
    // FIXME
    this.fontRenderContext = frc;
  }

  protected Object clone ()
  {
    try
      {
        return super.clone ();
      }
    catch (CloneNotSupportedException e)
      {
        // This should never occur
        throw new InternalError ();
      }
  }

  public void draw (Graphics2D g2, float x, float y) 
  {
    throw new Error ("not implemented");
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof TextLayout))
      return false;

    return equals ((TextLayout) obj);
  }

  public boolean equals (TextLayout tl)
  {
    throw new Error ("not implemented");
  }

  public float getAdvance ()
  {
    throw new Error ("not implemented");
  }

  public float getAscent ()
  {
    throw new Error ("not implemented");
  }

  public byte getBaseline ()
  {
    throw new Error ("not implemented");
  }

  public float[] getBaselineOffsets ()
  {
    throw new Error ("not implemented");
  }

  public Shape getBlackBoxBounds (int firstEndpoint, int secondEndpoint)
  {
    throw new Error ("not implemented");
  }

  public Rectangle2D getBounds()
  {
    throw new Error ("not implemented");
  }

  public float[] getCaretInfo (TextHitInfo hit)
  {
    throw new Error ("not implemented");
  }

  public float[] getCaretInfo (TextHitInfo hit, Rectangle2D bounds)
  {
    throw new Error ("not implemented");
  }

  public Shape getCaretShape (TextHitInfo hit)
  {
    throw new Error ("not implemented");
  }

  public Shape getCaretShape (TextHitInfo hit, Rectangle2D bounds)
  {
    throw new Error ("not implemented");
  }

  public Shape[] getCaretShapes (int offset)
  {
    throw new Error ("not implemented");
  }

  public Shape[] getCaretShapes (int offset, Rectangle2D bounds)
  {
    throw new Error ("not implemented");
  }

  public Shape[] getCaretShapes (int offset, Rectangle2D bounds,
                                 TextLayout.CaretPolicy policy)
  {
    throw new Error ("not implemented");
  }

  public int getCharacterCount ()
  {
    throw new Error ("not implemented");
  }

  public byte getCharacterLevel (int index)
  {
    throw new Error ("not implemented");
  }

  public float getDescent ()
  {
    throw new Error ("not implemented");
  }

  public TextLayout getJustifiedLayout (float justificationWidth)
  {
    throw new Error ("not implemented");
  }

  public float getLeading ()
  {
    throw new Error ("not implemented");
  }

  public Shape getLogicalHighlightShape (int firstEndpoint, int secondEndpoint)
  {
    throw new Error ("not implemented");
  }

  public Shape getLogicalHighlightShape (int firstEndpoint, int secondEndpoint,
                                         Rectangle2D bounds)
  {
    throw new Error ("not implemented");
  }

  public int[] getLogicalRangesForVisualSelection (TextHitInfo firstEndpoint,
                                                   TextHitInfo secondEndpoint)
  {
    throw new Error ("not implemented");
  }

  public TextHitInfo getNextLeftHit (int offset)
  {
    throw new Error ("not implemented");
  }

  public TextHitInfo getNextLeftHit (int offset, TextLayout.CaretPolicy policy)
  {
    throw new Error ("not implemented");
  }

  public TextHitInfo getNextLeftHit (TextHitInfo hit)
  {
    throw new Error ("not implemented");
  }

  public TextHitInfo getNextRightHit (int offset)
  {
    throw new Error ("not implemented");
  }

  public TextHitInfo getNextRightHit (int offset, TextLayout.CaretPolicy policy)
  {
    throw new Error ("not implemented");
  }

  public TextHitInfo getNextRightHit (TextHitInfo hit)
  {
    throw new Error ("not implemented");
  }

  public Shape getOutline (AffineTransform tx)
  {
    throw new Error ("not implemented");
  }

  public float getVisibleAdvance ()
  {
    throw new Error ("not implemented");
  }

  public Shape getVisualHighlightShape (TextHitInfo firstEndpoint,
                                        TextHitInfo secondEndpoint)
  {
    throw new Error ("not implemented");
  }

  public Shape getVisualHighlightShape (TextHitInfo firstEndpoint,
                                        TextHitInfo secondEndpoint,
                                        Rectangle2D bounds)
  {
    throw new Error ("not implemented");
  }

  public TextHitInfo getVisualOtherHit (TextHitInfo hit)
  {
    throw new Error ("not implemented");
  }

  protected void handleJustify (float justificationWidth)
  {
    throw new Error ("not implemented");
  }

  public int hashCode ()
  {
    throw new Error ("not implemented");
  }

  public TextHitInfo hitTestChar (float x, float y)
  {
    throw new Error ("not implemented");
  }

  public TextHitInfo hitTestChar (float x, float y, Rectangle2D bounds)
  {
    throw new Error ("not implemented");
  }

  public boolean isLeftToRight ()
  {
    throw new Error ("not implemented");
  }

  public boolean isVertical ()
  {
    throw new Error ("not implemented");
  }

  public String toString ()
  {
    throw new Error ("not implemented");
  }
}
