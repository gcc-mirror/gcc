/* TextLayout.java --
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

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

import gnu.java.awt.ClasspathToolkit;
import gnu.java.awt.peer.ClasspathTextLayoutPeer;

import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.util.Map;

/**
 * @author Michael Koch
 */
public final class TextLayout implements Cloneable
{
  public static final CaretPolicy DEFAULT_CARET_POLICY = new CaretPolicy ();
  ClasspathTextLayoutPeer peer;

  public static class CaretPolicy
  {
    public CaretPolicy ()
    {
      // Do nothing here.
    }

    public TextHitInfo getStrongCaret (TextHitInfo hit1, TextHitInfo hit2,
                                       TextLayout layout)
    {
      return layout.peer.getStrongCaret(hit1, hit2);
    }
  }

  public TextLayout (AttributedCharacterIterator text, FontRenderContext frc)
  {    
    AttributedString as = new AttributedString (text);
    ClasspathToolkit tk = (ClasspathToolkit)(Toolkit.getDefaultToolkit ());
    peer = tk.getClasspathTextLayoutPeer(as, frc);
  }

  public TextLayout (String string, Font font, FontRenderContext frc) 
  {
    AttributedString as = new AttributedString (string);
    as.addAttribute (TextAttribute.FONT, font);
    ClasspathToolkit tk = (ClasspathToolkit)(Toolkit.getDefaultToolkit ());
    peer = tk.getClasspathTextLayoutPeer(as, frc);
  }

  public TextLayout (String string, Map attributes, FontRenderContext frc)  
  {
    AttributedString as = new AttributedString (string, attributes);
    ClasspathToolkit tk = (ClasspathToolkit)(Toolkit.getDefaultToolkit ());
    peer = tk.getClasspathTextLayoutPeer(as, frc);
  }

  protected Object clone ()
  {
    try
      {
        TextLayout tl = (TextLayout) super.clone ();
        tl.peer = (ClasspathTextLayoutPeer) this.peer.clone();
        return tl;
      }
    catch (CloneNotSupportedException e)
      {
        // This should never occur
        throw new InternalError ();
      }
  }


  public void draw (Graphics2D g2, float x, float y) 
  {
    peer.draw(g2, x, y);
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof TextLayout))
      return false;

    return equals ((TextLayout) obj);
  }

  public boolean equals (TextLayout tl)
  {
    return this.peer.equals(tl.peer);
  }

  public float getAdvance ()
  {
    return peer.getAdvance();
  }

  public float getAscent ()
  {
    return peer.getAscent();
  }

  public byte getBaseline ()
  {
    return peer.getBaseline();
  }

  public float[] getBaselineOffsets ()
  {
    return peer.getBaselineOffsets();
  }

  public Shape getBlackBoxBounds (int firstEndpoint, int secondEndpoint)
  {
    return peer.getBlackBoxBounds(firstEndpoint, secondEndpoint);
  }

  public Rectangle2D getBounds()
  {
    return peer.getBounds();
  }

  public float[] getCaretInfo (TextHitInfo hit)
  {
    return getCaretInfo(hit, getBounds());
  }

  public float[] getCaretInfo (TextHitInfo hit, Rectangle2D bounds)
  {
    return peer.getCaretInfo(hit, bounds);
  }

  public Shape getCaretShape (TextHitInfo hit)
  {
    return getCaretShape(hit, getBounds());
  }

  public Shape getCaretShape (TextHitInfo hit, Rectangle2D bounds)
  {
    return peer.getCaretShape(hit, bounds);
  }

  public Shape[] getCaretShapes (int offset)
  {
    return getCaretShapes(offset, getBounds());
  }

  public Shape[] getCaretShapes (int offset, Rectangle2D bounds)
  {
    return getCaretShapes(offset, getBounds(), DEFAULT_CARET_POLICY);
  }

  public Shape[] getCaretShapes (int offset, Rectangle2D bounds,
                                 TextLayout.CaretPolicy policy)
  {
    return peer.getCaretShapes(offset, bounds, policy);
  }

  public int getCharacterCount ()
  {
    return peer.getCharacterCount();
  }

  public byte getCharacterLevel (int index)
  {
    return peer.getCharacterLevel(index);
  }

  public float getDescent ()
  {
    return peer.getDescent();
  }

  public TextLayout getJustifiedLayout (float justificationWidth)
  {
    return peer.getJustifiedLayout(justificationWidth);
  }

  public float getLeading ()
  {
    return peer.getLeading();
  }

  public Shape getLogicalHighlightShape (int firstEndpoint, int secondEndpoint)
  {
    return getLogicalHighlightShape (firstEndpoint, secondEndpoint, getBounds());
  }

  public Shape getLogicalHighlightShape (int firstEndpoint, int secondEndpoint,
                                         Rectangle2D bounds)
  {
    return peer.getLogicalHighlightShape(firstEndpoint, secondEndpoint, bounds);
  }

  public int[] getLogicalRangesForVisualSelection (TextHitInfo firstEndpoint,
                                                   TextHitInfo secondEndpoint)
  {
    return peer.getLogicalRangesForVisualSelection(firstEndpoint, secondEndpoint);
  }

  public TextHitInfo getNextLeftHit (int offset)
  {
    return getNextLeftHit(offset, DEFAULT_CARET_POLICY);
  }

  public TextHitInfo getNextLeftHit (int offset, TextLayout.CaretPolicy policy)
  {
    return peer.getNextLeftHit(offset, policy);
  }

  public TextHitInfo getNextLeftHit (TextHitInfo hit)
  {
    return getNextLeftHit(hit.getCharIndex());
  }

  public TextHitInfo getNextRightHit (int offset)
  {
    return getNextRightHit(offset, DEFAULT_CARET_POLICY);
  }

  public TextHitInfo getNextRightHit (int offset, TextLayout.CaretPolicy policy)
  {
    return peer.getNextRightHit(offset, policy);
  }

  public TextHitInfo getNextRightHit (TextHitInfo hit)
  {
    return getNextRightHit(hit.getCharIndex());
  }

  public Shape getOutline (AffineTransform tx)
  {
    return peer.getOutline(tx);
  }

  public float getVisibleAdvance ()
  {
    return peer.getVisibleAdvance();
  }

  public Shape getVisualHighlightShape (TextHitInfo firstEndpoint,
                                        TextHitInfo secondEndpoint)
  {
    return getVisualHighlightShape(firstEndpoint, secondEndpoint, getBounds());
  }

  public Shape getVisualHighlightShape (TextHitInfo firstEndpoint,
                                        TextHitInfo secondEndpoint,
                                        Rectangle2D bounds)
  {
    return peer.getVisualHighlightShape(firstEndpoint, secondEndpoint, bounds);
  }

  public TextHitInfo getVisualOtherHit (TextHitInfo hit)
  {
    return peer.getVisualOtherHit(hit);
  }

  protected void handleJustify (float justificationWidth)
  {
    peer.handleJustify(justificationWidth);
  }

  public int hashCode ()
  {
    return peer.hashCode();
  }

  public TextHitInfo hitTestChar (float x, float y)
  {
    return hitTestChar(x, y, getBounds());
  }

  public TextHitInfo hitTestChar (float x, float y, Rectangle2D bounds)
  {
    return peer.hitTestChar(x, y, bounds);
  }

  public boolean isLeftToRight ()
  {
    return peer.isLeftToRight();
  }

  public boolean isVertical ()
  {
    return peer.isVertical();
  }

  public String toString ()
  {
    return peer.toString();
  }
}
