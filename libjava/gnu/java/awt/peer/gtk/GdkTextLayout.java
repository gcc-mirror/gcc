/* GdkTextLayout.java
   Copyright (C) 2003, 2005  Free Software Foundation, Inc.

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

import gnu.classpath.Configuration;
import gnu.java.awt.peer.ClasspathTextLayoutPeer;

import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphMetrics;
import java.awt.font.GlyphVector;
import java.awt.font.TextAttribute;
import java.awt.font.TextHitInfo;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.Rectangle2D;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.text.CharacterIterator;

/**
 * This is an implementation of the text layout peer interface which
 * delegates all the hard work to pango.
 *
 * @author Graydon Hoare
 */

public class GdkTextLayout 
  implements ClasspathTextLayoutPeer
{
  // native side, plumbing, etc.
  static 
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
        System.loadLibrary("gtkpeer");
      }
    initStaticState ();
  }
  private native void setText(String str);
  private native void getExtents(double[] inkExtents,
                                 double[] logExtents);
  private native void indexToPos(int idx, double[] pos);
  private native void initState ();
  private native void dispose ();
  static native void initStaticState();
  private final int native_state = GtkGenericPeer.getUniqueInteger ();
  protected void finalize ()
  {
    dispose ();
  }

  // we hold on to these to make sure we can render when presented
  // with non-GdkGraphics2D paint targets
  private AttributedString attributedString;
  private FontRenderContext fontRenderContext;

  public GdkTextLayout(AttributedString str, FontRenderContext frc)
  {
    initState();
    attributedString = str;
    fontRenderContext = frc;
  }

  protected class CharacterIteratorProxy 
    implements CharacterIterator
  {
    public CharacterIterator target;
    public int begin;
    public int limit;
    public int index;

    public CharacterIteratorProxy (CharacterIterator ci)
    {
      target = ci;
    }

    public int getBeginIndex ()
    {
      return begin;
    }

    public int getEndIndex ()
    {
      return limit;
    }

    public int getIndex ()
    {
      return index;
    }

    public char setIndex (int idx) 
      throws IllegalArgumentException
    {
      if (idx < begin || idx >= limit)
        throw new IllegalArgumentException ();
      char ch = target.setIndex (idx);
      index = idx;
      return ch;
    }

    public char first ()
    {
      int save = target.getIndex ();
      char ch = target.setIndex (begin);
      target.setIndex (save);
      return ch;
    }

    public char last ()
    {
      if (begin == limit)
        return this.first ();

      int save = target.getIndex ();
      char ch = target.setIndex (limit - 1);
      target.setIndex (save);
      return ch;
    }

    public char current ()
    {
      return target.current();
    }

    public char next ()
    {
      if (index >= limit - 1)
        return CharacterIterator.DONE;
      else
        {
          index++;
          return target.next();
        }
    }

    public char previous ()
    {
      if (index <= begin)
        return CharacterIterator.DONE;
      else
        {
          index--;
          return target.previous ();
        }
    }

    public Object clone ()
    {
      CharacterIteratorProxy cip = new CharacterIteratorProxy (this.target);
      cip.begin = this.begin;
      cip.limit = this.limit;
      cip.index = this.index;
      return cip;
    }
    
  }


  // public side

  public void draw (Graphics2D g2, float x, float y)
  {
    if (g2 instanceof GdkGraphics2D)
      {
        // we share pango structures directly with GdkGraphics2D 
        // when legal
        GdkGraphics2D gg2 = (GdkGraphics2D) g2;
        gg2.drawGdkTextLayout(this, x, y);
      }
    else 
      {
        // falling back to a rather tedious layout algorithm when
        // not legal
        AttributedCharacterIterator ci = attributedString.getIterator ();
        CharacterIteratorProxy proxy = new CharacterIteratorProxy (ci);
        Font defFont = g2.getFont ();

        /* Note: this implementation currently only interprets FONT text
         * attributes. There is a reasonable argument to be made for some
         * attributes being interpreted out here, where we have control of the
         * Graphics2D and can construct or derive new fonts, and some
         * attributes being interpreted by the GlyphVector itself. So far, for
         * all attributes except FONT we do neither.
         */

        for (char c = ci.first ();
             c != CharacterIterator.DONE;
             c = ci.next ())
          {                
            proxy.begin = ci.getIndex ();
            proxy.limit = ci.getRunLimit(TextAttribute.FONT);
            if (proxy.limit <= proxy.begin)
              continue;

            proxy.index = proxy.begin;

            Object fnt = ci.getAttribute(TextAttribute.FONT);
            GlyphVector gv;
            if (fnt instanceof Font)
              gv = ((Font)fnt).createGlyphVector (fontRenderContext, proxy);
            else
              gv = defFont.createGlyphVector (fontRenderContext, proxy);

            g2.drawGlyphVector (gv, x, y);

            int n = gv.getNumGlyphs ();
            for (int i = 0; i < n; ++i)
              {
                GlyphMetrics gm = gv.getGlyphMetrics (i);
                if (gm.getAdvanceX() == gm.getAdvance ())
                  x += gm.getAdvanceX ();
                else
                  y += gm.getAdvanceY ();
              }
          }
      }
  }

  public TextHitInfo getStrongCaret (TextHitInfo hit1, 
                                     TextHitInfo hit2)
  {
    throw new Error("not implemented");
  }

  public byte getBaseline ()
  {
    throw new Error("not implemented");
  }

  public boolean isLeftToRight ()
  {
    throw new Error("not implemented");
  }

  public boolean isVertical ()
  {
    throw new Error("not implemented");
  }

  public float getAdvance ()
  {
    throw new Error("not implemented");
  }

  public float getAscent ()
  {
    throw new Error("not implemented");
  }

  public float getDescent ()
  {
    throw new Error("not implemented");
  }

  public float getLeading ()
  {
    throw new Error("not implemented");
  }

  public int getCharacterCount ()
  {
    throw new Error("not implemented");
  }

  public byte getCharacterLevel (int index)
  {
    throw new Error("not implemented");
  }

  public float[] getBaselineOffsets ()
  {
    throw new Error("not implemented");
  }

  public Shape getBlackBoxBounds (int firstEndpoint, int secondEndpoint)
  {
    throw new Error("not implemented");
  }

  public Rectangle2D getBounds ()
  {
    double[] inkExtents = new double[4];
    double[] logExtents = new double[4];
    getExtents(inkExtents, logExtents);
    return new Rectangle2D.Double(logExtents[0], logExtents[1],
                                  logExtents[2], logExtents[3]);
  }

  public float[] getCaretInfo (TextHitInfo hit, Rectangle2D bounds)
  {
    throw new Error("not implemented");
  }

  public Shape getCaretShape (TextHitInfo hit, Rectangle2D bounds)
  {
    throw new Error("not implemented");
  }

  public Shape[] getCaretShapes (int offset, Rectangle2D bounds,
                                 TextLayout.CaretPolicy policy)
  {
    throw new Error("not implemented");
  }

  public Shape getLogicalHighlightShape (int firstEndpoint, int secondEndpoint,
                                         Rectangle2D bounds)
  {
    AffineTransform at = new AffineTransform();
    GeneralPath gp = new GeneralPath();
    double [] rect = new double[4];
    Rectangle2D tmp = new Rectangle2D.Double();
    for (int i = firstEndpoint; i <= secondEndpoint; ++i)
      {
        indexToPos(i, rect);
        tmp.setRect(rect[0], rect[1], rect[2], rect[3]);
        Rectangle2D.intersect(tmp, bounds, tmp);
        gp.append(tmp.getPathIterator(at), false);
      }
    return gp;
  }

  public int[] getLogicalRangesForVisualSelection (TextHitInfo firstEndpoint,
                                                   TextHitInfo secondEndpoint)
  {
    throw new Error("not implemented");
  }
  
  public TextHitInfo getNextLeftHit (int offset, TextLayout.CaretPolicy policy)
  {
    throw new Error("not implemented");
  }
  public TextHitInfo getNextRightHit (int offset, TextLayout.CaretPolicy policy)
  {
    throw new Error("not implemented");
  }
  public TextHitInfo hitTestChar (float x, float y, Rectangle2D bounds)
  {
    throw new Error("not implemented");
  }
  public TextHitInfo getVisualOtherHit (TextHitInfo hit)
  {
    throw new Error("not implemented");
  }

  public float getVisibleAdvance ()
  {
    throw new Error("not implemented");
  }

  public Shape getOutline (AffineTransform tx)
  {
    throw new Error("not implemented");
  }

  public Shape getVisualHighlightShape (TextHitInfo firstEndpoint,
                                        TextHitInfo secondEndpoint,
                                        Rectangle2D bounds)
  {
    throw new Error("not implemented");
  }

  
  public TextLayout getJustifiedLayout (float justificationWidth)
  {
    throw new Error("not implemented");
  }

  public void handleJustify (float justificationWidth)
  {
    throw new Error("not implemented");
  }

  public Object clone ()
  {
    throw new Error("not implemented");
  }

  public int hashCode ()
  {
    throw new Error("not implemented");
  }

  public boolean equals (ClasspathTextLayoutPeer tl)
  {
    throw new Error("not implemented");
  }

  public String toString ()
  {
    throw new Error("not implemented");
  }

}
