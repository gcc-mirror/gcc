/* GlyphMetrics.java
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

import java.awt.geom.Rectangle2D;

/**
 * @author Michael Koch
 */
public final class GlyphMetrics
{
  public static final byte COMBINING = 2;
  public static final byte COMPONENT = 3;
  public static final byte LIGATURE = 1;
  public static final byte STANDARD = 0;
  public static final byte WHITESPACE = 4;
 
  private boolean horizontal;
  private float advanceX;
  private float advanceY;
  private Rectangle2D bounds;
  private byte glyphType;
  
  public GlyphMetrics (boolean horizontal, float advanceX, float advanceY,
                       Rectangle2D bounds, byte glyphType)
  {
    this.horizontal = horizontal;
    this.advanceX = advanceX;
    this.advanceY = advanceY;
    this.bounds = bounds;
    this.glyphType = glyphType;
  }
  
  public GlyphMetrics (float advance, Rectangle2D bounds, byte glyphType)
  {
    this (true, advance, advance, bounds, glyphType);
  }

  public float getAdvance ()
  {
    return horizontal ? advanceX : advanceY;
  }

  public float getAdvanceX ()
  {
    return advanceX;
  }

  public float getAdvanceY ()
  {
    return advanceY;
  }

  public Rectangle2D getBounds2D ()
  {
    return bounds;
  }

  public float getLSB()
  {
    if (horizontal)
      return (float) bounds.getX();
    return (float) bounds.getY();
  }

  public float getRSB()
  {
    if (horizontal)
      return (float) (advanceX - (bounds.getX() + bounds.getWidth()));
    return (float) (advanceY - (bounds.getY() + bounds.getHeight()));
  }

  public int getType ()
  {
    return glyphType;
  }

  public boolean isCombining ()
  {
    return (glyphType == COMBINING);
  }

  public boolean isComponent ()
  {
    return (glyphType == COMPONENT);
  }

  public boolean isLigature()
  {
    return (glyphType == LIGATURE);
  }

  public boolean isStandard()
  {
    return (glyphType == STANDARD);
  }

  public boolean isWhitespace()
  {
    return (glyphType == WHITESPACE);
  }
}
