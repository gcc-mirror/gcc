/* ClasspathTextLayoutPeer.java
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


package gnu.java.awt.peer;

import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.font.TextHitInfo;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;

/**
 * @author Graydon Hoare
 */

public interface ClasspathTextLayoutPeer
{
  TextHitInfo getStrongCaret (TextHitInfo hit1, 
                              TextHitInfo hit2);

  void draw (Graphics2D g2, float x, float y);

  byte getBaseline ();

  boolean isLeftToRight ();
  boolean isVertical ();

  float getAdvance ();
  float getAscent ();
  float getDescent ();
  float getLeading ();

  int getCharacterCount ();
  byte getCharacterLevel (int index);

  float[] getBaselineOffsets ();
  Shape getBlackBoxBounds (int firstEndpoint, int secondEndpoint);
  Rectangle2D getBounds ();

  float[] getCaretInfo (TextHitInfo hit, Rectangle2D bounds);
  Shape getCaretShape (TextHitInfo hit, Rectangle2D bounds);
  Shape[] getCaretShapes (int offset, Rectangle2D bounds,
                          TextLayout.CaretPolicy policy);

  Shape getLogicalHighlightShape (int firstEndpoint, int secondEndpoint,
                                  Rectangle2D bounds);
  int[] getLogicalRangesForVisualSelection (TextHitInfo firstEndpoint,
                                            TextHitInfo secondEndpoint);

  TextHitInfo getNextLeftHit (int offset, TextLayout.CaretPolicy policy);
  TextHitInfo getNextRightHit (int offset, TextLayout.CaretPolicy policy);
  TextHitInfo hitTestChar (float x, float y, Rectangle2D bounds);
  TextHitInfo getVisualOtherHit (TextHitInfo hit);

  float getVisibleAdvance ();
  Shape getOutline (AffineTransform tx);
  Shape getVisualHighlightShape (TextHitInfo firstEndpoint,
                                 TextHitInfo secondEndpoint,
                                 Rectangle2D bounds);

  TextLayout getJustifiedLayout (float justificationWidth);
  void handleJustify (float justificationWidth);

  Object clone ();
  int hashCode ();
  boolean equals (ClasspathTextLayoutPeer tl);
  String toString ();
}
