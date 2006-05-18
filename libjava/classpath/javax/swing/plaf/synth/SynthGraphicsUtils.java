/* SynthGraphicsUtils.java -- Wrapper for graphics primitives used in Synth
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.swing.plaf.synth;

import gnu.classpath.NotImplementedException;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Rectangle;

import javax.swing.Icon;
import javax.swing.SwingUtilities;

/**
 * Wrapper for graphics primitives used in Synth.
 *
 * @author Roman Kennke (kennke@aicas.com)
 *
 * @since 1.5
 */
public class SynthGraphicsUtils

{
  /**
   * Creates a new <code>SynthGraphicsUtils</code> object.
   */
  public SynthGraphicsUtils()
  {
    // Nothing to do here.
  }

  /**
   * Draws a line from (x1,y1) to (x2,y2).
   *
   * @param ctx the synth context, identifies the region
   * @param paintKey identifies the portion of the component to be painted, may
   *        be <code>null</code>
   * @param g the graphics context to use for painting
   * @param x1 the x coordinate of the start point
   * @param y1 the y coordinate of the start point
   * @param x2 the x coordinate of the end point
   * @param y2 the y coordinate of the end point
   */
  public void drawLine(SynthContext ctx, Object paintKey, Graphics g, int x1,
                       int y1, int x2, int y2)
  {
    // TODO: Correct?
    g.drawLine(x1, y1, x2, y2);
  }

  /**
   * Lays out a label and (if non-null) an icon. The calculated coordinates are
   * then stored in <code>viewR</code>, <code>iconR</code> and
   * <code>textR</code>.
   *   
   * The alignment and position parameters may be one of the alignment or
   * position constants defined in {@link javax.swing.SwingConstants}.
   * 
   * @param ctx the synth context, identifies the current region
   * @param fm the font metrics to use to fetch the text measures
   * @param text the text to lay out, may be <code>null</code>
   * @param icon the icon to lay out, may be <code>null</code>
   * @param hAlign the horizontal alignment of the label
   * @param vAlign the vertical alignment of the label
   * @param hTextPos the horizontal text position
   * @param vTextPos the vertical text position
   * @param viewR the view rectangle (return parameter)
   * @param iconR the icon rectangle (return parameter)
   * @param textR the text rectangle (return parameter)
   * @param iconTextGap the gap between text and label
   *
   * @return the label text, may be shortened
   */
  public String layoutText(SynthContext ctx, FontMetrics fm, String text,
                           Icon icon, int hAlign, int vAlign, int hTextPos,
                           int vTextPos, Rectangle viewR, Rectangle iconR,
                           Rectangle textR, int iconTextGap)
  {
    return SwingUtilities.layoutCompoundLabel(fm, text, icon, vAlign, hAlign,
                                              vTextPos, hTextPos, viewR, iconR,
                                              textR, iconTextGap);
  }

  /**
   * Returns the width of the string <code>text</code> for the specified font
   * and font metrics.
   * 
   * @param ctx identifies the current region
   * @param font the font
   * @param fm the font metrics to use
   * @param text the text to be measured
   *
   * @return the width of the string <code>text</code> for the specified font
   *         and font metrics
   */
  public int computeStringWidth(SynthContext ctx, Font font, FontMetrics fm,
                                String text)
  {
    return fm.stringWidth(text);
  }

  /**
   * Calculates the minimums size that is needed to render the label with
   * <code>text</code> and <code>icon</code> correctly.
   *
   * @param ctx identifies the current region
   * @param font the font to use
   * @param text the label text
   * @param icon the label icon
   * @param hAlign the horizontal alignment
   * @param vAlign the vertical alignment
   * @param hTextPosition the horizontal text position
   * @param vTextPosition the vertical text position
   * @param iconTextGap the gap between icon and text
   * @param mnemonicIndex index to the mnemonic character within
   *        <code>text</code>
   *
   * @return the minimums size that is needed to render the label with
   *         <code>text</code> and <code>icon</code> correctly
   */
  public Dimension getMinimumSize(SynthContext ctx, Font font, String text,
                                  Icon icon, int hAlign, int vAlign,
                                  int hTextPosition,int vTextPosition,
                                  int iconTextGap,int mnemonicIndex)
    throws NotImplementedException
  {
    // FIXME: Implement this correctly.
    return new Dimension(0, 0);
  }

  /**
   * Calculates the preferred size that is needed to render the label with
   * <code>text</code> and <code>icon</code> correctly.
   *
   * @param ctx identifies the current region
   * @param font the font to use
   * @param text the label text
   * @param icon the label icon
   * @param hAlign the horizontal alignment
   * @param vAlign the vertical alignment
   * @param hTextPosition the horizontal text position
   * @param vTextPosition the vertical text position
   * @param iconTextGap the gap between icon and text
   * @param mnemonicIndex index to the mnemonic character within
   *        <code>text</code>
   *
   * @return the preferred size that is needed to render the label with
   *         <code>text</code> and <code>icon</code> correctly
   */
  public Dimension getPreferredSize(SynthContext ctx, Font font, String text,
                                    Icon icon, int hAlign, int vAlign,
                                    int hTextPosition,int vTextPosition,
                                    int iconTextGap,int mnemonicIndex)
    throws NotImplementedException
  {
    // FIXME: Implement this correctly.
    return new Dimension(0, 0);
  }

  /**
   * Calculates the maximum size that is needed to render the label with
   * <code>text</code> and <code>icon</code> correctly.
   *
   * @param ctx identifies the current region
   * @param font the font to use
   * @param text the label text
   * @param icon the label icon
   * @param hAlign the horizontal alignment
   * @param vAlign the vertical alignment
   * @param hTextPosition the horizontal text position
   * @param vTextPosition the vertical text position
   * @param iconTextGap the gap between icon and text
   * @param mnemonicIndex index to the mnemonic character within
   *        <code>text</code>
   *
   * @return the maximum size that is needed to render the label with
   *         <code>text</code> and <code>icon</code> correctly
   */
  public Dimension getMaximumSize(SynthContext ctx, Font font, String text,
                                  Icon icon, int hAlign, int vAlign,
                                  int hTextPosition,int vTextPosition,
                                  int iconTextGap,int mnemonicIndex)
    throws NotImplementedException
  {
    // FIXME: Implement this correctly.
    return new Dimension(0, 0);
  }

  /**
   * Returns the maximum character height of the font from the component of the
   * passed in <code>context</code>.
   *
   * @param context identifies the current component and region
   *
   * @return the maximum character height of the font from the component of the
   *         passed in <code>context</code>
   */
  public int getMaximumCharHeight(SynthContext context)
  {
    Component comp = context.getComponent();
    Font font = comp.getFont();
    return comp.getFontMetrics(font).getHeight();
  }

  /**
   * Renders the specified <code>text</code> within the <code>bounds</code>.
   *
   * @param ctx identifies the component and region
   * @param g the graphics context for drawing the tetx
   * @param text the text to be rendered
   * @param bounds the bounds within which the text should be rendered
   * @param mnemonicIndex the index of the mnemonic character within
   *        <code>text</code>
   */
  public void paintText(SynthContext ctx, Graphics g, String text,
                        Rectangle bounds, int mnemonicIndex)
  {
    // FIXME: This is very primitive and should be improved to paint the
    // mnemonic char.
    g.drawString(text, bounds.x, bounds.y);
  }

  /**
   * Renders the specified <code>text</code> at the specified location.
   *
   * @param ctx identifies the component and region
   * @param g the graphics context for drawing the tetx
   * @param text the text to be rendered
   * @param x the X location where the text should be rendered
   * @param y the Y location where the text should be rendered
   * @param mnemonicIndex the index of the mnemonic character within
   *        <code>text</code>
   */
  public void paintText(SynthContext ctx, Graphics g, String text,
                        int x, int y, int mnemonicIndex)
  {
    // FIXME: This is very primitive and should be improved to paint the
    // mnemonic char.
    g.drawString(text, x, y);
  }

  public void paintText(SynthContext ctx, Graphics g, String text, Icon icon,
                        int hAlign, int vAlign, int hTextPosition,
                        int vTextPosition, int iconTextGap, int mnemonicIndex,
                        int textOffset)
    throws NotImplementedException
  {
    // FIXME: Implement this correctly.
  }
}
