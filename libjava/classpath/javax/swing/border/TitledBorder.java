/* TitledBorder.java -- 
   Copyright (C) 2003, 2004, 2005, 2006,  Free Software Foundation, Inc.

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


package javax.swing.border;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;

import javax.swing.SwingUtilities;
import javax.swing.UIManager;


/**
 * A border that paints a title on top of another border.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class TitledBorder extends AbstractBorder
{
  /**
   * A value for the <code>titlePosition</code> property that vertically
   * positions the title text at the default vertical position, which
   * is in the middle of the top line of the border.
   *
   * @see #getTitlePosition()
   * @see #setTitlePosition(int)
   */
  public static final int DEFAULT_POSITION = 0;


  /**
   * A value for the <code>titlePosition</code> property that vertically
   * positions the title text above the top line of the border.
   *
   * @see #getTitlePosition()
   * @see #setTitlePosition(int)
   */
  public static final int ABOVE_TOP = 1;


  /**
   * A value for the <code>titlePosition</code> property that vertically
   * positions the title text at the middle of the top line
   * of the border.
   *
   * @see #getTitlePosition()
   * @see #setTitlePosition(int)
   */
  public static final int TOP = 2;


  /**
   * A value for the <code>titlePosition</code> property that vertically
   * positions the title text below the top line of the border.
   *
   * @see #getTitlePosition()
   * @see #setTitlePosition(int)
   */
  public static final int BELOW_TOP = 3;


  /**
   * A value for the <code>titlePosition</code> property that vertically
   * positions the title text above the bottom line of the border.
   *
   * @see #getTitlePosition()
   * @see #setTitlePosition(int)
   */
  public static final int ABOVE_BOTTOM = 4;


  /**
   * A value for the <code>titlePosition</code> property that vertically
   * positions the title text at the center of the bottom line
   * of the border.
   *
   * @see #getTitlePosition()
   * @see #setTitlePosition(int)
   */
  public static final int BOTTOM = 5;


  /**
   * A value for the <code>titlePosition</code> property that vertically
   * positions the title text below the bottom line of the border.
   *
   * @see #getTitlePosition()
   * @see #setTitlePosition(int)
   */
  public static final int BELOW_BOTTOM = 6;


  /**
   * A value for the <code>titleJustification</code> property that
   * horizontally aligns the title text with either the left or the
   * right edge of the border, depending on the orientation of the
   * component nested into the border. If the component orientation
   * is left-to-right, the title text is aligned with the left edge;
   * otherwise, it is aligned with the right edge.  This is the same
   * behavior as with {@link #LEADING}.
   *
   * @see #getTitleJustification()
   * @see #setTitleJustification(int)
   * @see java.awt.ComponentOrientation#isLeftToRight()
   */
  public static final int DEFAULT_JUSTIFICATION = 0;


  /**
   * A value for the <code>titleJustification</code> property that
   * horizontally aligns the title text with the left-hand edge of
   * the border.
   *
   * @see #getTitleJustification()
   * @see #setTitleJustification(int)
   */
  public static final int LEFT = 1;


  /**
   * A value for the <code>titleJustification</code> property that
   * horizontally aligns the title text with the center of the border.
   *
   * @see #getTitleJustification()
   * @see #setTitleJustification(int)
   */
  public static final int CENTER = 2;


  /**
   * A value for the <code>titleJustification</code> property that
   * horizontally aligns the title text with the right-hand edge of
   * the border.
   *
   * @see #getTitleJustification()
   * @see #setTitleJustification(int)
   */
  public static final int RIGHT = 3;


  /**
   * A value for the <code>titleJustification</code> property that
   * horizontally aligns the title text with either the left or the
   * right edge of the border, depending on the orientation of the
   * component nested into the border. If the component orientation
   * is left-to-right, the title text is aligned with the left edge;
   * otherwise, it is aligned with the right edge. This is the same
   * behavior as with {@link #DEFAULT_JUSTIFICATION}.
   *
   * @see #getTitleJustification()
   * @see #setTitleJustification(int)
   * @see java.awt.ComponentOrientation#isLeftToRight()
   */
  public static final int LEADING = 4;


  /**
   * A value for the <code>titleJustification</code> property that
   * horizontally aligns the title text with either the right or the
   * left edge of the border, depending on the orientation of the
   * component nested into the border. If the component orientation
   * is left-to-right, the title text is aligned with the right edge;
   * otherwise, it is aligned with the left edge.
   *
   * @see #getTitleJustification()
   * @see #setTitleJustification(int)
   * @see java.awt.ComponentOrientation#isLeftToRight()
   */
  public static final int TRAILING = 5;


  /**
   * The number of pixels between the inside of {@link #border}
   * and the bordered component.
   */
  protected static final int EDGE_SPACING = 2;


  /**
   * The number of pixels between the outside of this TitledBorder
   * and the beginning (if left-aligned) or end (if right-aligned)
   * of the title text.
   */
  protected static final int TEXT_INSET_H = 5;


  /**
   * The number of pixels between the title text and {@link #border}.
   * This value is only relevant if the title text does not intersect
   * {@link #border}. No intersection occurs if {@link #titlePosition}
   * is one of {@link #ABOVE_TOP}, {@link #BELOW_TOP}, {@link #ABOVE_BOTTOM},
   * or {@link #BELOW_BOTTOM}.
   */
  protected static final int TEXT_SPACING = 2;


  /**
   * Determined using the <code>serialver</code> tool of Apple/Sun JDK 1.3.1
   * on MacOS X 10.1.5.
   */
  static final long serialVersionUID = 8012999415147721601L;
  

  /**
   * The title, or <code>null</code> to display no title.
   */
  protected String title;


  /**
   * The border underneath the title. If this value is
   * <code>null</code>, the border will be retrieved from the {@link
   * javax.swing.UIManager}&#x2019;s defaults table using the key
   * <code>TitledBorder.border</code>.
   */
  protected Border border;

  
  /**
   * The vertical position of the title text relative to the border,
   * which is one of {@link #ABOVE_TOP}, {@link #TOP}, {@link
   * #BELOW_TOP}, {@link #ABOVE_BOTTOM}, {@link #BOTTOM}, {@link
   * #BELOW_BOTTOM}, or {@link #DEFAULT_POSITION}.
   */
  protected int titlePosition;


  /**
   * The horizontal alignment of the title text in relation to the
   * border, which is one of {@link #LEFT}, {@link #CENTER}, {@link
   * #RIGHT}, {@link #LEADING}, {@link #TRAILING}, or {@link
   * #DEFAULT_JUSTIFICATION}.
   */
  protected int titleJustification;


  /**
   * The font for displaying the title text. If this value is
   * <code>null</code>, the font will be retrieved from the {@link
   * javax.swing.UIManager}&#x2019;s defaults table using the key
   * <code>TitledBorder.font</code>.
   */
  protected Font titleFont;


  /**
   * The color for displaying the title text. If this value is
   * <code>null</code>, the color will be retrieved from the {@link
   * javax.swing.UIManager}&#x2019;s defaults table using the key
   * <code>TitledBorder.titleColor</code>.
   */
  protected Color titleColor;


  /**
   * Constructs a TitledBorder given the text of its title.
   *
   * @param title the title text, or <code>null</code> to use no title text.
   */
  public TitledBorder(String title)
  {
    this(/* border */ null,
         title, LEADING, TOP,
         /* titleFont */ null, /* titleColor */ null);
  }


  /**
   * Constructs an initially untitled TitledBorder given another border.
   *
   * @param border the border underneath the title, or <code>null</code>
   *        to use a default from the current look and feel.
   */
  public TitledBorder(Border border)
  {
    this(border, /* title */ "", LEADING, TOP,
         /* titleFont */ null, /* titleColor */ null);
  }
  

  /**
   * Constructs a TitledBorder given its border and title text.
   *
   * @param border the border underneath the title, or <code>null</code>
   *        to use a default from the current look and feel.
   *
   * @param title the title text, or <code>null</code> to use no title
   *        text.
   */
  public TitledBorder(Border border, String title)
  {
    this(border, title, LEADING, TOP,
         /* titleFont */ null, /* titleColor */ null);
  }
  

  /**
   * Constructs a TitledBorder given its border, title text, horizontal
   * alignment, and vertical position.
   *
   * @param border the border underneath the title, or <code>null</code>
   *        to use a default from the current look and feel.
   *
   * @param title the title text, or <code>null</code> to use no title
   *        text.
   *
   * @param titleJustification the horizontal alignment of the title
   *        text in relation to the border. The value must be one of
   *        {@link #LEFT}, {@link #CENTER}, {@link #RIGHT}, {@link #LEADING},
   *        {@link #TRAILING}, or {@link #DEFAULT_JUSTIFICATION}.
   
   * @param titlePosition the vertical position of the title text
   *        in relation to the border. The value must be one of
   *        {@link #ABOVE_TOP}, {@link #TOP}, {@link #BELOW_TOP},
   *        {@link #ABOVE_BOTTOM}, {@link #BOTTOM}, {@link #BELOW_BOTTOM},
   *        or {@link #DEFAULT_POSITION}.
   *
   * @throws IllegalArgumentException if <code>titleJustification</code>
   *         or <code>titlePosition</code> have an unsupported value.
   */
  public TitledBorder(Border border, String title, int titleJustification,
                      int titlePosition)
  {
    this(border, title, titleJustification, titlePosition,
         /* titleFont */ null, /* titleColor */ null);
  }
  

  /**
   * Constructs a TitledBorder given its border, title text, horizontal
   * alignment, vertical position, and font.
   *
   * @param border the border underneath the title, or <code>null</code>
   *        to use a default from the current look and feel.
   *
   * @param title the title text, or <code>null</code> to use no title
   *        text.
   *
   * @param titleJustification the horizontal alignment of the title
   *        text in relation to the border. The value must be one of
   *        {@link #LEFT}, {@link #CENTER}, {@link #RIGHT}, {@link #LEADING},
   *        {@link #TRAILING}, or {@link #DEFAULT_JUSTIFICATION}.
   *
   * @param titlePosition the vertical position of the title text
   *        in relation to the border. The value must be one of
   *        {@link #ABOVE_TOP}, {@link #TOP}, {@link #BELOW_TOP},
   *        {@link #ABOVE_BOTTOM}, {@link #BOTTOM}, {@link #BELOW_BOTTOM},
   *        or {@link #DEFAULT_POSITION}.
   *
   * @param titleFont the font for the title text, or <code>null</code>
   *        to use a default from the current look and feel.
   *
   * @throws IllegalArgumentException if <code>titleJustification</code>
   *         or <code>titlePosition</code> have an unsupported value.
   */
  public TitledBorder(Border border, String title, int titleJustification,
                      int titlePosition, Font titleFont)
  {
    this(border, title, titleJustification, titlePosition, titleFont,
         /* titleColor */ null);
  }
  

  /**
   * Constructs a TitledBorder given its border, title text, horizontal
   * alignment, vertical position, font, and color.
   *
   * @param border the border underneath the title, or <code>null</code>
   *        to use a default from the current look and feel.
   *
   * @param title the title text, or <code>null</code> to use no title
   *        text.
   *
   * @param titleJustification the horizontal alignment of the title
   *        text in relation to the border. The value must be one of
   *        {@link #LEFT}, {@link #CENTER}, {@link #RIGHT}, {@link #LEADING},
   *        {@link #TRAILING}, or {@link #DEFAULT_JUSTIFICATION}.
   *
   * @param titlePosition the vertical position of the title text
   *        in relation to the border. The value must be one of
   *        {@link #ABOVE_TOP}, {@link #TOP}, {@link #BELOW_TOP},
   *        {@link #ABOVE_BOTTOM}, {@link #BOTTOM}, {@link #BELOW_BOTTOM},
   *        or {@link #DEFAULT_POSITION}.
   *
   * @param titleFont the font for the title text, or <code>null</code>
   *        to use a default from the current look and feel.
   *
   * @param titleColor the color for the title text, or <code>null</code>
   *        to use a default from the current look and feel.
   *
   * @throws IllegalArgumentException if <code>titleJustification</code>
   *         or <code>titlePosition</code> have an unsupported value.
   */
  public TitledBorder(Border border, String title, int titleJustification,
                      int titlePosition, Font titleFont, Color titleColor)
  {
    this.border = border;
    this.title = title;

    /* Invoking the setter methods ensures that the newly constructed
     * TitledBorder has valid property values.
     */
    setTitleJustification(titleJustification);
    setTitlePosition(titlePosition);

    this.titleFont = titleFont;
    this.titleColor = titleColor;
  }
  
  
  /**
   * Paints the border and the title text.
   *
   * @param c the component whose border is to be painted.
   * @param g the graphics for painting.
   * @param x the horizontal position for painting the border.
   * @param y the vertical position for painting the border.
   * @param width the width of the available area for painting the border.
   * @param height the height of the available area for painting the border.
   */
  public void paintBorder(Component c, Graphics  g, 
                          int x, int y, int width, int height)
  {
    Rectangle borderRect = new Rectangle(x + EDGE_SPACING, y + EDGE_SPACING,
                                         width - (EDGE_SPACING * 2),
                                         height - (EDGE_SPACING * 2));
    Point textLoc = new Point();

    // Save color and font.
    Color savedColor = g.getColor();
    Font savedFont = g.getFont();

    // The font metrics.
    Font font = getFont(c);
    g.setFont(font);
    FontMetrics fm = c.getFontMetrics(font);

    layoutBorderWithTitle(c, fm, borderRect, textLoc);
    paintBorderWithTitle(c, g, x, y, width, height, borderRect, textLoc, fm);

    g.setColor(getTitleColor());
    g.drawString(getTitle(), textLoc.x, textLoc.y);
    g.setFont(savedFont);
    g.setColor(savedColor);
  }

  /**
   * Calculates the bounding box of the inner border and the location of the
   * title string.
   *
   * @param c the component on which to paint the border
   * @param fm the font metrics
   * @param borderRect output parameter, holds the bounding box of the inner
   *        border on method exit
   * @param textLoc output parameter, holds the location of the title text
   *        on method exit
   */
  private void layoutBorderWithTitle(Component c, FontMetrics fm,
                                     Rectangle borderRect,
                                     Point textLoc)
  {
    Border b = getBorder();

    // The font metrics.
    int fontHeight = fm.getHeight();
    int fontDescent = fm.getDescent();
    int fontAscent = fm.getAscent();
    int titleWidth = fm.stringWidth(getTitle());

    // The base insets.
    Insets insets;
    if (b == null)
      insets = new Insets(0, 0, 0, 0);
    else
      insets = b.getBorderInsets(c);

    // The offset of the border rectangle, dependend on the title placement.
    int offset;

    // Layout border and text vertically.
    int titlePosition = getTitlePosition();
    switch (titlePosition)
    {
      case ABOVE_BOTTOM:
        textLoc.y = borderRect.y + borderRect.height - insets.bottom
                     - fontDescent - TEXT_SPACING;
        break;
      case BOTTOM:
        borderRect.height -= fontHeight / 2;
        textLoc.y = borderRect.y + borderRect.height - fontDescent
                     + (fontAscent + fontDescent - insets.bottom) / 2;
        break;
      case BELOW_BOTTOM:
        borderRect.height -=  fontHeight;
        textLoc.y = borderRect.y + borderRect.height + fontAscent
                     + TEXT_SPACING;
        break;
      case ABOVE_TOP:
        offset = fontAscent + fontDescent
                 + Math.max(EDGE_SPACING, TEXT_SPACING * 2) - EDGE_SPACING;
        borderRect.y += offset;
        borderRect.height -= offset;
        textLoc.y = borderRect.y - (fontDescent + TEXT_SPACING);
        break;
      case BELOW_TOP:
        textLoc.y = borderRect.y + insets.top + fontAscent + TEXT_SPACING;
        break;
      case TOP:
      case DEFAULT_POSITION:
      default:
        offset = Math.max(0, ((fontAscent / 2) + TEXT_SPACING) - EDGE_SPACING);
        borderRect.y += offset;
        borderRect.height -= offset;
        textLoc.y = borderRect.y - fontDescent
                     + (insets.top + fontAscent + fontDescent) / 2;
        break;
    }

    // Layout border and text horizontally.
    int justification = getTitleJustification();
    // Adjust justification for LEADING and TRAILING depending on the direction
    // of the component.
    if (c.getComponentOrientation().isLeftToRight())
      {
        if (justification == LEADING || justification == DEFAULT_JUSTIFICATION)
          justification = LEFT;
        else if (justification == TRAILING)
          justification = RIGHT;
      }
    else
      {
        if (justification == LEADING || justification == DEFAULT_JUSTIFICATION)
          justification = RIGHT;
        else if (justification == TRAILING)
          justification = LEFT;
      }

    switch (justification)
    {
      case CENTER:
        textLoc.x = borderRect.x + (borderRect.width - titleWidth) / 2;
        break;
      case RIGHT:
        textLoc.x = borderRect.x + borderRect.width - titleWidth
                     - TEXT_INSET_H - insets.right;
        break;
      case LEFT:
      default:
        textLoc.x = borderRect.x + TEXT_INSET_H + insets.left;
    }
  }

  /**
   * Paints the border with the title.
   *
   * @param c the component to paint on
   * @param g the graphics context used for paintin
   * @param x the upper left corner of the whole border
   * @param y the upper left corner of the whole border
   * @param width the width of the whole border
   * @param height the width of the whole border
   * @param borderRect the bounding box of the inner border
   * @param textLoc the location of the border title
   * @param fm the font metrics of the title
   */
  private void paintBorderWithTitle(Component c, Graphics g, int x, int y,
                                    int width, int height,
                                    Rectangle borderRect, Point textLoc,
                                    FontMetrics fm)
  {
    Border b = getBorder();
    int fontDescent = fm.getDescent();
    int fontAscent = fm.getAscent();
    int titleWidth = fm.stringWidth(getTitle());

    if (b != null)
      {
        // Paint border in segments, when the title is painted above the
        // border.
        if (((titlePosition == TOP || titlePosition == DEFAULT_POSITION)
            && (borderRect.y > textLoc.y - fontAscent))
            || (titlePosition == BOTTOM
                && borderRect.y + borderRect.height < textLoc.y + fontDescent))
          {
            Rectangle clip = new Rectangle();
            Rectangle saved = g.getClipBounds();

            // Paint border left from the text.
            clip.setBounds(saved);
            SwingUtilities.computeIntersection(x, y, textLoc.x - x - 1,
                                               height, clip);
            if (! clip.isEmpty())
              {
                g.setClip(clip);
                b.paintBorder(c, g, borderRect.x, borderRect.y,
                              borderRect.width,
                              borderRect.height);
              }
            // Paint border right from the text.
            clip.setBounds(saved);
            SwingUtilities.computeIntersection(textLoc.x + titleWidth + 1, y,
                x + width - (textLoc.x + titleWidth + 1), height, clip);
            if (! clip.isEmpty())
              {
                g.setClip(clip);
                b.paintBorder(c, g, borderRect.x, borderRect.y,
                              borderRect.width,
                              borderRect.height);
              }

            if (titlePosition == TOP || titlePosition == DEFAULT_POSITION)
              {
                // Paint border below the text.
                clip.setBounds(saved);
                SwingUtilities.computeIntersection(textLoc.x - 1,
                                                   textLoc.y + fontDescent,
                                                   titleWidth + 2,
                                                   y + height - textLoc.y - fontDescent,
                                                   clip);
                if (! clip.isEmpty())
                  {
                    g.setClip(clip);
                    b.paintBorder(c, g, borderRect.x, borderRect.y,
                                  borderRect.width,
                                  borderRect.height);
                  }
	                
              }
            else
              {
                // Paint border above the text.
                clip.setBounds(saved);
                SwingUtilities.computeIntersection(textLoc.x - 1, y,
                                                   titleWidth + 2,
                                                   textLoc.y - fontDescent - y,
                                                   clip);
                if (! clip.isEmpty())
                  {
                    g.setClip(clip);
                    b.paintBorder(c, g, borderRect.x, borderRect.y,
                                  borderRect.width,
                                  borderRect.height);
                  }
	                
              }
            g.setClip(saved);
          }
        else
          {
            b.paintBorder(c, g, borderRect.x, borderRect.y, borderRect.width,
                          borderRect.height);
          }
      }
  }

  /**
   * Measures the width of this border.
   *
   * @param c the component whose border is to be measured.
   *
   * @return an Insets object whose <code>left</code>, <code>right</code>,
   *         <code>top</code> and <code>bottom</code> fields indicate the
   *         width of the border at the respective edge.
   *
   * @see #getBorderInsets(java.awt.Component, java.awt.Insets)
   */
  public Insets getBorderInsets(Component c)
  {
    return getBorderInsets(c, new Insets(0, 0, 0, 0));
  }
  

  /**
   * Measures the width of this border, storing the results into a
   * pre-existing Insets object.
   *
   * @param insets an Insets object for holding the result values.
   *        After invoking this method, the <code>left</code>,
   *        <code>right</code>, <code>top</code> and
   *        <code>bottom</code> fields indicate the width of the
   *        border at the respective edge.
   *
   * @return the same object that was passed for <code>insets</code>.
   *
   * @see #getBorderInsets(Component)
   */
  public Insets getBorderInsets(Component c, Insets insets)
  {
    // Initialize insets with the insets from our border.
    Border border = getBorder();
    if (border != null)
      {
        if (border instanceof AbstractBorder)
          {
            AbstractBorder aBorder = (AbstractBorder) border;
            aBorder.getBorderInsets(c, insets);
          }
        else
          {
            Insets i = border.getBorderInsets(c);
            insets.top = i.top;
            insets.bottom = i.bottom;
            insets.left = i.left;
            insets.right = i.right;
          }
      }
    else
      {
        insets.top = 0;
        insets.bottom = 0;
        insets.left = 0;
        insets.right = 0;
      }

    // Add spacing.
    insets.top += EDGE_SPACING + TEXT_SPACING;
    insets.bottom += EDGE_SPACING + TEXT_SPACING;
    insets.left += EDGE_SPACING + TEXT_SPACING;
    insets.right += EDGE_SPACING + TEXT_SPACING;

    String title = getTitle();
    if (c != null && title != null && !title.equals(""))
      {
        Font font = getFont(c);
        FontMetrics fm = c.getFontMetrics(font);
        int ascent = fm.getAscent();
        int descent = fm.getDescent();
        int height = fm.getHeight();
        switch (getTitlePosition())
        {
          case ABOVE_BOTTOM:
            insets.bottom += ascent + descent + TEXT_SPACING;
            break;
          case BOTTOM:
            insets.bottom += ascent + descent;
            break;
          case BELOW_BOTTOM:
            insets.bottom += height;
            break;
          case ABOVE_TOP:
            insets.top += ascent + descent +
                          Math.max(EDGE_SPACING, TEXT_SPACING * 2)
                          - EDGE_SPACING;
            break;
          case BELOW_TOP:
            insets.top += ascent + descent + TEXT_SPACING;
            break;
          case TOP:
          case DEFAULT_POSITION:
          default:
            insets.top += ascent + descent;
        }
      }
    return insets;
  }
  
  
  /**
   * Returns <code>false</code>, indicating that there are pixels inside
   * the area of this border where the background shines through.
   *
   * @return <code>false</code>.
   */
  public boolean isBorderOpaque()
  {
    /* Note that the AbstractBorder.isBorderOpaque would also return
     * false, so there is actually no need to override the inherited
     * implementation. However, GNU Classpath strives for exact
     * compatibility with the Sun reference implementation, which
     * overrides isBorderOpaque for unknown reasons.
     */
    return false;
  }


  /**
   * Returns the text of the title.
   *
   * @return the title text, or <code>null</code> if no title is
   *         displayed.
   */
  public String getTitle()
  {
    return title;
  }


  /**
   * Retrieves the border underneath the title. If no border has been
   * set, or if it has been set to<code>null</code>, the current
   * {@link javax.swing.LookAndFeel} will be asked for a border
   * using the key <code>TitledBorder.border</code>.
   *
   * @return a border, or <code>null</code> if the current LookAndFeel
   *         does not provide a border for the key
   *         <code>TitledBorder.border</code>.
   *
   * @see javax.swing.UIManager#getBorder(Object)
   */
  public Border getBorder()
  {
    if (border != null)
      return border;

    return UIManager.getBorder("TitledBorder.border");
  }


  /**
   * Returns the vertical position of the title text in relation
   * to the border.
   *
   * @return one of the values {@link #ABOVE_TOP}, {@link #TOP},
   *         {@link #BELOW_TOP}, {@link #ABOVE_BOTTOM}, {@link #BOTTOM},
   *         {@link #BELOW_BOTTOM}, or {@link #DEFAULT_POSITION}.
   */
  public int getTitlePosition()
  {
    return titlePosition;
  }


  /**
   * Returns the horizontal alignment of the title text in relation to
   * the border.
   *
   * @return one of the values {@link #LEFT}, {@link #CENTER}, {@link
   *         #RIGHT}, {@link #LEADING}, {@link #TRAILING}, or {@link
   *         #DEFAULT_JUSTIFICATION}.
   */
  public int getTitleJustification()
  {
    return titleJustification;
  }


  /**
   * Retrieves the font for displaying the title text. If no font has
   * been set, or if it has been set to<code>null</code>, the current
   * {@link javax.swing.LookAndFeel} will be asked for a font
   * using the key <code>TitledBorder.font</code>.
   *
   * @return a font, or <code>null</code> if the current LookAndFeel
   *         does not provide a font for the key
   *         <code>TitledBorder.font</code>.
   *
   * @see javax.swing.UIManager#getFont(Object)
   */
  public Font getTitleFont()
  {
    if (titleFont != null)
      return titleFont;

    return UIManager.getFont("TitledBorder.font");
  }


  /**
   * Retrieves the color for displaying the title text. If no color has
   * been set, or if it has been set to<code>null</code>, the current
   * {@link javax.swing.LookAndFeel} will be asked for a color
   * using the key <code>TitledBorder.titleColor</code>.
   *
   * @return a color, or <code>null</code> if the current LookAndFeel
   *         does not provide a color for the key
   *         <code>TitledBorder.titleColor</code>.
   *
   * @see javax.swing.UIManager#getColor(Object)
   */
  public Color getTitleColor()
  {
    if (titleColor != null)
      return titleColor;

    return UIManager.getColor("TitledBorder.titleColor");
  }


  /**
   * Sets the text of the title.
   *
   * @param title the new title text, or <code>null</code> for displaying
   *        no text at all.
   */
  public void setTitle(String title)
  {
    // Swing borders are not JavaBeans, thus no need to fire an event.
    this.title = title;
  }


  /**
   * Sets the border underneath the title.
   *
   * @param border a border, or <code>null</code> to use the
   *        border that is supplied by the current LookAndFeel.
   *
   * @see #getBorder()
   */
  public void setBorder(Border border)
  {
    // Swing borders are not JavaBeans, thus no need to fire an event.
    this.border = border;
  }


  /**
   * Sets the vertical position of the title text in relation
   * to the border.
   *
   * @param titlePosition one of the values {@link #ABOVE_TOP},
   *        {@link #TOP}, {@link #BELOW_TOP}, {@link #ABOVE_BOTTOM},
   *        {@link #BOTTOM}, {@link #BELOW_BOTTOM},
   *        or {@link #DEFAULT_POSITION}.
   *
   * @throws IllegalArgumentException if an unsupported value is passed
   *         for <code>titlePosition</code>.
   */
  public void setTitlePosition(int titlePosition)
  {
    if ((titlePosition < DEFAULT_POSITION) || (titlePosition > BELOW_BOTTOM))
      throw new IllegalArgumentException(titlePosition 
          + " is not a valid title position.");

    // Swing borders are not JavaBeans, thus no need to fire an event.
    this.titlePosition = titlePosition;
  }


  /**
   * Sets the horizontal alignment of the title text in relation to the border.
   *
   * @param titleJustification the new alignment, which must be one of
   *        {@link #LEFT}, {@link #CENTER}, {@link #RIGHT}, {@link #LEADING},
   *        {@link #TRAILING}, or {@link #DEFAULT_JUSTIFICATION}.
   *
   * @throws IllegalArgumentException if an unsupported value is passed
   *         for <code>titleJustification</code>.
   */
  public void setTitleJustification(int titleJustification)
  {
    if ((titleJustification < DEFAULT_JUSTIFICATION)
        || (titleJustification > TRAILING))
      throw new IllegalArgumentException(titleJustification 
          + " is not a valid title justification.");

    // Swing borders are not JavaBeans, thus no need to fire an event.
    this.titleJustification = titleJustification;
  }


  /**
   * Sets the font for displaying the title text.
   *
   * @param titleFont the font, or <code>null</code> to use the font
   *        provided by the current {@link javax.swing.LookAndFeel}.
   *
   * @see #getTitleFont()
   */
  public void setTitleFont(Font titleFont)
  {
    // Swing borders are not JavaBeans, thus no need to fire an event.
    this.titleFont = titleFont;
  }


  /**
   * Sets the color for displaying the title text.
   *
   * @param titleColor the color, or <code>null</code> to use the color
   *        provided by the current {@link javax.swing.LookAndFeel}.
   *
   * @see #getTitleColor()
   */
  public void setTitleColor(Color titleColor)
  {
    // Swing borders are not JavaBeans, thus no need to fire an event.
    this.titleColor = titleColor;
  }


  /**
   * Calculates the minimum size needed for displaying the border
   * and its title.
   *
   * @param c the Component for which this TitledBorder constitutes
   *        a border.
   *        
   * @return The minimum size.
   */
  public Dimension getMinimumSize(Component c)
  {
    Insets i = getBorderInsets(c);
    Dimension minSize = new Dimension(i.left + i.right, i.top + i.bottom);
    Font font = getFont(c);
    FontMetrics fm = c.getFontMetrics(font);
    int titleWidth = fm.stringWidth(getTitle());
    switch (getTitlePosition())
    {
      case ABOVE_TOP:
      case BELOW_BOTTOM:
        minSize.width = Math.max(minSize.width, titleWidth);
        break;
      case BELOW_TOP:
      case ABOVE_BOTTOM:
      case TOP:
      case BOTTOM:
      case DEFAULT_POSITION:
      default:
        minSize.width += titleWidth;
    }
    return minSize;
  }


  /**
   * Returns the font that is used for displaying the title text for
   * a given Component.
   *
   * @param c the Component for which this TitledBorder is the border.
   *
   * @return The font returned by {@link #getTitleFont()}, or a fallback
   *         if {@link #getTitleFont()} returned <code>null</code>.
   */
  protected Font getFont(Component c)
  {
    Font f;

    f = getTitleFont();
    if (f != null)
      return f;

    return new Font("Dialog", Font.PLAIN, 12);
  }

}
