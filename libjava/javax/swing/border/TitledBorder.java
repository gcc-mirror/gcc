/* TitledBorder.java -- 
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


package javax.swing.border;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Shape;

import javax.swing.UIManager;


/**
 * A border that paints a title on top of another border.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class TitledBorder
  extends AbstractBorder
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
         title, DEFAULT_JUSTIFICATION, DEFAULT_POSITION,
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
    this(border, /* title */ "", DEFAULT_JUSTIFICATION, DEFAULT_POSITION,
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
    this(border, title, DEFAULT_JUSTIFICATION, DEFAULT_POSITION,
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
    Measurements mes = getMeasurements(c);
    Font oldFont = g.getFont();
    Color oldColor = g.getColor();

    /**
     * A local helper class for painting the border without changing
     * any pixels inside the rectangle of the title text.
     */
    class BorderPainter
    {
      private Component c;
      private Border b;
      private int x, y, width, height;

      /**
       * Constructs a BorderPainter.
       *
       * @param c the component whose border is being painted.
       * @param b the border object.
       * @param x the x coordinate of the rectangle delimiting the border.
       * @param y the y coordinate of the rectangle delimiting the border.
       * @param width the width of the rectangle delimiting the border.
       * @param height the width of the rectangle delimiting the border.
       */
      public BorderPainter(Component c, Border b,
                           int x, int y, int width, int height)
      {
        this.c = c;
        this.b = b;
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
      }


      /**
       * Paints the entire border.
       */
      public void paint(Graphics g)
      {
        if (b != null)
          b.paintBorder(c, g, x, y, width - 1, height - 1);
      }


      /**
       * Paints the border, clipping the drawing operation to a
       * given rectangular area.
       */
      private void paint(Graphics g,
                         int clipX, int clipY, int clipWidth, int clipHeight)
      {
        Shape oldClip = g.getClip();
        try
        {
          g.clipRect(clipX, clipY, clipWidth, clipHeight);
          paint(g);
        }
        finally
        {
          g.setClip(oldClip);
        }
      }


      /**
       * Paints the border without affecting a given rectangular area.
       * This is used for painting the border without drawing anything
       * underneath the title text.
       *
       * <p>Since we do not want to introduce unnecessary dependencies
       * on Java 2D, we perform the clipping without constructive geometry
       * (provided by java.awt.geom.Area). Instead, the border&#x2019;s
       * bounding rectangle is split into smaller parts, which are then
       * clipped and painted individually.:
       *
       * <p><pre>
       *    +--------------------+          +--------------------+
       *    |                    |          |        1           |
       *    |   +--------+       |          +---+--------+-------+
       *    |   | hole   |       |  |====>  | 2 | hole   |   3   |
       *    |   +--------+       |          |---+--------+-------+
       *    |                    |          |        4           |
       *    +--------------------+          +--------------------+</pre>
       *
       */
      public void paintExcept(Graphics g,
                              int holeX, int holeY, int holeWidth, int holeHeight)
      {
        int stripeHeight;

        stripeHeight = holeY - y;
        if (stripeHeight > 0)
          paint(g, x, y, width, stripeHeight);   // patch #1 in the image above

        stripeHeight = holeHeight;
        if (stripeHeight > 0)
        {
          paint(g, x, holeY, holeX - x, stripeHeight);  // patches #2 and #3
          paint(g, holeX + holeWidth, holeY, width - (holeX + holeWidth), stripeHeight);
        }

        stripeHeight = height - (holeY - y + holeHeight);
        if (stripeHeight > 0)
          paint(g, x, y + height - stripeHeight, width, stripeHeight); // #4
      }
    };

    BorderPainter bp;
    int textX, textY, borderWidth, borderHeight;

    borderWidth = width - (mes.borderSpacing.left + mes.borderSpacing.right);
    borderHeight = height - (mes.borderSpacing.top + mes.borderSpacing.bottom);
    bp = new BorderPainter(c, getBorder(),
                           x + mes.borderSpacing.left, y + mes.borderSpacing.top,
                           borderWidth, borderHeight);

    switch (getRealTitleJustification(c))
    {
    case LEFT:
      textX = x + TEXT_INSET_H;
      break;

    case CENTER:
      textX = x + (borderWidth - mes.textWidth) / 2;
      break;

    case RIGHT:
      textX = x + borderWidth - (mes.textWidth + TEXT_INSET_H);
      break;

    default:
      throw new IllegalStateException();
    }

    switch (titlePosition)
    {
    case ABOVE_TOP:
      textY = y;
      break;

    case TOP:
    case DEFAULT_POSITION:
    default:
      textY = y + mes.borderSpacing.top + mes.borderInsets.top - mes.textAscent;
      break;

    case BELOW_TOP:
      textY = y + mes.borderSpacing.top + mes.borderInsets.top + TEXT_SPACING;
      break;

    case ABOVE_BOTTOM:
      textY = y + height - mes.borderSpacing.bottom - mes.borderInsets.bottom
        - TEXT_SPACING - (mes.textAscent + mes.textDescent);
      break;

    case BOTTOM:
    case BELOW_BOTTOM:
      textY = y + height - (mes.textAscent + mes.textDescent);
      break;
    }

    if (mes.trimmedText == null)
      bp.paint(g);
    else
    {
      try
      {
        g.setFont(mes.font);
        g.setColor(getTitleColor());
        g.drawString(mes.trimmedText, textX, textY + mes.textAscent);
      }
      finally
      {
        g.setFont(oldFont);
        g.setColor(oldColor);
      }
      bp.paintExcept(g, textX - 2, textY,
                     mes.textWidth + 2, mes.textAscent + mes.textDescent);
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
   * @see #getBorderInsets()
   */
  public Insets getBorderInsets(Component c, Insets insets)
  {
    return getMeasurements(c).getContentInsets(insets);
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
      throw new IllegalArgumentException();

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
      throw new IllegalArgumentException();

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
   * @param c the Component for which this TitledBorder consitutes
   *        a border.
   */
  public Dimension getMinimumSize(Component c)
  {
    return getMeasurements(c).getMinimumSize();
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


  /**
   * Returns the horizontal alignment of the title text in relation to
   * the border, mapping the component-dependent alignment constants
   * {@link #LEADING}, {@link #TRAILING} and {@link #DEFAULT_JUSTIFICATION}
   * to the correct value according to the embedded component&#x2019;s
   * orientation.
   *
   * @param c the Component for which this TitledBorder is the border.
   *
   * @return one of the values {@link #LEFT}, {@link #CENTER}, or {@link
   *         #RIGHT}.
   */
  private int getRealTitleJustification(Component c)
  {
    switch (titleJustification)
    {
    case DEFAULT_JUSTIFICATION:
    case LEADING:
      if ((c == null) || c.getComponentOrientation().isLeftToRight())
        return LEFT;
      else
        return RIGHT;

    case TRAILING:
      if ((c == null) || c.getComponentOrientation().isLeftToRight())
        return RIGHT;
      else
        return LEFT;

    default:
      return titleJustification;
    }
  }


  /**
   * Performs various measurements for the current state of this TitledBorder
   * and the given Component.
   */
  private Measurements getMeasurements(Component c)
  {
    Measurements m = new Measurements();
    FontMetrics fmet;

    m.font = getFont(c);
    fmet = c.getFontMetrics(m.font);
    m.border = getBorder();
    if (m.border != null)
      m.borderInsets = m.border.getBorderInsets(c);
    else
      m.borderInsets = new Insets(0, 0, 0, 0);

    if (title != null)
    {
      m.trimmedText = title.trim();
      if (m.trimmedText.length() == 0)
        m.trimmedText = null;
    }
    
    m.textAscent = fmet.getAscent();
    m.textDescent = fmet.getDescent();
    if (m.trimmedText != null)
      m.textWidth = fmet.stringWidth(m.trimmedText) + 3;

    m.edgeSpacing = new Insets(EDGE_SPACING, EDGE_SPACING, EDGE_SPACING, EDGE_SPACING);
    m.borderSpacing = new Insets(0, 0, 0, 0);

    switch (titlePosition)
    {
    case ABOVE_TOP:
      m.borderSpacing.top += m.textAscent + m.textDescent + TEXT_SPACING;
      break;

    case BELOW_TOP:
      m.edgeSpacing.top += m.textAscent + m.textDescent + TEXT_SPACING;
      break;

    case ABOVE_BOTTOM:
      m.edgeSpacing.bottom += m.textAscent + m.textDescent + TEXT_SPACING;
      break;

    case BOTTOM:
      m.edgeSpacing.bottom += Math.max(m.textAscent - m.borderInsets.bottom, 0);
      m.borderSpacing.bottom += m.textDescent;
      break;

    case BELOW_BOTTOM:
      m.borderSpacing.bottom += m.textAscent + m.textDescent + TEXT_SPACING;
      break;

    default:
      m.borderSpacing.top += m.textAscent;
    }

    return m;
  }


  /**
   * A private helper class for holding the result of measuring the
   * distances of a TitledBorder.  While it would be possible to cache
   * these objects, it does not seem to be worth the effort. Note that
   * invalidating the cache would be tricky, especially since there is
   * no notification mechanism that would inform the cache when
   * border has changed, so it would return different insets.
   */
  private static class Measurements
  {
    /**
     * The font used for displaying the title text. Note that it can
     * well be that the TitledBorder&#x2019;s font is <code>null</code>,
     * which means that the font is to be retrieved from the current
     * LookAndFeel. In this case, this <code>font</code> field will
     * contain the result of the retrieval. Therefore, it is safe
     * to assume that his <code>font</code> field will never have
     * a <code>null</code> value.
     */
    Font font;


    /**
     * The number of pixels between the base line and the top of the
     * text box.
     */
    int textAscent;


    /**
     * The number of pixels between the base line and the bottom of
     * the text box.
     */
    int textDescent;


    /**
     * The title text after removing leading and trailing white space
     * characters. If the title consists only of white space, the
     * value of <code>trimmedText</code> will be <code>null</code>.
     */
    String trimmedText;


    /**
     * The width of the trimmed title text in pixels.
     */
    int textWidth;


    /**
     * The border that constitues the interior border
     * underneath the title text.
     */
    Border border;


    /**
     * The distance between the TitledBorder and the interior border.
     */
    Insets borderSpacing;

    
    /**
     * The width of the interior border, as returned by
     * <code>border.getBorderInsets()</code>.
     */
    Insets borderInsets;

    
    /**
     * The distance between the interior border and the nested
     * Component for which this TitledBorder is a border.
     */
    Insets edgeSpacing;


    /**
     * Determines the insets of the nested component when it has a
     * TitledBorder as its border. Used by {@link
     * TitledBorder#getBorderInsets()}.
     *
     * @param i an Insets object for storing the results into, or
     *        <code>null</code> to cause the creation of a
     *        new instance.
     *
     * @return the <code>i</code> object, or a new Insets object
     *         if <code>null</code> was passed for <code>i</code>.
     */
    public Insets getContentInsets(Insets i)
    {
      if (i == null)
        i = new Insets(0, 0, 0, 0);
      i.left = borderSpacing.left + borderInsets.left + edgeSpacing.left;
      i.right = borderSpacing.right + borderInsets.right + edgeSpacing.right;
      i.top = borderSpacing.top + borderInsets.top + edgeSpacing.top;
      i.bottom = borderSpacing.bottom + borderInsets.bottom + edgeSpacing.bottom;
      return i;
    }


    /**
     * Calculates the minimum size needed for displaying the border
     * and its title. Used by {@link TitledBorder#getMiminumSize()}.
     */
    public Dimension getMinimumSize()
    {
      int width;
      Insets insets;

      insets = getContentInsets(null);
      width = Math.max(insets.left + insets.right, textWidth + 2 * TEXT_INSET_H);
      return new Dimension(width, insets.top + insets.bottom);
    }
  }
}
