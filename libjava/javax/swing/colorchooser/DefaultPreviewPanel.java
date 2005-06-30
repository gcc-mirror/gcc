/* DefaultPreviewPanel.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package javax.swing.colorchooser;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;

import javax.swing.JColorChooser;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;

/**
 * This is the default preview panel for the JColorChooser. The default
 * preview panel is responsible for displaying the currently selected color
 * of the JColorChooser.
 */
class DefaultPreviewPanel extends JPanel
{
  /**
   * This is the border around the preview panel.
   */
  class PreviewBorder implements Border
  {
    /** This is the value of the top, bottom, top, and right inset. */
    private static final int edge = 20;

    /**
     * This is the distance from the top left corner of the border to the
     * text.
     */
    private static final int lead = 5;

    /** This is the horizontal gap between the text and the border. */
    private static final int gap = 3;

    /**
     * This method returns the border insets for the given Component.
     *
     * @param c The Component to retrieve insets for.
     *
     * @return The insets for the given Component.
     */
    public Insets getBorderInsets(Component c)
    {
      return new Insets(edge, edge, edge, edge);
    }

    /**
     * This method returns whether the border is responsible for painting its
     * own background.
     *
     * @return Whether the border is responsible for painting its own
     *         background.
     */
    public boolean isBorderOpaque()
    {
      return true;
    }

    /**
     * This method paints the border for the given component with the graphics
     * object using the given properties.
     *
     * @param c The Component to paint the border for.
     * @param g The Graphics object to paint with.
     * @param x The x location to paint at.
     * @param y The y location to paint at.
     * @param width The width of the component.
     * @param height The height of the component.
     */
    public void paintBorder(Component c, Graphics g, int x, int y, int width,
                            int height)
    {
      Color saved = g.getColor();
      FontMetrics fm = g.getFontMetrics();

      g.setColor(Color.BLACK);
      g.drawLine(x + edge / 2, y + edge / 2, x + edge / 2,
                 y + height - edge / 2);
      g.drawLine(x + edge / 2, y + height - edge / 2, x + width - edge / 2,
                 y + height - edge / 2);
      g.drawLine(x + width - edge / 2, y + edge / 2, x + width - edge / 2,
                 y + height - edge / 2);
      g.drawLine(x + edge / 2, y + edge / 2, x + edge / 2 + lead, y + edge / 2);

      int strwidth = fm.stringWidth("Preview");

      g.drawString("Preview", x + edge / 2 + lead + gap,
                   y + edge / 2 + fm.getAscent() / 2);

      g.drawLine(x + lead + edge / 2 + strwidth + gap * 2, y + edge / 2,
                 x + width - edge / 2, y + edge / 2);

      g.setColor(saved);
    }
  }

  /** A standard large gap size. */
  private static int largeGap = 6;

  /** A standard small gap size. */
  private static int smallGap = 2;

  /** The size of each side of the square. */
  private static int squareSize = 36;

  /** This padding between the text and the edge of its box. */
  private static int textPadding = 4;

  /** The width of the right most rectangles. */
  private static int rightSideRectWidth = 60;

  /** The sample text. */
  private static String sample = "Sample Text   Sample Text";

  /**
   * Creates a new DefaultPreviewPanel object.
   */
  DefaultPreviewPanel()
  {
    super();
    setBorder(new PreviewBorder());
  }

  /**
   * This method paints the default preview panel with the given Graphics
   * object.
   *
   * @param g The Graphics object.
   */
  public void paint(Graphics g)
  {
    super.paint(g);
    Color currentColor = null;
    JColorChooser chooser = (JColorChooser) SwingUtilities.getAncestorOfClass(JColorChooser.class,
                                                                              this);
    if (chooser != null)
      currentColor = chooser.getColor();

    Color saved = g.getColor();
    Insets insets = getInsets();

    int down = insets.top + squareSize + largeGap;
    int currX = insets.left;

    paintSquare(g, currX, insets.top, Color.WHITE, currentColor, Color.WHITE,
                -1, -1, -1);
    paintSquare(g, currX, down, currentColor, null, null, -1, -1, -1);

    currX += squareSize + largeGap;

    paintSquare(g, currX, insets.top, Color.BLACK, currentColor, Color.WHITE,
                -1, -1, -1);
    paintSquare(g, currX, down, Color.WHITE, currentColor, null, -1, -1, -1);

    currX += squareSize + largeGap;

    paintSquare(g, currX, insets.top, Color.WHITE, currentColor, Color.BLACK,
                -1, -1, -1);
    paintSquare(g, currX, down, Color.BLACK, currentColor, null, -1, -1, -1);

    FontMetrics fm = g.getFontMetrics();
    int strWidth = fm.stringWidth(sample);
    int strHeight = fm.getHeight();

    currX += squareSize + largeGap;

    int boxWidth = 2 * textPadding + strWidth;
    int boxHeight = 2 * textPadding + strHeight;

    int first = insets.top + textPadding;
    int second = insets.top + boxHeight + smallGap;
    int third = insets.top + 2 * (boxHeight + smallGap);

    g.setColor(Color.WHITE);
    g.fillRect(currX, third, boxWidth, boxHeight);

    g.setColor(currentColor);
    g.drawString(sample, currX + textPadding,
                 first + textPadding + fm.getAscent());

    g.fillRect(currX, second, boxWidth, boxHeight);

    g.drawString(sample, currX + textPadding,
                 third + textPadding + fm.getAscent());

    g.setColor(Color.BLACK);
    g.drawString(sample, currX + textPadding,
                 second + textPadding + fm.getAscent());

    currX += boxWidth + largeGap;

    g.setColor(Color.WHITE);
    g.fillRect(currX, insets.top, rightSideRectWidth, squareSize
               + largeGap / 2);

    g.setColor(currentColor);
    g.fillRect(currX, insets.top + squareSize + largeGap / 2,
               rightSideRectWidth, squareSize + largeGap / 2);

    g.setColor(saved);
  }

  /**
   * This method creates and paints a square. The square has two smaller
   * squares inside of it. Each of the three squares has their sizes
   * determined by the size arguments. If the size is not given (by passing
   * in -1), then the size is determined automatically.
   *
   * @param g The Graphics object to paint with.
   * @param x The x location to paint at.
   * @param y The y location to paint at.
   * @param first The color of the first square.
   * @param second The color of the second square.
   * @param third The color of the third square.
   * @param firstSize The size of the first square.
   * @param secondSize The size of the second square.
   * @param thirdSize The size of the third square.
   */
  private void paintSquare(Graphics g, int x, int y, Color first,
                           Color second, Color third, int firstSize,
                           int secondSize, int thirdSize)
  {
    Color saved = g.getColor();
    if (firstSize == -1)
      firstSize = squareSize;
    if (secondSize == -1)
      secondSize = squareSize * 2 / 3;
    if (thirdSize == -1)
      thirdSize = squareSize / 3;
    int secondOffset = (firstSize - secondSize) / 2;
    int thirdOffset = (firstSize - thirdSize) / 2;

    if (first == null)
      return;
    g.setColor(first);
    g.fillRect(x, y, firstSize, firstSize);
    if (second == null)
      return;
    g.setColor(second);
    g.fillRect(x + secondOffset, y + secondOffset, secondSize, secondSize);
    if (third == null)
      return;
    g.setColor(third);
    g.fillRect(x + thirdOffset, y + thirdOffset, thirdSize, thirdSize);

    g.setColor(saved);
  }

  /**
   * This method returns the preferred size of the default preview panel.
   *
   * @return The preferred size of the default preview panel.
   */
  public Dimension getPreferredSize()
  {
    Graphics g = getGraphics();
    FontMetrics fm = g.getFontMetrics();
    g.dispose();

    int strWidth = fm.stringWidth(sample);
    int strHeight = fm.getHeight();

    int h1 = (strHeight + 2 * textPadding) * 3 + 2 * smallGap;
    int h2 = 2 * squareSize + largeGap;

    int height = Math.max(h1, h2);

    int width = 3 * (squareSize + largeGap) + strWidth + 2 * textPadding
                + largeGap + rightSideRectWidth;

    Insets insets = getInsets();

    return new Dimension(width + insets.right + insets.left,
                         height + insets.top + insets.bottom);
  }
}
