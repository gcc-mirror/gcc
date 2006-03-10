/* PasswordView.java -- 
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


package javax.swing.text;

import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.JPasswordField;

public class PasswordView
  extends FieldView
{
  /**
   * Buffer for putting the echo char into it and
   * then using it to draw it into the view.
   */
  private char[] oneCharBuffer = new char[1];

  public PasswordView(Element elem)
  {
    super(elem);
  }

  /**
   * Draws one echo character at a given position.
   *
   * @param g the <code>Graphics</code> object to draw to
   * @param x the x-position
   * @param y the y-position
   * @param ch the echo character
   *
   * @return the next x position right of the drawn character
   */
  protected int drawEchoCharacter(Graphics g, int x, int y, char ch)
  {
    // Update font metrics.
    updateMetrics();

    // Draw character.
    oneCharBuffer[0] = ch;
    g.drawChars(oneCharBuffer, 0, 1, x, y);

    // Return new x position right of drawn character.
    return x + metrics.charWidth(ch);
  }

  private char getEchoChar()
  {
    char ch = ((JPasswordField) getContainer()).getEchoChar();

    if (ch == 0)
      ch = '*';

    return ch;
  }

  /**
   * Draws selected text at a given position.
   *
   * @param g the <code>Graphics</code> object to draw to
   * @param x the x-position
   * @param y the y-position
   * @param p0 the position of the first character to draw
   * @param p1 the position of the first character not to draw
   *
   * @return the next x position right of the drawn character
   */
  protected int drawSelectedText(Graphics g, int x, int y, int p0, int p1)
    throws BadLocationException
  {
    // Update font metrics.
    updateMetrics();

    // Get echo character.
    char ch = getEchoChar();

    // Set color for selected text.
    g.setColor(selectedColor);
    g.setColor(Color.BLACK);

    // Draw echo character using drawEchoCharacter() method.
    for (int index = p0; index < p1; ++index)
      x = drawEchoCharacter(g, x, y, ch);
    return x;
  }

  /**
   * Draws unselected text at a given position.
   *
   * @param g the <code>Graphics</code> object to draw to
   * @param x the x-position of the start of the baseline
   * @param y the y-position of the start of the baseline
   * @param p0 the position of the first character to draw
   * @param p1 the position of the first character not to draw
   *
   * @return the next x position right of the drawn character
   */
  protected int drawUnselectedText(Graphics g, int x, int y, int p0, int p1)
    throws BadLocationException
  {
    // Update font metrics.
    updateMetrics();

    // Get echo character.
    char ch = getEchoChar();

    // Set color for unselected text.
    g.setColor(unselectedColor);
    g.setColor(Color.BLACK);

    // Draw echo character using drawEchoCharacter() method.
    for (int index = p0; index < p1; ++index)
      x = drawEchoCharacter(g, x, y, ch);
    return x;
  }

  /**
   * Determines the preferred span for this view along an axis.
   * 
   * @param axis to get the preferred span of
   * @return the preferred span of the axis
   */
  public float getPreferredSpan(int axis)
  {
    if (axis != X_AXIS && axis != Y_AXIS)
      throw new IllegalArgumentException();

    FontMetrics fm = getFontMetrics();

    if (axis == Y_AXIS)
        return fm.getHeight();

    String text;
    Element elem = getElement();

    try
      {
        text = elem.getDocument().getText(elem.getStartOffset(),
                                          elem.getEndOffset());
      }
    catch (BadLocationException e)
      {
        // This should never happen.
        text = "";
      }
    return fm.stringWidth(text);
  }

  /**
   * Provides a mapping from the document model coordinate space to the
   * coordinate space of the view mapped to it.
   *
   * This method is overridden to provide a correct mapping with respect to the
   * echo char and not to the real content.
   *
   * @param pos - the position to convert >= 0
   * @param a - the allocated region to render into
   * @param b - typesafe enumeration to indicate bias to a position in the model.
   * @return the bounding box of the given position
   * @throws BadLocationException if the given position does not 
   * represent a valid location in the associated document
   */
  public Shape modelToView(int pos, Shape a, Position.Bias b)
    throws BadLocationException
  {
    Shape newAlloc = adjustAllocation(a);

    // Ensure metrics are up-to-date.
    updateMetrics();
    
    // Get rectangle of the line containing position.
    int lineIndex = getElement().getElementIndex(pos);
    Rectangle rect = lineToRect(newAlloc, lineIndex);

    // Get the rectangle for position.
    Element line = getElement().getElement(lineIndex);
    int lineStart = line.getStartOffset();
    Segment segment = getLineBuffer();
    segment.array = new char[pos - lineStart];
    char echoChar = getEchoChar();
    for (int i = 0; i < segment.array.length; ++i)
      segment.array[i] = echoChar;
    segment.offset = 0;
    segment.count = segment.array.length;

    int xoffset = Utilities.getTabbedTextWidth(segment, metrics, rect.x,
                           this, lineStart);

    // Calc the real rectangle.
    rect.x += xoffset;
    rect.width = 1;
    rect.height = metrics.getHeight();

    return rect;
  }

  /**
   * Provides a mapping from the view coordinate space to the logical 
   * coordinate space of the model.
   * 
   * @param fx - the X coordinate >= 0.0f
   * @param fy - the Y coordinate >= 0.0f
   * @param a - the allocated region to render into 
   * @param bias - typesafe enumeration to indicate bias to a position in the model.
   * @return the location within the model that best represents 
   * the given point in the view
   * 
   */
  public int viewToModel(float fx, float fy, Shape a, Position.Bias[] bias)
  {
    // FIXME: This only provides a view->model mapping for the real text
    // content and does not respect the echo char.
    return super.viewToModel(fx, fy, a, bias);
  }
}
