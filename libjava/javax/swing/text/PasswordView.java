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


package javax.swing.text;

import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPasswordField;

public class PasswordView extends FieldView
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
    // FIXME: Throw BadLocationException somehow.

    // Update font metrics.
    updateMetrics();
    
    // Get echo character.
    char ch = getEchoChar();
    
    // Set color for selected text.
    g.setColor(selectedColor);
    g.setColor(Color.BLACK);

    // Initialize buffer for faster drawing of all characters.
    int len = p1 - p0;
    char[] buffer = new char[len];
    for (int index = 0; index < len; ++index)
      buffer[index] = ch;
    
    // Draw echo charaters.
    g.drawChars(buffer, 0, len, x, y);

    // Return new x position right of all drawn characters.
    return x + len * metrics.charWidth(ch);
  }

  /**
   * Draws unselected text at a given position.
   *
   * @param g the <code>Graphics</code> object to draw to
   * @param x the x-position
   * @param y the y-position
   * @param p0 the position of the first character to draw
   * @param p1 the position of the first character not to draw
   *
   * @return the next x position right of the drawn character
   */
  protected int drawUnselectedText(Graphics g, int x, int y, int p0, int p1)
    throws BadLocationException
  {
    // FIXME: Throw BadLocationException somehow.

    // Update font metrics.
    updateMetrics();
    
    // Get echo character.
    char ch = getEchoChar();
    
    // Set color for unselected text.
    g.setColor(unselectedColor);
    g.setColor(Color.BLACK);

    // Initialize buffer for faster drawing of all characters.
    int len = p1 - p0;
    char[] buffer = new char[len];
    for (int index = 0; index < len; ++index)
      buffer[index] = ch;
    
    // Draw echo charaters.
    g.drawChars(buffer, 0, len, x, y);

    // Return new x position right of all drawn characters.
    return x + len * metrics.charWidth(ch);
  }
}

