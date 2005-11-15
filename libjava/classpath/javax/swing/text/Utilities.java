/* Utilities.java --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.text.BreakIterator;

import javax.swing.SwingConstants;

/**
 * A set of utilities to deal with text. This is used by several other classes
 * inside this package.
 *
 * @author Roman Kennke (roman@ontographics.com)
 */
public class Utilities
{
  /**
   * The length of the char buffer that holds the characters to be drawn.
   */
  private static final int BUF_LENGTH = 64;

  /**
   * Creates a new <code>Utilities</code> object.
   */
  public Utilities()
  {
    // Nothing to be done here.
  }

  /**
   * Draws the given text segment. Contained tabs and newline characters
   * are taken into account. Tabs are expanded using the
   * specified {@link TabExpander}.
   *
   * @param s the text fragment to be drawn.
   * @param x the x position for drawing.
   * @param y the y position for drawing.
   * @param g the {@link Graphics} context for drawing.
   * @param e the {@link TabExpander} which specifies the Tab-expanding
   *     technique.
   * @param startOffset starting offset in the text.
   * @return the x coordinate at the end of the drawn text.
   */
  public static final int drawTabbedText(Segment s, int x, int y, Graphics g,
                                         TabExpander e, int startOffset)
  {
    // This buffers the chars to be drawn.
    char[] buffer = s.array;


    // The current x and y pixel coordinates.
    int pixelX = x;
    int pixelY = y;

    // The font metrics of the current selected font.
    FontMetrics metrics = g.getFontMetrics();
    int ascent = metrics.getAscent();

    int pixelWidth = 0;
    int pos = s.offset;
    int len = 0;

    for (int offset = s.offset; offset < (s.offset + s.count); ++offset)
      {
        char c = buffer[offset];
        if (c == '\t' || c == '\n')
          {
            if (len > 0) {
              g.drawChars(buffer, pos, len, pixelX, pixelY + ascent);            
              pixelX += pixelWidth;
              pixelWidth = 0;
            }
            pos = offset+1;
            len = 0;
          }
        
	switch (c)
	  {
	  case '\t':
	    // In case we have a tab, we just 'jump' over the tab.
	    // When we have no tab expander we just use the width of ' '.
	    if (e != null)
	      pixelX = (int) e.nextTabStop((float) pixelX,
					   startOffset + offset - s.offset);
	    else
	      pixelX += metrics.charWidth(' ');
	    break;
	  case '\n':
	    // In case we have a newline, we must jump to the next line.
	    pixelY += metrics.getHeight();
	    pixelX = x;
	    break;
	  default:
            ++len;
	    pixelWidth += metrics.charWidth(buffer[offset]);
	    break;
	  }
      }

    if (len > 0)
      g.drawChars(buffer, pos, len, pixelX, pixelY + ascent);            
    
    return pixelX;
  }

  /**
   * Determines the width, that the given text <code>s</code> would take
   * if it was printed with the given {@link java.awt.FontMetrics} on the
   * specified screen position.
   * @param s the text fragment
   * @param metrics the font metrics of the font to be used
   * @param x the x coordinate of the point at which drawing should be done
   * @param e the {@link TabExpander} to be used
   * @param startOffset the index in <code>s</code> where to start
   * @returns the width of the given text s. This takes tabs and newlines
   * into account.
   */
  public static final int getTabbedTextWidth(Segment s, FontMetrics metrics,
                                             int x, TabExpander e,
                                             int startOffset)
  {
    // This buffers the chars to be drawn.
    char[] buffer = s.array;

    // The current x coordinate.
    int pixelX = x;

    // The current maximum width.
    int maxWidth = 0;

    for (int offset = s.offset; offset < (s.offset + s.count); ++offset)
      {
	switch (buffer[offset])
	  {
	  case '\t':
	    // In case we have a tab, we just 'jump' over the tab.
	    // When we have no tab expander we just use the width of 'm'.
	    if (e != null)
	      pixelX = (int) e.nextTabStop((float) pixelX,
					   startOffset + offset - s.offset);
	    else
	      pixelX += metrics.charWidth(' ');
	    break;
	  case '\n':
	    // In case we have a newline, we must 'draw'
	    // the buffer and jump on the next line.
	    pixelX += metrics.charWidth(buffer[offset]);
	    maxWidth = Math.max(maxWidth, pixelX - x);
	    pixelX = x;
	    break;
	  default:
	    // Here we draw the char.
	    pixelX += metrics.charWidth(buffer[offset]);
	    break;
	  }
      }

    // Take the last line into account.
    maxWidth = Math.max(maxWidth, pixelX - x);

    return maxWidth;
  }

  /**
   * Provides a facility to map screen coordinates into a model location. For a
   * given text fragment and start location within this fragment, this method
   * determines the model location so that the resulting fragment fits best
   * into the span <code>[x0, x]</code>.
   *
   * The parameter <code>round</code> controls which model location is returned
   * if the view coordinates are on a character: If <code>round</code> is
   * <code>true</code>, then the result is rounded up to the next character, so
   * that the resulting fragment is the smallest fragment that is larger than
   * the specified span. If <code>round</code> is <code>false</code>, then the
   * resulting fragment is the largest fragment that is smaller than the
   * specified span.
   *
   * @param s the text segment
   * @param fm the font metrics to use
   * @param x0 the starting screen location
   * @param x the target screen location at which the requested fragment should
   *        end
   * @param te the tab expander to use; if this is <code>null</code>, TABs are
   *        expanded to one space character
   * @param p0 the starting model location
   * @param round if <code>true</code> round up to the next location, otherwise
   *        round down to the current location
   *
   * @return the model location, so that the resulting fragment fits within the
   *         specified span
   */
  public static final int getTabbedTextOffset(Segment s, FontMetrics fm, int x0,
                                              int x, TabExpander te, int p0,
                                              boolean round)
  {
    // At the end of the for loop, this holds the requested model location
    int pos;
    int currentX = x0;

    for (pos = p0; pos < s.count; pos++)
      {
        char nextChar = s.array[s.offset+pos];
        if (nextChar == 0)
          {
            if (! round)
              pos--;
            break;
          }
        if (nextChar != '\t')
          currentX += fm.charWidth(nextChar);
        else
          {
            if (te == null)
              currentX += fm.charWidth(' ');
            else
              currentX = (int) te.nextTabStop(currentX, pos);
          }
        if (currentX > x)
          {
            if (! round)
              pos--;
            break;
          }
      }
    return pos;
  }

  /**
   * Provides a facility to map screen coordinates into a model location. For a
   * given text fragment and start location within this fragment, this method
   * determines the model location so that the resulting fragment fits best
   * into the span <code>[x0, x]</code>.
   *
   * This method rounds up to the next location, so that the resulting fragment
   * will be the smallest fragment of the text, that is greater than the
   * specified span.
   *
   * @param s the text segment
   * @param fm the font metrics to use
   * @param x0 the starting screen location
   * @param x the target screen location at which the requested fragment should
   *        end
   * @param te the tab expander to use; if this is <code>null</code>, TABs are
   *        expanded to one space character
   * @param p0 the starting model location
   *
   * @return the model location, so that the resulting fragment fits within the
   *         specified span
   */
  public static final int getTabbedTextOffset(Segment s, FontMetrics fm, int x0,
                                              int x, TabExpander te, int p0)
  {
    return getTabbedTextOffset(s, fm, x0, x, te, p0, true);
  }
  
  /**
   * Finds the start of the next word for the given offset.
   * 
   * @param c
   *          the text component
   * @param offs
   *          the offset in the document
   * @return the location in the model of the start of the next word.
   * @throws BadLocationException
   *           if the offset is invalid.
   */
  public static final int getNextWord(JTextComponent c, int offs)
      throws BadLocationException
  {
    if (offs < 0 || offs > (c.getText().length() - 1))
      throw new BadLocationException("invalid offset specified", offs);
    String text = c.getText();
    BreakIterator wb = BreakIterator.getWordInstance();
    wb.setText(text);
    int last = wb.following(offs);
    int current = wb.next();
    while (current != BreakIterator.DONE)
      {
        for (int i = last; i < current; i++)
          {
            // FIXME: Should use isLetter(int) and text.codePointAt(int)
            // instead, but isLetter(int) isn't implemented yet
            if (Character.isLetter(text.charAt(i)))
              return last;
          }
        last = current;
        current = wb.next();
      }
    return BreakIterator.DONE;
  }

  /**
   * Finds the start of the previous word for the given offset.
   * 
   * @param c
   *          the text component
   * @param offs
   *          the offset in the document
   * @return the location in the model of the start of the previous word.
   * @throws BadLocationException
   *           if the offset is invalid.
   */
  public static final int getPreviousWord(JTextComponent c, int offs)
      throws BadLocationException
  {
    if (offs < 0 || offs > (c.getText().length() - 1))
      throw new BadLocationException("invalid offset specified", offs);
    String text = c.getText();
    BreakIterator wb = BreakIterator.getWordInstance();
    wb.setText(text);
    int last = wb.preceding(offs);
    int current = wb.previous();

    while (current != BreakIterator.DONE)
      {
        for (int i = last; i < offs; i++)
          {
            // FIXME: Should use isLetter(int) and text.codePointAt(int)
            // instead, but isLetter(int) isn't implemented yet
            if (Character.isLetter(text.charAt(i)))
              return last;
          }
        last = current;
        current = wb.previous();
      }
    return 0;
  }
  
  /**
   * Finds the start of a word for the given location.
   * @param c the text component
   * @param offs the offset location
   * @return the location of the word beginning
   * @throws BadLocationException if the offset location is invalid
   */
  public static final int getWordStart(JTextComponent c, int offs)
      throws BadLocationException
  {
    if (offs < 0 || offs >= c.getText().length())
      throw new BadLocationException("invalid offset specified", offs);
    
    String text = c.getText();
    BreakIterator wb = BreakIterator.getWordInstance();
    wb.setText(text);
    if (wb.isBoundary(offs))
      return offs;
    return wb.preceding(offs);
  }
  
  /**
   * Finds the end of a word for the given location.
   * @param c the text component
   * @param offs the offset location
   * @return the location of the word end
   * @throws BadLocationException if the offset location is invalid
   */
  public static final int getWordEnd(JTextComponent c, int offs)
      throws BadLocationException
  {
    if (offs < 0 || offs >= c.getText().length())
      throw new BadLocationException("invalid offset specified", offs);
    
    String text = c.getText();
    BreakIterator wb = BreakIterator.getWordInstance();
    wb.setText(text);
    return wb.following(offs);
  }
  
  /**
   * Get the model position of the end of the row that contains the 
   * specified model position.  Return null if the given JTextComponent
   * does not have a size.
   * @param c the JTextComponent
   * @param offs the model position
   * @return the model position of the end of the row containing the given 
   * offset
   * @throws BadLocationException if the offset is invalid
   */
  public static final int getRowEnd(JTextComponent c, int offs)
      throws BadLocationException
  {
    String text = c.getText();
    if (text == null)
      return -1;

    // Do a binary search for the smallest position X > offs
    // such that that character at positino X is not on the same
    // line as the character at position offs
    int high = offs + ((text.length() - 1 - offs) / 2);
    int low = offs;
    int oldHigh = text.length() + 1;
    while (true)
      {
        if (c.modelToView(high).y != c.modelToView(offs).y)
          {
            oldHigh = high;
            high = low + ((high + 1 - low) / 2);
            if (oldHigh == high)
              return high - 1;
          }
        else
          {
            low = high;
            high += ((oldHigh - high) / 2);
            if (low == high)
              return low;
          }
      }
  }
      
  /**
   * Get the model position of the start of the row that contains the specified
   * model position. Return null if the given JTextComponent does not have a
   * size.
   * 
   * @param c the JTextComponent
   * @param offs the model position
   * @return the model position of the start of the row containing the given
   *         offset
   * @throws BadLocationException if the offset is invalid
   */
  public static final int getRowStart(JTextComponent c, int offs)
      throws BadLocationException
  {
    String text = c.getText();
    if (text == null)
      return -1;

    // Do a binary search for the greatest position X < offs
    // such that the character at position X is not on the same
    // row as the character at position offs
    int high = offs;
    int low = 0;
    int oldLow = 0;
    while (true)
      {
        if (c.modelToView(low).y != c.modelToView(offs).y)
          {
            oldLow = low;
            low = high - ((high + 1 - low) / 2);
            if (oldLow == low)
              return low + 1;
          }
        else
          {
            high = low;
            low -= ((low - oldLow) / 2);
            if (low == high)
              return low;
          }
      }
  }
  
  /**
   * Determine where to break the text in the given Segment, attempting to find
   * a word boundary.
   * @param s the Segment that holds the text
   * @param metrics the font metrics used for calculating the break point
   * @param x0 starting view location representing the start of the text
   * @param x the target view location
   * @param e the TabExpander used for expanding tabs (if this is null tabs
   * are expanded to 1 space)
   * @param startOffset the offset in the Document of the start of the text
   * @return the offset at which we should break the text
   */
  public static final int getBreakLocation(Segment s, FontMetrics metrics,
                                           int x0, int x, TabExpander e,
                                           int startOffset)
  {
    int mark = Utilities.getTabbedTextOffset(s, metrics, x0, x, e, startOffset);
    BreakIterator breaker = BreakIterator.getWordInstance();
    breaker.setText(s.toString());
    
    // If mark is equal to the end of the string, just use that position
    if (mark == s.count)
      return mark;
    
    // Try to find a word boundary previous to the mark at which we 
    // can break the text
    int preceding = breaker.preceding(mark + 1);
    
    if (preceding != 0)
      return preceding;
    else
      // If preceding is 0 we couldn't find a suitable word-boundary so
      // just break it on the character boundary
      return mark;
  }

  /**
   * Returns the paragraph element in the text component <code>c</code> at
   * the specified location <code>offset</code>.
   *
   * @param c the text component
   * @param offset the offset of the paragraph element to return
   *
   * @return the paragraph element at <code>offset</code>
   */
  public static final Element getParagraphElement(JTextComponent c, int offset)
  {
    Document doc = c.getDocument();
    Element par = null;
    if (doc instanceof StyledDocument)
      {
        StyledDocument styledDoc = (StyledDocument) doc;
        par = styledDoc.getParagraphElement(offset);
      }
    else
      {
        Element root = c.getDocument().getDefaultRootElement();
        int parIndex = root.getElementIndex(offset);
        par = root.getElement(parIndex);
      }
    return par;
  }

  /**
   * Returns the document position that is closest above to the specified x
   * coordinate in the row containing <code>offset</code>.
   *
   * @param c the text component
   * @param offset the offset
   * @param x the x coordinate
   *
   * @return  the document position that is closest above to the specified x
   *          coordinate in the row containing <code>offset</code>
   *
   * @throws BadLocationException if <code>offset</code> is not a valid offset
   */
  public static final int getPositionAbove(JTextComponent c, int offset, int x)
    throws BadLocationException
  {
    View rootView = c.getUI().getRootView(c);
    Rectangle r = c.modelToView(offset);
    int offs = c.viewToModel(new Point(x, r.y));
    int pos = rootView.getNextVisualPositionFrom(c, offs,
                                                 Position.Bias.Forward,
                                                 SwingConstants.NORTH,
                                                 new Position.Bias[1]);
    return pos;
  }

  /**
   * Returns the document position that is closest below to the specified x
   * coordinate in the row containing <code>offset</code>.
   *
   * @param c the text component
   * @param offset the offset
   * @param x the x coordinate
   *
   * @return  the document position that is closest above to the specified x
   *          coordinate in the row containing <code>offset</code>
   *
   * @throws BadLocationException if <code>offset</code> is not a valid offset
   */
  public static final int getPositionBelow(JTextComponent c, int offset, int x)
    throws BadLocationException
  {
    View rootView = c.getUI().getRootView(c);
    Rectangle r = c.modelToView(offset);
    int offs = c.viewToModel(new Point(x, r.y));
    int pos = rootView.getNextVisualPositionFrom(c, offs,
                                                 Position.Bias.Forward,
                                                 SwingConstants.SOUTH,
                                                 new Position.Bias[1]);
    return pos;
  }
}
