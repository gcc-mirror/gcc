/* TextMeasurer.java
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


package java.awt.font;

import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.awt.Shape;

/**
 * TextMeasurer is a small utility class for measuring the length of laid-out
 * text objects. 
 *
 * @author Sven de Marothy
 * @since 1.3
 */
public final class TextMeasurer implements Cloneable
{
  private AttributedCharacterIterator text;
  private FontRenderContext frc;
  private TextLayout totalLayout;
  private int numChars;

  /**
   * Creates a TextMeasurer from a given text in the form of an
   * <code>AttributedCharacterIterator</code> and a 
   * <code>FontRenderContext</code>.
   */  
  public TextMeasurer (AttributedCharacterIterator text, FontRenderContext frc)
  {
    this.text = text;
    this.frc = frc;
    totalLayout = new TextLayout( text, frc );
    numChars = totalLayout.getCharacterCount();
  }

  /**
   * Clones the TextMeasurer object
   */
  protected Object clone ()
  {
    return new TextMeasurer( text, frc );
  }

  /**
   * Update the text if a character is deleted at the position deletePos
   * @param newParagraph - the updated paragraph.
   * @param deletePos - the deletion position
   */
  public void deleteChar (AttributedCharacterIterator newParagraph,
                          int deletePos)
  {
    totalLayout = new TextLayout(newParagraph, frc);
    if( deletePos < 0 || deletePos > totalLayout.getCharacterCount() )
      throw new NullPointerException("Invalid deletePos:"+deletePos);
    numChars = totalLayout.getCharacterCount();
    text = newParagraph;
  }

  /**
   * Update the text if a character is inserted at the position insertPos
   * @param newParagraph - the updated paragraph.
   * @param insertPos - the insertion position
   */
  public void insertChar (AttributedCharacterIterator newParagraph,
                          int insertPos)
  {
    totalLayout = new TextLayout(newParagraph, frc);
    if( insertPos < 0 || insertPos > totalLayout.getCharacterCount() )
      throw new NullPointerException("Invalid insertPos:"+insertPos);
    numChars = totalLayout.getCharacterCount();
    text = newParagraph;
  }

  /***
   * Returns the total advance between two positions in the paragraph.
   * Characters from start to limit-1 (inclusive) are included in this count.
   *
   * @param start - the starting character index.
   * @param limit - the limiting index.
   */
  public float getAdvanceBetween (int start, int limit)
  {
    Shape s = totalLayout.getLogicalHighlightShape( start, limit );
    return (float)s.getBounds2D().getWidth();
  }

  /**
   * Returns a <code>TextLayout</code> object corresponding to the characters
   * from text to limit.
   * @param start - the starting character index.
   * @param limit - the limiting index.
   */
  public TextLayout getLayout (int start, int limit)
  {
    if( start >= limit )
      throw new IllegalArgumentException("Start position must be < limit.");
    return new TextLayout( totalLayout, start, limit );
  }

  /**
   * Returns the line-break index from a given starting index and a maximum
   * advance. The index returned is the first character outside the given
   * advance (or the limit of the string, if all remaining characters fit.)
   *
   * @param start - the starting index.
   * @param maxAdvance - the maximum advance allowed.
   * @return the index of the first character beyond maxAdvance, or the 
   * index of the last character + 1.
   */
  public int getLineBreakIndex (int start, float maxAdvance)
  {   
    if( start < 0 )
      throw new IllegalArgumentException("Start parameter must be > 0.");

    double remainingLength = getAdvanceBetween( start, numChars );
    
    int guessOffset = (int)( ( (double)maxAdvance / (double)remainingLength)
			     * ( (double)numChars - (double)start ) );
    guessOffset += start;
    if( guessOffset > numChars )
      guessOffset = numChars;
    
    double guessLength = getAdvanceBetween( start, guessOffset );
    boolean makeSmaller = ( guessLength > maxAdvance );
    int inc = makeSmaller ? -1 : 1;
    boolean keepGoing = true;

    do
      {
	guessOffset = guessOffset + inc;
	if( guessOffset <= start || guessOffset > numChars )
	  {
	    keepGoing = false;
	  }
	else
	  {
	    guessLength = getAdvanceBetween( start, guessOffset );
	    if( makeSmaller && ( guessLength <= maxAdvance) )	  
	      keepGoing = false;
	    if( !makeSmaller && ( guessLength >= maxAdvance) )
	      keepGoing = false;
	  }
      }
    while( keepGoing );

    // Return first index that doesn't fit.
    if( !makeSmaller )
      guessOffset--;

    if( guessOffset > numChars )
      return numChars;

    return guessOffset;
  }
}
