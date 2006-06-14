/* LineBreakMeasurer.java
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
import java.text.BreakIterator;
import java.awt.font.TextLayout;
import java.awt.font.FontRenderContext;
import java.awt.Shape;

public final class LineBreakMeasurer
{
  private AttributedCharacterIterator text;
  private int position;
  private FontRenderContext frc;
  private TextLayout totalLayout;
  private int numChars;

  public LineBreakMeasurer(AttributedCharacterIterator text, 
			   BreakIterator breakIter, FontRenderContext frc)
  {
    this.text = text;
    this.frc = frc;
    position = 0;
    totalLayout = new TextLayout(text, frc);
    numChars = totalLayout.getCharacterCount();
  }

  public LineBreakMeasurer(AttributedCharacterIterator text, 
			   FontRenderContext frc)
  {
    this.text = text;
    this.frc = frc;
    position = 0;
    totalLayout = new TextLayout(text, frc);
    numChars = totalLayout.getCharacterCount();
  }

  public void deleteChar(AttributedCharacterIterator newParagraph, 
			 int deletePos)
  {
    totalLayout = new TextLayout(newParagraph, frc);
    if( deletePos < 0 || deletePos > totalLayout.getCharacterCount() )
      throw new NullPointerException("Invalid deletePos:"+deletePos);
    numChars = totalLayout.getCharacterCount();
    text = newParagraph;
    position = 0;
  }

  public void insertChar(AttributedCharacterIterator newParagraph, 
			 int insertPos)
  {
    totalLayout = new TextLayout(newParagraph, frc);
    if( insertPos < 0 || insertPos > totalLayout.getCharacterCount() )
      throw new NullPointerException("Invalid insertPos:"+insertPos);
    numChars = totalLayout.getCharacterCount();
    text = newParagraph;
    position = 0;
  }

  public TextLayout nextLayout(float wrappingWidth)
  {
    return nextLayout( wrappingWidth, numChars, false );
  }

  public TextLayout nextLayout(float wrappingWidth, int offsetLimit, 
			       boolean requireNextWord)
  {
    int next = nextOffset( wrappingWidth, offsetLimit, requireNextWord );
    AttributedCharacterIterator aci = (new AttributedString( text, 
							     position, next )
				       ).getIterator();
    position = next;
    return new TextLayout( aci, frc );
  }

  public int nextOffset(float wrappingWidth)
  {
    return nextOffset( wrappingWidth, numChars, false );
  }

  public int nextOffset(float wrappingWidth, int offsetLimit, 
			boolean requireNextWord)
  {
    Shape s = totalLayout.getBlackBoxBounds( position, offsetLimit );
    double remainingLength = s.getBounds2D().getWidth();

    int guessOffset = (int)( ( (double)wrappingWidth / (double)remainingLength)
			     * ( (double)numChars - (double)position ) );
    guessOffset += position;
    if( guessOffset > offsetLimit )
      guessOffset = offsetLimit;

    s = totalLayout.getBlackBoxBounds( position, guessOffset );
    double guessLength = s.getBounds2D().getWidth();

    boolean makeSmaller = ( guessLength > wrappingWidth );
    int inc = makeSmaller ? -1 : 1;
    boolean keepGoing = true;

    do
      {
	guessOffset = guessOffset + inc;
	if( guessOffset <= position || guessOffset > offsetLimit )
	  {
	    keepGoing = false;
	  }
	else
	  {
	    s = totalLayout.getBlackBoxBounds( position, guessOffset );
	    guessLength = s.getBounds2D().getWidth();
	    if( makeSmaller && ( guessLength <= wrappingWidth) )	  
	      keepGoing = false;
	    if( !makeSmaller && ( guessLength >= wrappingWidth) )
	      keepGoing = false;
	  }
      }
    while( keepGoing );

    if( !makeSmaller )
      guessOffset--;

    if( guessOffset >= offsetLimit )
      return offsetLimit;

    text.setIndex( guessOffset );
    if( !requireNextWord )
      {
	char c = text.previous();
	while( !Character.isWhitespace( c ) && c != '-' && 
	       guessOffset > position )
	  { 
	    guessOffset--; 
	    c = text.previous();
	  }
      }
    else
      {
	char c = text.next();
	while( !Character.isWhitespace( c ) && c != '-' && 
	       guessOffset < offsetLimit )
	  {
	    guessOffset++;
	    c = text.next();
	  }
      }

    return guessOffset;
  }

  public void setPosition(int newPosition)
  {
    position = newPosition;
  }

  public int getPosition()
  {
    return position;
  }
}

