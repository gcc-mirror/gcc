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
import java.text.BreakIterator;

public final class LineBreakMeasurer
{
  private AttributedCharacterIterator text;
  private int position;
  private TextMeasurer tm;
  private int numChars;

  public LineBreakMeasurer(AttributedCharacterIterator text,
                           BreakIterator breakIter, FontRenderContext frc)
  {
    this( text, frc );
  }

  public LineBreakMeasurer(AttributedCharacterIterator text,
                           FontRenderContext frc)
  {
    this.text = text;
    position = 0;
    numChars = text.getEndIndex();
    tm = new TextMeasurer( text, frc );
  }

  public void deleteChar(AttributedCharacterIterator newParagraph,
                         int deletePos)
  {
    tm.deleteChar( newParagraph, deletePos );
    position = 0;
  }

  public void insertChar(AttributedCharacterIterator newParagraph,
                         int insertPos)
  {
    tm.insertChar( newParagraph, insertPos );
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
    TextLayout tl = tm.getLayout( position, next );
    position = next;
    return tl;
  }

  public int nextOffset(float wrappingWidth)
  {
    return nextOffset( wrappingWidth, numChars, false );
  }

  public int nextOffset(float wrappingWidth, int offsetLimit,
                        boolean requireNextWord)
  {
    int guessOffset = tm.getLineBreakIndex(position, wrappingWidth);
    if( offsetLimit > numChars )
      offsetLimit = numChars;

    if( guessOffset > offsetLimit )
      {
        text.setIndex( offsetLimit );
        return offsetLimit;
      }

    text.setIndex( guessOffset );

    // If we're on a breaking character, return directly
    if( Character.isWhitespace( text.current() ) )
      return guessOffset;

    // Otherwise jump forward or backward to the last such char.
    if( !requireNextWord )
      while( !Character.isWhitespace( text.previous() ) &&
             guessOffset > position )
        guessOffset--;
    else
      while( !Character.isWhitespace( text.next() ) &&
             guessOffset < offsetLimit )
        guessOffset++;

    if( guessOffset > offsetLimit )
      {
        text.setIndex( offsetLimit );
        return offsetLimit;
      }

    text.setIndex( guessOffset );

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
