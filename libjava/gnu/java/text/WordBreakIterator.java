/* WordBreakIterator.java - Default word BreakIterator.
   Copyright (C) 1999, 2001, 2004 Free Software Foundation, Inc.

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


package gnu.java.text;

import java.text.CharacterIterator;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date March 22, 1999
 * Written using The Unicode Standard, Version 2.0.
 */

public class WordBreakIterator extends BaseBreakIterator
{
  public Object clone ()
  {
    return new WordBreakIterator (this);
  }

  public WordBreakIterator ()
  {
  }

  private WordBreakIterator (WordBreakIterator other)
  {
    iter = (CharacterIterator) other.iter.clone();
  }

  // Some methods to tell us different properties of characters.
  private final boolean isHira (char c)
  {
    return c >= 0x3040 && c <= 0x309f;
  }
  private final boolean isKata (char c)
  {
    return c >= 0x30a0 && c <= 0x30ff;
  }
  private final boolean isHan (char c)
  {
    return c >= 0x4e00 && c <= 0x9fff;
  }

  public int next ()
  {
    int end = iter.getEndIndex();
    if (iter.getIndex() == end)
      return DONE;

    while (iter.getIndex() < end)
      {
	char c = iter.current();
	if (c == CharacterIterator.DONE)
	  break;
	int type = Character.getType(c);

	char n = iter.next();
	if (n == CharacterIterator.DONE)
	  break;

	// Break after paragraph separators.
	if (type == Character.PARAGRAPH_SEPARATOR
	    || type == Character.LINE_SEPARATOR)
	  break;

	// Break between letters and non-letters.
	// FIXME: we treat apostrophe as part of a word.  This
	// is an English-ism.
	boolean is_letter = Character.isLetter(c);
	if (c != '\'' && ! is_letter && type != Character.NON_SPACING_MARK
	    && Character.isLetter(n))
	  break;

	// Always break after certain symbols, such as punctuation.
	// This heuristic is derived from hints in the JCL book and is
	// not part of Unicode.  It seems to be right, however.
	// FIXME: we treat apostrophe as part of a word.  This
	// is an English-ism.
	if (c != '\''
	    && (type == Character.DASH_PUNCTUATION
		|| type == Character.START_PUNCTUATION
		|| type == Character.END_PUNCTUATION
		|| type == Character.CONNECTOR_PUNCTUATION
		|| type == Character.OTHER_PUNCTUATION
		|| type == Character.MATH_SYMBOL
		|| type == Character.CURRENCY_SYMBOL
		|| type == Character.MODIFIER_SYMBOL
		|| type == Character.OTHER_SYMBOL
		|| type == Character.FORMAT
		|| type == Character.CONTROL))
	  break;

	boolean is_hira = isHira (c);
	boolean is_kata = isKata (c);
	boolean is_han = isHan (c);

	// Special case Japanese.
	if (! is_hira && ! is_kata && ! is_han
	    && type != Character.NON_SPACING_MARK
	    && (isHira (n) || isKata (n) || isHan (n)))
	  break;

	if (is_hira || is_kata || is_han || is_letter)
	  {
	    // Now we need to do some lookahead.  We might need to do
	    // quite a bit of lookahead, so we save our position and
	    // restore it later.
	    int save = iter.getIndex();
	    // Skip string of non spacing marks.
	    while (n != CharacterIterator.DONE
		   && Character.getType(n) == Character.NON_SPACING_MARK)
	      n = iter.next();
	    if (n == CharacterIterator.DONE)
	      break;
	    if ((is_hira && ! isHira (n))
		|| (is_kata && ! isHira (n) && ! isKata (n))
		|| (is_han && ! isHira (n) && ! isHan (n))
		// FIXME: we treat apostrophe as part of a word.  This
		// is an English-ism.
		|| (is_letter && ! Character.isLetter(n) && n != '\''))
	      break;
	    iter.setIndex(save);
	  }
      }

    return iter.getIndex();
  }

  public int previous ()
  {
    int start = iter.getBeginIndex();
    if (iter.getIndex() == start)
      return DONE;

    while (iter.getIndex() >= start)
      {
	char c = iter.previous();
	if (c == CharacterIterator.DONE)
	  break;

	boolean is_hira = isHira (c);
	boolean is_kata = isKata (c);
	boolean is_han = isHan (c);
	boolean is_letter = Character.isLetter(c);

	char n = iter.previous();
	if (n == CharacterIterator.DONE)
	  break;
	iter.next();
	int type = Character.getType(n);
	// Break after paragraph separators.
	if (type == Character.PARAGRAPH_SEPARATOR
	    || type == Character.LINE_SEPARATOR)
	  break;

	// Break between letters and non-letters.
	// FIXME: we treat apostrophe as part of a word.  This
	// is an English-ism.
	if (n != '\'' && ! Character.isLetter(n)
	    && type != Character.NON_SPACING_MARK
	    && is_letter)
	  break;

	// Always break after certain symbols, such as punctuation.
	// This heuristic is derived from hints in the JCL book and is
	// not part of Unicode.  It seems to be right, however.
	// FIXME: we treat apostrophe as part of a word.  This
	// is an English-ism.
	if (n != '\''
	    && (type == Character.DASH_PUNCTUATION
		|| type == Character.START_PUNCTUATION
		|| type == Character.END_PUNCTUATION
		|| type == Character.CONNECTOR_PUNCTUATION
		|| type == Character.OTHER_PUNCTUATION
		|| type == Character.MATH_SYMBOL
		|| type == Character.CURRENCY_SYMBOL
		|| type == Character.MODIFIER_SYMBOL
		|| type == Character.OTHER_SYMBOL
		|| type == Character.FORMAT
		|| type == Character.CONTROL))
	  break;

	// Special case Japanese.
	if ((is_hira || is_kata || is_han)
	    && ! isHira (n) && ! isKata (n) && ! isHan (n)
	    && type != Character.NON_SPACING_MARK)
	  break;

	// We might have to skip over non spacing marks to see what's
	// on the other side.
	if (! is_hira || (! is_letter && c != '\''))
	  {
	    int save = iter.getIndex();
	    while (n != CharacterIterator.DONE
		   && Character.getType(n) == Character.NON_SPACING_MARK)
	      n = iter.previous();
	    iter.setIndex(save);
	    // This is a strange case: a bunch of non-spacing marks at
	    // the beginning.  We treat the current location as a word
	    // break.
	    if (n == CharacterIterator.DONE)
	      break;
	    if ((isHira (n) && ! is_hira)
		|| (isKata (n) && ! is_hira && ! is_kata)
		|| (isHan (n) && ! is_hira && ! is_han)
		// FIXME: we treat apostrophe as part of a word.  This
		// is an English-ism.
		|| (! is_letter && c != '\'' && Character.isLetter(n)))
	      break;
	  }
      }

    return iter.getIndex();
  }
}
