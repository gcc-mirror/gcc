// StringCharacterIterator.java - Iterate over string of Unicode characters.

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date February 22, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct to 1.1.
 */

public final class StringCharacterIterator implements CharacterIterator
{
  public Object clone ()
    {
      return (Object) new StringCharacterIterator (text, begin, end, pos);
    }

  public char current ()
    {
      // This follows JDK 1.2 semantics and not 1.1 semantics.
      // In 1.1 we would throw an exception if begin==end.
      return (pos < end) ? text.charAt(pos) : CharacterIterator.DONE;
    }

  public boolean equals (Object obj)
    {
      if (! (obj instanceof StringCharacterIterator))
	return false;
      StringCharacterIterator sci = (StringCharacterIterator) obj;
      // The spec says "the same text".  We take this to mean equals,
      // not ==.
      return (pos == sci.pos
	      && begin == sci.begin
	      && end == sci.end
	      && text.equals(sci.text));
    }

  public char first ()
    {
      pos = begin;
      return current ();
    }

  public int getBeginIndex ()
    {
      return begin;
    }

  public int getEndIndex ()
    {
      return end;
    }

  public int getIndex ()
    {
      return pos;
    }

  public int hashCode ()
    {
      // FIXME: this is a terrible hash code.  Find a better one.
      return text.hashCode() + pos + begin + end;
    }

  public char last ()
    {
      pos = end;
      return current ();
    }

  public char next ()
    {
      if (pos == end)
	return CharacterIterator.DONE;
      ++pos;
      return current ();
    }

  public char previous ()
    {
      if (pos == begin)
	return CharacterIterator.DONE;
      --pos;
      return current ();
    }

  public char setIndex (int idx)
    {
      // In 1.1 we would throw an error if `idx == end'.
      if (idx < begin || idx > end)
	throw new IllegalArgumentException ();
      pos = idx;
      return current ();
    }

  public StringCharacterIterator (String text)
    {
      // FIXME: remove check for null once we have compiler/runtime
      // support for NullPointerException.
      this (text, 0, text == null ? 0 : text.length(), 0);
    }
  public StringCharacterIterator (String text, int pos)
    {
      // FIXME: remove check for null once we have compiler/runtime
      // support for NullPointerException.
      this (text, 0, text == null ? 0 : text.length(), pos);
    }
  public StringCharacterIterator (String text, int begin, int end, int pos)
    {
      if (text == null)
	throw new NullPointerException ();
      if (begin < 0 || begin > end || end > text.length()
	  // In 1.1 we would also throw if `pos == end'.
	  || pos < begin || pos > end)
	throw new IllegalArgumentException ();

      this.text = text;
      this.begin = begin;
      this.end = end;
      this.pos = pos;
    }

  // String to iterate over.
  private String text;
  // Current position.
  private int pos;
  // Start position in string.
  private int begin;
  // End position in string.
  private int end;
}
