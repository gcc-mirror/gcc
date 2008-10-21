/* gnu/regexp/REFilterInputStream.java
   Copyright (C) 1998-2001, 2004 Free Software Foundation, Inc.

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


package gnu.java.util.regex;
import java.io.FilterInputStream;
import java.io.InputStream;

/**
 * Replaces instances of a given RE found within an InputStream
 * with replacement text.   The replacements are interpolated into the
 * stream when a match is found.
 *
 * @author <A HREF="mailto:wes@cacas.org">Wes Biggs</A>
 * @deprecated This class cannot properly handle all character
 *             encodings.  For proper handling, use the REFilterReader
 *             class instead.
 */

public class REFilterInputStream extends FilterInputStream
{

  private RE expr;
  private String replace;
  private String buffer;
  private int bufpos;
  private int offset;
  private CharIndexedInputStream stream;

  /**
   * Creates an REFilterInputStream.  When reading from this stream,
   * occurrences of patterns matching the supplied regular expression
   * will be replaced with the supplied replacement text (the
   * metacharacters $0 through $9 may be used to refer to the full
   * match or subexpression matches).
   *
   * @param stream The InputStream to be filtered.
   * @param expr The regular expression to search for.
   * @param replace The text pattern to replace matches with.  
   */
  public REFilterInputStream (InputStream stream, RE expr, String replace)
  {
    super (stream);
    this.stream = new CharIndexedInputStream (stream, 0);
    this.expr = expr;
    this.replace = replace;
  }

  /**
   * Reads the next byte from the stream per the general contract of
   * InputStream.read().  Returns -1 on error or end of stream.
   */
  public int read ()
  {
    // If we have buffered replace data, use it.
    if ((buffer != null) && (bufpos < buffer.length ()))
      {
	return (int) buffer.charAt (bufpos++);
      }

    // check if input is at a valid position
    if (!stream.isValid ())
      return -1;

    REMatch mymatch = new REMatch (expr.getNumSubs (), offset, 0);
    if (expr.match (stream, mymatch))
      {
	mymatch.end[0] = mymatch.index;
	mymatch.finish (stream);
	stream.move (mymatch.toString ().length ());
	offset += mymatch.toString ().length ();
	buffer = mymatch.substituteInto (replace);
	bufpos = 1;

	// This is prone to infinite loops if replace string turns out empty.
	if (buffer.length () > 0)
	  {
	    return buffer.charAt (0);
	  }
      }
    char ch = stream.charAt (0);
    if (ch == CharIndexed.OUT_OF_BOUNDS)
      return -1;
    stream.move (1);
    offset++;
    return ch;
  }

  /** 
   * Returns false.  REFilterInputStream does not support mark() and
   * reset() methods. 
   */
  public boolean markSupported ()
  {
    return false;
  }

  /** Reads from the stream into the provided array. */
  public int read (byte[]b, int off, int len)
  {
    int i;
    int ok = 0;
    while (len-- > 0)
      {
	i = read ();
	if (i == -1)
	  return (ok == 0) ? -1 : ok;
	b[off++] = (byte) i;
	ok++;
      }
    return ok;
  }

  /** Reads from the stream into the provided array. */
  public int read (byte[]b)
  {
    return read (b, 0, b.length);
  }
}
