/* Buffer.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.javax.swing.text.html.parser.support.low;

/**
 * A string buffer that additionally holds line and absolute postion
 * information.
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class Buffer
{
  public static int INITIAL_SIZE = 2048;

  /**
   * True if the \n symbol has been seen.
   */
  public boolean n_seen;

  /**
   * True if the \r symbol has been seen.
   */
  public boolean r_seen;
  char[] chr = new char[ INITIAL_SIZE ];
  int[] line = new int[ INITIAL_SIZE ];
  int[] position = new int[ INITIAL_SIZE ];

  /**
   * Current line.
   */
  int current_line = 0;

  /**
   * Point to the next free position.
   */
  int length;

  public Buffer()
  {
  }

  public Buffer(String content)
  {
    for (int i = 0; i < content.length(); i++)
      {
        append(content.charAt(i), i);
      }
  }

  /**
   * Get the characters into array.
   * @param srcBegin From, inclusive
   * @param srcEnd To, exclusive.
   * @param dst Into
   * @param dstBegin Offset.
   */
  public void getChars(int srcBegin, int srcEnd, char[] dst, int dstBegin)
  {
    System.arraycopy(chr, srcBegin, dst, dstBegin, (srcEnd - srcBegin));
  }

  /**
   * Return the sequence, used to separate lines in the document.
   * @return one of \n, \r or \r\n.
   */
  public String getEndOfLineSequence()
  {
    if (r_seen && n_seen)
      return "\r\n";
    else if (r_seen)
      return "\r";
    else

      // This also is returned for single-line document.
      return "\n";
  }

  /**
   * Truncate.
   * @param n The length to truncate till.
   */
  public void setLength(int n)
  {
    length = n;
  }

  /**
   * Get location information for the given region.
   * @param from Region start, inclusive.
   * @param to Region end, exclusive.
   * @return The location, covering the region.
   */
  public Location getLocation(int from, int to)
  {
    Location l = new Location();
    l.beginLine = line [ from ];
    l.endLine = line [ to - 1 ];

    l.startPosition = position [ from ];
    l.endPosition = position [ to - 1 ] + 1;

    return l;
  }

  /**
   * Add the character.
   * @param c The character.
   * @param pos The character position in the stream (the line number
   * is handled internally in the buffer).
   */
  public void append(char c, int pos)
  {
    if (length >= chr.length)
      expand();
    chr [ length ] = c;
    position [ length ] = pos;

    if (c == '\n')
      {
        if (!r_seen)
          current_line++;
        n_seen = true;
      }
    else if (c == '\r')
      {
        current_line++;
        r_seen = true;
      }

    line [ length ] = current_line;

    length++;
  }

  /**
   * Return char at the given positon.
   */
  public char charAt(int i)
  {
    return chr [ i ];
  }

  /**
   * Delete the range
   * @param from Start position, inclusive.
   * @param to End position, exclusive.
   */
  public void delete(int from, int to)
  {
    int len = to - from;
    if (len < 1)
      throw new AssertionError("Deleting " + from + " till " + to);

    int tail = length - to;

    System.arraycopy(chr, to, chr, from, tail);
    System.arraycopy(position, to, position, from, tail);
    System.arraycopy(line, to, line, from, tail);
    length = length - len;
  }

  /**
   * Double the buffer size.
   */
  public void expand()
  {
    int nSize = 2 * chr.length;

    char[] nchr = new char[ nSize ];
    int[] nposition = new int[ nSize ];
    int[] nline = new int[ nSize ];

    System.arraycopy(chr, 0, nchr, 0, chr.length);
    System.arraycopy(position, 0, nposition, 0, position.length);
    System.arraycopy(line, 0, nline, 0, line.length);

    chr = nchr;
    position = nposition;
    line = nline;
  }

  /**
   * Return length of the occupied part of the buffer.
   */
  public int length()
  {
    return length;
  }

  /**
   * Prepare for parsing the new document.
   */
  public void reset()
  {
    setLength(0);
    r_seen = n_seen = false;
  }

  public String toString()
  {
    return new String(chr, 0, length);
  }
}
