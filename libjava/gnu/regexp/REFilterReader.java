/*
 *  gnu/regexp/REFilterReader.java
 *  Copyright (C) 2001 Lee Sau Dan
 *  Based on gnu.regexp.REFilterInputStream by Wes Biggs
 *
 *  This library is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published
 *  by the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

package gnu.regexp;
import java.io.FilterReader;
import java.io.Reader;

/**
 * Replaces instances of a given RE with replacement text. 
 *
 * @author <A HREF="http://www.csis.hku.hk/~sdlee/">Lee Sau Dan</A>
 * @since gnu.regexp 1.1.0
 */

public class REFilterReader extends FilterReader {

  private RE expr;
  private String replace;
  private String buffer;
  private int bufpos;
  private int offset;
  private CharIndexedReader stream;

  /**
   * Creates an REFilterReader.  When reading from this stream,
   * occurrences of patterns matching the supplied regular expression
   * will be replaced with the supplied replacement text (the
   * metacharacters $0 through $9 may be used to refer to the full
   * match or subexpression matches.
   *
   * @param stream The Reader to be filtered.
   * @param expr The regular expression to search for.
   * @param replace The text pattern to replace matches with.  
   */
  public REFilterReader(Reader stream, RE expr, String replace) {
    super(stream);
    this.stream = new CharIndexedReader(stream,0);
    this.expr = expr;
    this.replace = replace;
  }

  /**
   * Reads the next character from the stream per the general contract of
   * Reader.read().  Returns -1 on error or end of stream.
   */
  public int read() {
    // If we have buffered replace data, use it.
    if ((buffer != null) && (bufpos < buffer.length())) {
      return (int) buffer.charAt(bufpos++);
    }

    // check if input is at a valid position
    if (!stream.isValid()) return -1;

    REMatch mymatch = new REMatch(expr.getNumSubs(),offset,0);
    if (expr.match(stream,mymatch)) {
      mymatch.end[0] = mymatch.index;
      mymatch.finish(stream);
      stream.move(mymatch.toString().length());
      offset += mymatch.toString().length();
      buffer = mymatch.substituteInto(replace);
      bufpos = 1;

      if (buffer.length() > 0) {
	  return buffer.charAt(0);
      }
    }
    char ch = stream.charAt(0);
    if (ch == CharIndexed.OUT_OF_BOUNDS) return -1;
    stream.move(1);
    offset++;
    return ch;
  }

  /** 
   * Returns false.  REFilterReader does not support mark() and
   * reset() methods. 
   */
  public boolean markSupported() {
    return false;
  }

  /** Reads from the stream into the provided array. */
  public int read(char[] b, int off, int len) {
    int i;
    int ok = 0;
    while (len-- > 0) {
      i = read();
      if (i == -1) return (ok == 0) ? -1 : ok;
      b[off++] = (char) i;
      ok++;
    }
    return ok;
  }

  /** Reads from the stream into the provided array. */
  public int read(char[] b) {
    return read(b,0,b.length);
  }
}
