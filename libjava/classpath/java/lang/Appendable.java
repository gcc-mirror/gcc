/* Appendable.java -- Something to which characters can be appended
   Copyright (C) 2004 Free Software Foundation

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

package java.lang;

import java.io.IOException;

/**
 * <p>
 * An <code>Appendable</code> object is one to which a sequence of Unicode
 * characters can be added.  The appended characters must be valid Unicode
 * characters, and may include supplementary characters, composed of multiple
 * 16-bit <code>char</code> values.
 * </p>
 * <p>
 * The behaviour of the <code>Appendable</code> object is heavily dependent
 * on the particular implementation being used.  Some implementations may be
 * thread-safe, while others may not.  Likewise, some implementing classes
 * may produce errors which aren't propogated to the invoking class, due
 * to differences in the error handling used.
 * </p>
 * <p>
 * <strong>Note</strong>: implementation of this interface is required for
 * any class that wishes to receive data from a <code>Formatter</code>
 * instance.
 * </p>
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface Appendable
{

  /**
   * Appends the Unicode character, c, to this <code>Appendable</code>
   * object.
   *
   * @param c the character to append.
   * @return a reference to this object.
   * @throws IOException if an I/O error occurs.
   */
  Appendable append(char c)
    throws IOException;

  /**
   * Appends the specified sequence of Unicode characters to this
   * <code>Appendable</code> object.  The entire sequence may not
   * be appended, if constrained by the underlying implementation.
   * For example, a buffer may reach its size limit before the entire
   * sequence is appended.
   *
   * @param seq the character sequence to append.  If seq is null,
   *        then the string "null" (the string representation of null)
   *        is appended.
   * @return a reference to this object.
   * @throws IOException if an I/O error occurs.
   */
  Appendable append(CharSequence seq)
    throws IOException;

  /**
   * Appends the specified subsequence of Unicode characters to this
   * <code>Appendable</code> object, starting and ending at the specified
   * positions within the sequence.  The entire sequence may not
   * be appended, if constrained by the underlying implementation.
   * For example, a buffer may reach its size limit before the entire
   * sequence is appended.  The behaviour of this method matches the
   * behaviour of <code>append(seq.subSequence(start,end))</code> when
   * the sequence is not null.
   *
   * @param seq the character sequence to append.  If seq is null,
   *        then the string "null" (the string representation of null)
   *        is appended.
   * @param start the index of the first Unicode character to use from
   *        the sequence.
   * @param end the index of the last Unicode character to use from the
   *        sequence.
   * @return a reference to this object.
   * @throws IOException if an I/O error occurs.
   * @throws IndexOutOfBoundsException if either of the indices are negative,
   *         the start index occurs after the end index, or the end index is
   *         beyond the end of the sequence.
   */
  Appendable append(CharSequence seq, int start, int end)
    throws IOException;

}
