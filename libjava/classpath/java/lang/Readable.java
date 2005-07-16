/* Readable.java -- A character source
   Copyright (C) 2004, 2005 Free Software Foundation

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
import java.nio.CharBuffer;

/**
 * A <code>Readable</code> object is simply a source for Unicode character
 * data.  On request, a <code>Readable</code> will provide its data in
 * a supplied <code>CharBuffer</code>.
 *
 * @author Tom Tromey <tromey@redhat.com>
 * @author Andrew John Hughes <gnu_andrew@member.fsf.org>
 * @since 1.5
 */
public interface Readable
{

  /**
   * Adds the character data supplied by this <code>Readable</code>
   * to the specified character buffer.  This method simply places
   * each character into the buffer as supplied, using <code>put()</code>,
   * without flipping or rewinding.
   *
   * @param buf the buffer to place the character data in.
   * @return the number of <code>char</code> values placed in the buffer,
   *         or -1 if no more characters are available.
   * @throws IOException if an I/O error occurs.
   * @throws NullPointerException if buf is null.
   * @throws ReadOnlyBufferException if buf is read only.
   */
  int read(CharBuffer buf)
    throws IOException;

}
