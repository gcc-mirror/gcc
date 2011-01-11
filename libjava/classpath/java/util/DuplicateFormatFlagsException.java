/* DuplicateFormatFlagsException.java
   Copyright (C) 2005  Free Software Foundation, Inc.

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


package java.util;

/**
 * Thrown when the flags supplied to the {@link Formatter#format()}
 * method of a {@link Formatter} contain duplicates.
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class DuplicateFormatFlagsException
  extends IllegalFormatException
{
  private static final long serialVersionUID = 18890531L;

  /**
   * The flags which contain a duplicate.
   *
   * @serial the flags containing a duplicate.
   */
  // Note: name fixed by serialization.
  private String flags;

  /**
   * Constructs a new <code>DuplicateFormatFlagsException</code>
   * which specifies that the supplied set of flags contains a
   * duplicate.
   *
   * @param flags the flags containing a duplicate.
   * @throws NullPointerException if <code>flags</code> is null.
   */
  public DuplicateFormatFlagsException(String flags)
  {
    super("Duplicate flag passed in " + flags);
    if (flags == null)
      throw new
        NullPointerException("Null flags value passed to constructor.");
    this.flags = flags;
  }

  /**
   * Returns the flags which contain a duplicate.
   *
   * @return the flags.
   */
  public String getFlags()
  {
    return flags;
  }
}
