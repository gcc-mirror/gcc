/* Formattable.java -- Objects which can be passed to a Formatter
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


package java.util;

/**
 * <p>
 * The <code>Formattable</code> interface is used to provide customised
 * formatting to arbitrary objects via the {@link Formatter}.  The
 * {@link #formatTo} method is called for <code>Formattable</code>
 * objects used with the 's' conversion operator, allowing the object
 * to provide its own formatting of its internal data.
 * </p>
 * <p>
 * Thread safety is left up to the implementing class.  Thus,
 * {@link Formattable} objects are not guaranteed to be thread-safe,
 * and users should make their own provisions for multiple thread access.
 * </p>
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface Formattable
{

  /**
   * Formats the object using the supplied formatter to the specification
   * provided by the given flags, width and precision.
   *
   * @param formatter the formatter to use for formatting the object.
   *                  The formatter gives access to the output stream
   *                  and locale via {@link Formatter#out()} and
   *                  {@link Formatter#locale()} respectively.
   * @param flags a bit mask constructed from the flags in the
   *              {@link FormattableFlags} class.  When no flags
   *              are set, the implementing class should use its
   *              defaults.
   * @param width the minimum number of characters to include.
   *              A value of -1 indicates no minimum.  The remaining
   *              space is padded with ' ' either on the left
   *              (the default) or right (if left justification is
   *              specified by the flags).
   * @param precision the maximum number of characters to include.
   *              A value of -1 indicates no maximum.  This value
   *              is applied prior to the minimum (the width).  Thus,
   *              a value may meet the minimum width initially, but
   *              not when the width value is applied, due to
   *              characters being removed by the precision value.
   * @throws IllegalFormatException if there is a problem with
   *                                the syntax of the format
   *                                specification or a mismatch
   *                                between it and the arguments.
   */
  public void formatTo(Formatter formatter, int flags, int width,
                       int precision);
}
