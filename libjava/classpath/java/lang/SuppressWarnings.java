/* SuppressWarnings - Annotation to avoid compiler warnings
   Copyright (C) 2004 Free Software Foundation, Inc.

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

import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import static java.lang.annotation.RetentionPolicy.SOURCE;
import static java.lang.annotation.ElementType.*;

/**
 * Tell the compiler that a given warning should be suppressed when it
 * pertains to the marked program element and its sub-elements.
 *
 * Note that warning suppression is additive.  For instance if a
 * constructor has a warning suppressed, and a local variable in the
 * constructor has a different warning suppressed, then the resulting
 * set of suppressed warnings for that variable will be both warnings.
 *
 * @since 1.5
 */
@Retention(SOURCE)
@Target({TYPE, FIELD, METHOD, PARAMETER, CONSTRUCTOR, LOCAL_VARIABLE})
public @interface SuppressWarnings
{
  /**
   * The list of warnings to suppress.
   *
   * It is valid to list a name more than once.  Unrecognized names
   * are not a compile-time error.  At the present there is no
   * standard for the names to be recognized by compilers; consult
   * your compiler's documentation for this information.
   */
  String[] value ();
}
