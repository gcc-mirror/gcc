/* MXBean.java - Marks a management bean interface as an MXBean.
   Copyright (C) 2007 Free Software Foundation

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

package javax.management;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.RetentionPolicy.RUNTIME;
import static java.lang.annotation.ElementType.TYPE;

/**
 * <p>
 * An annotation used to explictly mark an interface
 * as defining (or not defining) an {@link MXBean}.  By
 * default, such beans are taken to be those whose interface
 * has the suffix {@code "MXBean"}.  The presence of this
 * annotation overrides this intuition.  The following
 * interfaces would be classed as {@link MXBean}s:
 * </p>
 * <ul>
 * <li>{@code public interface SomethingMXBean{}}</li>
 * <li>{@code @MXBean public interface Someat{}}</li>
 * <li>{@code @MXBean(true) public interface SomeatElse{}}</li>
 * </ul>
 * <p>The following would not:</p>
 * <ul>
 * <li>{@code public interface RandomInterface{}}</li>
 * <li>{@code @MXBean(false) public interface SomethingMXBean{}}</li>
 * </ul>
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.6
 */
@Documented @Retention(RUNTIME) @Target(TYPE)
public @interface MXBean
{

  /**
   * Returns true if the annotated interface
   * is an {@link MXBean}.
   *
   * @return true if the interface is an {@link MXBean}.
   */
  boolean value();

}
