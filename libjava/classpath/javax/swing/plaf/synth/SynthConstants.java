/* SynthConstants.java -- A couple of constants used by Synth
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package javax.swing.plaf.synth;

/**
 * A couple of constants used by the Synth Look and Feel.
 *
 * @author Roman Kennke (kennke@aicas.com)
 *
 * @since 1.5
 */
public interface SynthConstants
{
  /**
   * A primary state indicating that a component is enabled.
   */
  static final int ENABLED = 1;

  /**
   * A primary state indicating that a component is disabled.
   */
  static final int DISABLED = 8;

  /**
   * A primary state indicating that the mouse is over a region.
   */
  static final int MOUSE_OVER = 2;

  /**
   * A primary state indicating that the component is in a pressed state (which
   * does not necessarily mean that the mouse is pressed over the component).
   */
  static final int PRESSED = 4;

  /**
   * Indicates that a region has focus.
   */
  static final int FOCUSED = 256;

  /**
   * Indicates that a region is selected.
   */
  static final int SELECTED = 512;

  /**
   * Indicates that a region is in its default state.
   */
  static final int DEFAULT = 1024;
}
