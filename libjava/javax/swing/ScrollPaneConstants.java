/* ScrollPaneConstants.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

package javax.swing;

/**
 * ScrollPaneConstants
 * @author	Andrew Selkirk
 * @version	1.0
 */
public interface ScrollPaneConstants
{
  /**
   * VIEWPORT
   */
  String VIEWPORT = "VIEWPORT";

  /**
   * VERTICAL_SCROLLBAR
   */
  String VERTICAL_SCROLLBAR = "VERTICAL_SCROLLBAR";

  /**
   * HORIZONTAL_SCROLLBAR
   */
  String HORIZONTAL_SCROLLBAR = "HORIZONTAL_SCROLLBAR";

  /**
   * ROW_HEADER
   */
  String ROW_HEADER = "ROW_HEADER";

  /**
   * COLUMN_HEADER
   */
  String COLUMN_HEADER = "COLUMN_HEADER";

  /**
   * LOWER_LEFT_CORNER
   */
  String LOWER_LEFT_CORNER = "LOWER_LEFT_CORNER";

  /**
   * LOWER_RIGHT_CORNER
   */
  String LOWER_RIGHT_CORNER = "LOWER_RIGHT_CORNER";

  /**
   * UPPER_LEFT_CORNER
   */
  String UPPER_LEFT_CORNER = "UPPER_LEFT_CORNER";

  /**
   * UPPER_RIGHT_CORNER
   */
  String UPPER_RIGHT_CORNER = "UPPER_RIGHT_CORNER";

  /**
   * LOWER_LEADING_CORNER
   */
  String LOWER_LEADING_CORNER = "LOWER_LEADING_CORNER";

  /**
   * LOWER_TRAILING_CORNER
   */
  String LOWER_TRAILING_CORNER = "LOWER_TRAILING_CORNER";

  /**
   * UPPER_LEADING_CORNER
   */
  String UPPER_LEADING_CORNER = "UPPER_LEADING_CORNER";

  /**
   * UPPER_TRAILING_CORNER
   */
  String UPPER_TRAILING_CORNER = "UPPER_TRAILING_CORNER";

  /**
   * VERTICAL_SCROLLBAR_POLICY
   */
  String VERTICAL_SCROLLBAR_POLICY = "VERTICAL_SCROLLBAR_POLICY";

  /**
   * HORIZONTAL_SCROLLBAR_POLICY
   */
  String HORIZONTAL_SCROLLBAR_POLICY = "HORIZONTAL_SCROLLBAR_POLICY";

  /**
   * VERTICAL_SCROLLBAR_AS_NEEDED
   */
  int VERTICAL_SCROLLBAR_AS_NEEDED = 20;

  /**
   * VERTICAL_SCROLLBAR_NEVER
   */
  int VERTICAL_SCROLLBAR_NEVER = 21;

  /**
   * VERTICAL_SCROLLBAR_ALWAYS
   */
  int VERTICAL_SCROLLBAR_ALWAYS = 22;

  /**
   * HORIZONTAL_SCROLLBAR_AS_NEEDED
   */
  int HORIZONTAL_SCROLLBAR_AS_NEEDED = 30;

  /**
   * HORIZONTAL_SCROLLBAR_NEVER
   */
  int HORIZONTAL_SCROLLBAR_NEVER = 31;

  /**
   * HORIZONTAL_SCROLLBAR_ALWAYS
   */
  int HORIZONTAL_SCROLLBAR_ALWAYS = 32;
}
