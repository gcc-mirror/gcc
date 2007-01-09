/* BorderWidth.java -- A CSS metric for border widths
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


package gnu.javax.swing.text.html.css;

/**
 * A special CSS metric for border widths. It basically understands everything
 * as Length, and in addition to that provides a mapping for the border-width's
 * thin, medium and think values.
 */
public class BorderWidth
  extends Length
{

  /**
   * Creates a new BorderWidth instance.
   *
   * @param val the CSS value to be interpreted
   */
  public BorderWidth(String val)
  {
    super(val);
    if (val.equals("thin"))
      floatValue = 1.F;
    else if (val.equals("medium"))
      floatValue = 2.F;
    else if (val.equals("thick"))
      floatValue = 3.F;
  }

  /**
   * Checks if the specified value makes up a valid border-width value.
   *
   * @param value the value to check
   *
   * @return <code>true</code> if the value is a valid border-width
   */
  public static boolean isValid(String value)
  {
    return value.equals("thin") || value.equals("medium")
           || value.equals("thick") || Length.isValid(value);
  }
}
