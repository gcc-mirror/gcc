/* Calendar_nl.java -- Dutch calendar locale data
   Copyright (C) 1999 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package gnu.java.locale;

import java.util.ListResourceBundle;
import java.util.Calendar;

/**
 * This class contains locale data for java.util.Calendar specific for 
 * dutch language.
 * @author Mark Wielaard
 */
public class Calendar_nl extends ListResourceBundle
{
  /**
   * This is the object array used to hold the keys and values
   * for this bundle
   */
  private static final Object[][] contents =
  {
    { "firstDayOfWeek", new Integer(Calendar.MONDAY) },

    /* XXX - I guess the default for gregorianCutover 
     * is also true for the Netherlands. But is it?
     */
  };

  /**
   * This method returns the object array of key, value pairs containing
   * the data for this bundle.
   *
   * @return The key, value information.
   */
  public Object[][] getContents()
  {
    return contents;
  }
}
