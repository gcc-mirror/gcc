/* DriverPropertyInfo.java -- Property information about drivers.
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


package java.sql;

/**
 * This class holds a driver property that can be used for querying or
 * setting driver configuration parameters.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class DriverPropertyInfo 
{
  /**
   * The name of the property.
   */
  public String name;

  /**
   * A description of the property, possibly <code>null</code>.
   */
  public String description;

  /**
   * A flag indicating whether or not a value for this property is required
   * in order to connect to the database.
   */
  public boolean required;

  /**
   * This is the value of the property.
   */
  public String value;

  /**
    * If values are restricted to certain choices, this is the list of valid
    * ones.  Otherwise it is <code>null</code>.
    */
  public String[] choices;

  /**
   * This method initializes a new instance of <code>DriverPropertyInfo</code>
   * with the specified name and value.  All other fields are defaulted.
   *
   * @param name The name of the property.
   * @param value The value to assign to the property.
   */
  public DriverPropertyInfo(String name, String value)
  {
    this.name = name;
    this.value = value;
  }
}
