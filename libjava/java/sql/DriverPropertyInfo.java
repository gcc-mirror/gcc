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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.sql;

/**
  * This class holds a driver property that can be used for querying or
  * setting driver configuration parameters.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class DriverPropertyInfo
{

/*
 * Instance Variables
 */

/**
  * The name of the property.
  */
public String name;

/**
  * This is the value of the property.
  */
public String value;

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
  * If values are restricted to certain choices, this is the list of valid
  * ones.  Otherwise it is <code>null</code>.
  */
public String[] choices;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * This method initializes a new instance of <code>DriverPropertyInfo</code>
  * with the specified name and value.  All other fields are defaulted.
  *
  * @param name The name of the property.
  * @param value The value to assign to the property.
  */
public
DriverPropertyInfo(String name, String value)
{
  this.name = name;
  this.value = value;
}

} // DriverPropertyInfo

