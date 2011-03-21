/* SerialFieldTag.java -- Information about the "@serialField" tag.
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


package com.sun.javadoc;

/**
  * This interface models the "@serialField" tag.
  */
public interface SerialFieldTag extends Tag, Comparable
{

/**
  * This method returns the name of the field.
  *
  * @return The name of the field.
  */
public abstract String
fieldName();

/*************************************************************************/

/**
  * This method returns the type name of the field.
  *
  * @return The type name of the field.
  */
public abstract String
fieldType();

/*************************************************************************/

/**
  * This method returns a <code>ClassDoc</code> instance for the type of
  * the field.  What about primitive types???
  *
  * @return A <code>ClassDoc</code> for the field type.
  */
public abstract ClassDoc
fieldTypeDoc();

/*************************************************************************/

/**
  * This method returns the description of the field.
  *
  * @return The description of the field.
  */
public abstract String
description();

/*************************************************************************/

/**
  * This method compares this object with the specified object in order to
  * determine proper ordering.
  *
  * @param obj The object to compare against.
  *
  * @return A negative number if this object is less than the specified
  * object, zero if the objects are equal, or a positive number if this object
  * is greater than the specified object.
  */
public abstract int
compareTo(Object obj);

} // interface SerialFieldTag
