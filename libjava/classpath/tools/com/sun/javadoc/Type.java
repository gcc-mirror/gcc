/* Type.java -- Documentation information about Java types.
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
  * This class is used for holding information about Java types needed for
  * documentation.
  */
public interface Type extends java.io.Serializable
{

/**
  * This method returns the unqualified name of the type, excluding any array
  * dimension information or brackets.
  *
  * @return The unqualified type name, sans array information or brackets.
  */
public abstract String
typeName();

/*************************************************************************/

/**
  * This method returns the fully qualified name of the type, excluding any
  * array dimension information or brackets.
  *
  * @return The fully qualified type name, sans array information or brackets.
  */
public abstract String
qualifiedTypeName();

/*************************************************************************/

/**
  * This method returns the array dimensions as brackets.
  *
  * @param The array dimensions.
  */
public abstract String
dimension();

/*************************************************************************/

/**
  * This method returns the unqualfied name of the type, and includes array
  * dimension information.
  *
  * @return The unqualified name of the type, including array dimension info.
  */
public abstract String
toString();

/*************************************************************************/

/**
  * This method returns this type as a <code>ClassDoc</object>.  This is not
  * a valid operation for primitive types.
  *
  * @return A <code>ClassDoc</code> for this type, or <code>null</code> if
  * this is a primitive type.
  */
public abstract ClassDoc
asClassDoc();

/**
  * This method returns whether this type represents one of the
  * built-in Java primitive types.
  */
public abstract boolean
isPrimitive();

/**
 * Returns this type as a <code>TypeVariable</code>, if it is an
 * instance of the <code>TypeVariable</code> class.  Otherwise,
 * it returns null.
 *
 * @return this cast to a <code>TypeVariable</code> instance, or null
 *         if this is not a type variable.
 */
TypeVariable
asTypeVariable();

} // interface Type

