/* FieldDoc.java -- Document a field
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
  * This package is used for documenting fields.
  */
public interface FieldDoc extends MemberDoc
{

/**
  * This method returns the type of this field.
  *
  * @return The type of this field.
  */
public abstract Type
type();

/*************************************************************************/

/**
  * This method tests whether or not the field is transient.
  *
  * @return <code>true</code> if the field is transient, <code>false</code>
  * otherwise.
  */
public abstract boolean
isTransient();

/*************************************************************************/

/**
  * This method tests whether or not the field is volatile.
  *
  * @return <code>true</code> if the field is volatile, <code>false</code>
  * otherwise.
  */
public abstract boolean
isVolatile();

/*************************************************************************/

/**
  * This method returns a list of all "@serialField" tags defined in this
  * field.
  *
  * @return The list of "@serialField" tags for this field.
  */
public abstract SerialFieldTag[]
serialFieldTags();

/*************************************************************************/

/**
  * This method returns the value of this static field.
  *
  * @return The value of this static field.
  */
public abstract Object
constantValue();


/*************************************************************************/

/**
  * This method returns the value of this static field converted to a
  * human-readable string.
  *
  * @return The value of this static field as a human-readable string.
  */
public abstract String
constantValueExpression();



} // interface FieldDoc

