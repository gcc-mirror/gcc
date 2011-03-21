/* Annotation.java -- Wrapper for a text attribute object
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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


package java.text;

/**
  * This class is used as a wrapper for a text attribute object.  Annotation
  * objects are associated with a specific range of text.  Changing either
  * the text range or the underlying text invalidates the object.
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class Annotation
{

/*
 * Instance Variables
 */

/**
  * This is the attribute object being wrappered
  */
private Object attrib;

/*************************************************************************/

/**
  * Constructors
  */

/**
  * This method initializes a new instance of <code>Annotation</code> to
  * wrapper the specified text attribute object.
  *
  * @param attrib The text attribute <code>Object</code> to wrapper.
  */
public
Annotation(Object attrib)
{
  this.attrib = attrib;
}

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * This method returns the text attribute object this <code>Annotation</code>
  * instance is wrappering.
  *
  * @return The text attribute object for this <code>Annotation</code>.
  */
public Object
getValue()
{
  return(attrib);
}

/*************************************************************************/

/**
  * This method returns a <code>String</code> representation of this
  * object.
  *
  * @return This object as a <code>String</code>.
  */
public String
toString()
{
  return(getClass().getName() + "[value=" + attrib.toString() + "]");
}

} // class Annotation
