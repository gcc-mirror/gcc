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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


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

