/* Insets.java -- Information about a container border.
   Copyright (C) 1999, 2000, 2002 Free Software Foundation, Inc.

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


package java.awt;

/**
  * This class represents the "margin" or space around a container.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class Insets implements Cloneable, java.io.Serializable
{

/*
 * Instance Variable
 */

/**
  * @serial The top inset
  */
public int top;

/**
  * @serial This bottom inset
  */
public int bottom;

/**
  * @serial The left inset
  */
public int left;

/**
  * @serial The right inset
  */
public int right;

/*************************************************************************/

/**
  * Initializes a new instance of <code>Inset</code> with the specified
  * inset values.
  *
  * @param top The top inset
  * @param left The left inset
  * @param bottom The bottom inset
  * @param right The right inset
  */
public
Insets(int top, int left, int bottom, int right)
{
  this.top = top;
  this.left = left;
  this.bottom = bottom;
  this.right = right;
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Tests whether this object is equal to the specified object.  This will
  * be true if and only if the specified object:
  * <p>
  * <ul>
  * <li>Is not <code>null</code>.
  * <li>Is an instance of <code>Insets</code>.
  * <li>Has the same top, bottom, left, and right inset values as this object.
  * </ul>
  *
  * @param obj The object to test against.
  *
  * @return <code>true</code> if the specified object is equal to this
  * one, <code>false</code> otherwise.
  */
public boolean
equals(Object obj)
{
  if (!(obj instanceof Insets))
    return(false);

  Insets i = (Insets)obj;

  if (i.top != top)
    return(false);
  if (i.bottom != bottom)
    return(false);
  if (i.left != left)
    return(false);
  if (i.right != right)
    return(false);

  return(true);
}

public int
hashCode()
{
  return top + bottom + left + right;
}

/*************************************************************************/

/**
  * Returns a string representation of this object.
  *
  * @return A string representation of this object.
  */
public String
toString()
{
  return(getClass().getName() + "(top=" + top + ",bottom=" + bottom +
         ",left=" + left + ",right=" + right + ")");
}

/*************************************************************************/

/**
  * Returns a copy of this object.
  *
  * @return A copy of this object.
  */
public Object
clone()
{
  try
    {
      return(super.clone());
    }
  catch(Exception e)
    {
      return(null);
    }
}

} // class Insets 
