/* PageFormat.java -- Information about the page format
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


package java.awt.print;

/**
  * This class contains information about the desired page format to
  * use for printing a particular set of pages.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class PageFormat implements Cloneable
{

/*
 * Static Variables
 */

/**
  * A constant for a landscaped page orientation.  Used by
  * <code>getOrientation</code> and <code>setOrientation</code>.
  */
public static final int LANDSCAPE = 0;

/**
  * A constant for a portrait page orientation.  Used by
  * <code>getOrientation</code> and <code>setOrientation</code>.
  */
public static final int PORTRAIT = 1;

/**
  * A constant for a reversed landscaped page orientation.  This is
  * the orientation used by Macintosh's for landscape.  The origin is
  * in the upper right hand corner instead of the upper left.  The
  * X and Y axes are reversed. Used by <code>getOrientation</code> and 
  * <code>setOrientation</code>.
  */
public static final int REVERSE_LANDSCAPE = 2;

/*************************************************************************/

/*
 * Instance Variables
 */

// The page orientation
private int orientation;

// The paper type
private Paper paper;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * This method creates a default page layout, which will be in portrait
  * format.
  */
public
PageFormat()
{
  this.paper = new Paper();
  this.orientation = PORTRAIT;
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * This method returns the width of the page, in 1/72nd's of an inch.  The
  * "width" measured depends on orientation.
  *
  * @return The width of the page.
  */
public double
getWidth()
{
  return(paper.getWidth());
}

/*************************************************************************/

/**
  * This method returns the height of the page, in 1/72nd's of an inch.
  * The "height" measured depends on the orientation.
  *
  * @return The height of the page.
  */
public double
getHeight()
{
  return(paper.getHeight());
}

/*************************************************************************/

/**
  * This method returns the X coordinate value of the upper leftmost
  * drawable area of the paper.
  *
  * @return The upper leftmost imageable X coordinate.
  */
public double
getImageableX()
{
  return(paper.getImageableX());
}

/*************************************************************************/

/**
  * This method returns the Y coordinate value of the upper leftmost
  * drawable area of the paper.
  *
  * @return The upper leftmost imageable Y coordinate.
  */
public double
getImageableY()
{
  return(paper.getImageableY());
}

/*************************************************************************/

/**
  * This method returns the imageable width of the paper, in 1/72nd's of
  * an inch.
  *
  * @return The imageable width of the paper.
  */
public double
getImageableWidth()
{
  return(paper.getImageableWidth());
}

/*************************************************************************/

/**
  * This method returns the imageable height of the paper, in 1/72nd's of
  * an inch.
  *
  * @return The imageable height of the paper.
  */
public double
getImageableHeigth()
{
  return(paper.getImageableHeight());
}

/*************************************************************************/

/**
  * Returns a copy of the <code>paper</code> object being used for this 
  * page format.
  *
  * @return A copy of the <code>Paper</code> object for this format.
  */
public Paper
getPaper()
{
  return((Paper)paper.clone());
}

/*************************************************************************/

/**
  * Sets the <code>Paper</code> object to be used by this page format.
  *
  * @param paper The new <code>Paper</code> object for this page format.
  */
public void
setPaper(Paper paper)
{
  this.paper = paper;
}

/*************************************************************************/

/**
  * This method returns the current page orientation.  The value returned
  * will be one of the page orientation constants from this class.
  *
  * @return The current page orientation.
  */
public int
getOrientation()
{
  return(orientation);
}

/*************************************************************************/

/**
  * This method sets the page orientation for this format to the
  * specified value.  It must be one of the page orientation constants
  * from this class or an exception will be thrown.
  *
  * @param orientation The new page orientation.
  *
  * @exception IllegalArgumentException If the specified page orientation
  * value is not one of the constants from this class.
  */
public void
setOrientation(int orientation) throws IllegalArgumentException
{
  if ((orientation != PORTRAIT) &&
      (orientation != LANDSCAPE) &&
      (orientation != REVERSE_LANDSCAPE))
    throw new IllegalArgumentException("Bad page orientation value: " +
                                       orientation);

  this.orientation = orientation;
}

/*************************************************************************/

/**
  * This method returns a matrix used for transforming user space
  * coordinates to page coordinates.  The value returned will be six
  * doubles as described in <code>java.awt.geom.AffineTransform</code>.
  *
  * @return The transformation matrix for this page format.
  */
public double[]
getMatrix()
{
  throw new RuntimeException("Not implemented since I don't know what to do");
}

/*************************************************************************/

/**
  * This method returns a copy of this object.
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
  catch(CloneNotSupportedException e)
    {
      return(null);
    }
}

} // class PageFormat

