/* Pageable.java -- Pages to be printed
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
  * This interface represents pages that are to be printed.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface Pageable
{

/*
 * Static Variables
 */

/**
  * This constant is returned when <code>getNumberOfPages()</code>
  * cannot determine the number of pages available for printing.
  */
int UNKNOWN_NUMBER_OF_PAGES = -1;

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * This method returns the number of pages this object contains, or
  * <code>UNKNOWN_NUMBER_OF_PAGES</code> if it cannot determine the number
  * of pages to be printed.
  *
  * @return The number of pages to be printed, or 
  * <code>UNKNOWN_NUMBER_OF_PAGES</code> if this is unknown.
  */
int
getNumberOfPages();

/*************************************************************************/

/**
  * This method returns the <code>PageFormat</code> instance for the
  * specified page.  Page numbers start at zero.  An exception is thrown if 
  * the requested page does not exist.
  *
  * @param pageIndex The index of the page to return the 
  * <code>PageFormat</code> for.
  *
  * @return The <code>PageFormat</code> for the requested page.
  *
  * @exception IndexOutOfBoundsException If the requested page number does
  * not exist.
  */
PageFormat
getPageFormat(int pageIndex) throws IndexOutOfBoundsException;

/*************************************************************************/

/**
  * This method returns the <code>Printable</code> instance for the
  * specified page.  Page numbers start at zero.  An exception is thrown if 
  * the requested page does not exist.
  *
  * @param pageIndex The index of the page to return the 
  * <code>Printable</code> for.
  *
  * @return The <code>Printable</code> for the requested page.
  *
  * @exception IndexOutOfBoundsException If the requested page number does
  * not exist.
  */
Printable
getPrintable(int pageIndex) throws IndexOutOfBoundsException;

} // interface Pageable

