/* Book.java -- A mixed group of pages to print.
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

import java.util.Vector;

/**
  * This class allows documents to be created with different paper types,
  * page formatters, and painters.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class Book implements Pageable
{

/*
 * Instance Variables
 */

// Painter objects for the book
Vector printables = new Vector();

// Page formats for the book
Vector page_formats = new Vector();

/*************************************************************************/

/*
 * Constructors
 */

/** 
  * Initializes a new instance of <code>Book</code> that is empty.
  */
public
Book()
{
  ;
}

/*************************************************************************/

/**
  * Returns the number of pages in this book.
  *
  * @return The number of pages in this book.
  */
public int
getNumberOfPages()
{
  return(printables.size());
}

/*************************************************************************/

/**
  * This method returns the <code>PageFormat</code> object for the
  * specified page.
  *
  * @param page_numbers The number of the page to get information for, where
  * page numbers start at 0.
  *
  * @return The <code>PageFormat</code> object for the specified page.
  *
  * @exception IndexOutOfBoundsException If the page number is not valid.
  */
public PageFormat
getPageFormat(int page_number)
{
  return((PageFormat)page_formats.elementAt(page_number));
}

/*************************************************************************/

/**
  * This method returns the <code>Printable</code> object for the
  * specified page.
  *
  * @param page_numbers The number of the page to get information for, where
  * page numbers start at 0.
  *
  * @return The <code>Printable</code> object for the specified page.
  *
  * @exception IndexOutOfBoundsException If the page number is not valid.
  */
public Printable
getPrintable(int page_number)
{
  return((Printable)printables.elementAt(page_number));
}

/*************************************************************************/

/**
  * This method appends a page to the end of the book.
  *
  * @param printable The <code>Printable</code> for this page.
  * @param page_format The <code>PageFormat</code> for this page.
  *
  * @exception NullPointerException If either argument is <code>null</code>.
  */
public void
append(Printable printable, PageFormat page_format)
{
  append(printable, page_format, 1);
} 

/*************************************************************************/

/**
  * This method appends the specified number of pages to the end of the book.
  * Each one will be associated with the specified <code>Printable</code>
  * and <code>PageFormat</code>.
  *
  * @param printable The <code>Printable</code> for this page.
  * @param page_format The <code>PageFormat</code> for this page.
  * @param num_pages The number of pages to append.
  *
  * @exception NullPointerException If any argument is <code>null</code>.
  */
public void
append(Printable painter, PageFormat page_format, int num_pages)
{
  for (int i = 0; i < num_pages; i++)
    {
      printables.addElement(painter);
      page_formats.addElement(page_format);
    }
}

/*************************************************************************/

/**
  * This method changes the <code>Printable</code> and <code>PageFormat</code>
  * for the specified page.  The page must already exist or an exception
  * will be thrown.
  *
  * @param page_num The page number to alter.
  * @param printable The new <code>Printable</code> for the page.
  * @param page_format The new <code>PageFormat</code> for the page.
  *
  * @param IndexOutOfBoundsException If the specified page does not exist.
  */
public void
setPage(int page_num, Printable printable, PageFormat page_format)
{
  printables.setElementAt(printable, page_num);
  page_formats.setElementAt(page_format, page_num);
}

} // class Book

