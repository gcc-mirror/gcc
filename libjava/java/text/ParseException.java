/* ParseException.java -- An error occurred while parsing.
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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

/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

/**
  * This exception is thrown when an unexpected error occurs during parsing.
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Per Bothner <bothner@cygnus.com>
  * @date October 25, 1998.
  */
public class ParseException extends Exception
{

/*
 * Instance Variables
 */

/**
  * This is the position where the error was encountered.
  */
private int errorOffset;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * This method initializes a new instance of <code>ParseException</code>
  * with a detailed error message and a error position.
  *
  * @param msg The descriptive message describing the error.
  * @param offset The position where the error was encountered.
  */
public
ParseException(String s, int offset)
{
  super(s);
  
  errorOffset = offset;
}

/*************************************************************************/

/**
  * This method returns the position where the error occurred.
  * 
  * @return The position where the error occurred.
  */
public int
getErrorOffset()
{
  return(errorOffset);
}

} // class ParseException

