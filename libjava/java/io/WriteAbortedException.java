/* WriteAbortedException.java -- An exception occurred while writing a 
   serialization stream
   Copyright (C) 1998, 2000 Free Software Foundation, Inc.

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


package java.io;

/**
  * This exception is thrown when one of the other ObjectStreamException 
  * subclasses was thrown during a serialization write.
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class WriteAbortedException extends ObjectStreamException
{

/*
 * Instance Variables
 */

/**
  * The detailed exception that caused this exception to be thrown
  */
public Exception detail;
private transient String message;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Create a new WriteAbortedException with an eof parameter indicating
  * the detailed Exception that caused this exception to be thrown.
  *
  * @param detail The exception that caused this exception to be thrown
  */
public
WriteAbortedException(String msg, Exception detail)
{
  this.message = msg;
  this.detail = detail;
}

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * This method returns a message indicating what went wrong, including 
  * the message text from the initial exception that caused this one to
  * be thrown
  */
public String
getMessage()
{
  return(message + ": " + detail.getMessage());
}

} // class WriteAbortedException

