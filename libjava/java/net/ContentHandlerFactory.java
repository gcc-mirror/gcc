/* ContentHandlerFactory.java -- Interface for creating content handlers
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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


package java.net;

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

/**
  * This interface maps MIME types to ContentHandler objects.  It consists
  * of one method that, when passed a MIME type, returns a handler for that
  * type.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Warren Levy <warrenl@cygnus.com>
  */
public interface ContentHandlerFactory
{
/**
  * This method is passed a MIME type as a string and is responsible for
  * returning the appropriate ContentType object.
  *
  * @param mime_type The MIME type to map to a ContentHandler
  *
  * @return The ContentHandler for the passed in MIME type
  */
ContentHandler
createContentHandler(String mime_type);

} // interface ContentHandlerFactory

