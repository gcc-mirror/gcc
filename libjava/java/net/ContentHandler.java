/* ContentHandler.java -- Abstract class for handling content from URL's
   Copyright (C) 1998, 1999 2000, 2001 Free Software Foundation, Inc.

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

package java.net;

import java.io.IOException;

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

/**
  * This is an abstract class that is the superclass for classes that read
  * objects from URL's.  Calling the <code>getContent()</code> method in the 
  * <code>URL</code> class or the <code>URLConnection</code> class will cause 
  * an instance of a subclass of <code>ContentHandler</code> to be created for 
  * the MIME type of the object being downloaded from the URL.  Thus, this 
  * class is seldom needed by applications/applets directly, but only 
  * indirectly through methods in other classes.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Warren Levy <warrenl@cygnus.com>
  */
public abstract class ContentHandler
{

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Default, no-argument constructor.
  */
public ContentHandler() { }

/*************************************************************************/

/**
  * This method reads from the <code>InputStream</code> of the passed in URL 
  * connection and uses the data downloaded to create an <code>Object</code> 
  * represening the content.  For example, if the URL is pointing to a GIF 
  * file, this method might return an <code>Image</code> object.  This method 
  * must be implemented by subclasses.
  *
  * @param urlc A <code>URLConnection</code> object to read data from.
  *
  * @return An object representing the data read
  *
  * @exception IOException If an error occurs
  */
public abstract Object getContent(URLConnection urlc) throws IOException;

} // class ContentHandler
