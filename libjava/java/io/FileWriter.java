/* FileWriter.java -- Convenience class for writing to files.
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


package java.io;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.1.
 */

/**
  * This is a convenience class for writing to files.  It creates an
  * <code>FileOutputStream</code> and initializes an 
  * <code>OutputStreamWriter</code> to write to it.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey <tromey@cygnus.com>
  */
public class FileWriter extends OutputStreamWriter
{

/*************************************************************************/

/*
 * Constructors
 */
 
/**
  * This method initializes a new <code>FileWriter</code> object to write
  * to the specified <code>File</code> object.
  *
  * @param file The <code>File</code> object to write to.
  *
  * @param SecurityException If writing to this file is forbidden by the
  *                          <code>SecurityManager</code>.
  * @param IOException If any other error occurs
  */
public 
FileWriter(File file) throws SecurityException, IOException
{
  super(new FileOutputStream(file));
}

/*************************************************************************/

/**
  * This method initializes a new <code>FileWriter</code> object to write
  * to the specified <code>FileDescriptor</code> object.
  *
  * @param fd The <code>FileDescriptor</code> object to write to
  *
  * @param SecurityException If writing to this file is forbidden by the
  *                          <code>SecurityManager</code>.
  */
public
FileWriter(FileDescriptor fd) throws SecurityException
{
  super(new FileOutputStream(fd));
}

/*************************************************************************/

/**
  * This method intializes a new <code>FileWriter</code> object to write to the
  * specified named file.
  *
  * @param name The name of the file to write to
  *
  * @param SecurityException If writing to this file is forbidden by the
  *                          <code>SecurityManager</code>.
  * @param IOException If any other error occurs
  */
public
FileWriter(String name) throws IOException
{
  super(new FileOutputStream(name));
}

/*************************************************************************/

/**
  * This method intializes a new <code>FileWriter</code> object to write to the
  * specified named file.  This form of the constructor allows the caller
  * to determin whether data should be written starting at the beginning or
  * the end of the file.
  *
  * @param name The name of the file to write to
  * @param append <code>true</code> to start adding data at the end of the
  *               file, <code>false</code> otherwise.
  *
  * @param SecurityException If writing to this file is forbidden by the
  *                          <code>SecurityManager</code>.
  * @param IOException If any other error occurs
  */
public
FileWriter(String name, boolean append) throws IOException
{
  super(new FileOutputStream(name, append));
}

} // class FileWriter

