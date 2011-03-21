/* FileWriter.java -- Convenience class for writing to files.
   Copyright (C) 1998, 1999, 2001, 2003, 2004  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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
  * @author Tom Tromey (tromey@cygnus.com)
  */
public class FileWriter extends OutputStreamWriter
{
  /**
   * This method initializes a new <code>FileWriter</code> object to write
   * to the specified <code>File</code> object.
   *
   * @param file The <code>File</code> object to write to.
   *
   * @throws SecurityException If writing to this file is forbidden by the
   * <code>SecurityManager</code>.
   * @throws IOException If any other error occurs
   */
  public FileWriter(File file) throws SecurityException, IOException
  {
    super(new FileOutputStream(file));
  }

  /**
   * This method initializes a new <code>FileWriter</code> object to write
   * to the specified <code>File</code> object.
   *
   * @param file The <code>File</code> object to write to.
   * @param append <code>true</code> to start adding data at the end of the
   * file, <code>false</code> otherwise.
   *
   * @throws SecurityException If writing to this file is forbidden by the
   * <code>SecurityManager</code>.
   * @throws IOException If any other error occurs
   */
  public FileWriter(File file, boolean append) throws IOException
  {
    super(new FileOutputStream(file, append));
  }

  /**
   * This method initializes a new <code>FileWriter</code> object to write
   * to the specified <code>FileDescriptor</code> object.
   *
   * @param fd The <code>FileDescriptor</code> object to write to
   *
   * @throws SecurityException If writing to this file is forbidden by the
   * <code>SecurityManager</code>.
   */
  public FileWriter(FileDescriptor fd) throws SecurityException
  {
    super(new FileOutputStream(fd));
  }

  /**
   * This method intializes a new <code>FileWriter</code> object to
   * write to the
   * specified named file.
   *
   * @param name The name of the file to write to
   *
   * @throws SecurityException If writing to this file is forbidden by the
   * <code>SecurityManager</code>.
   * @throws IOException If any other error occurs
   */
  public FileWriter(String name) throws IOException
  {
    super(new FileOutputStream(name));
  }

  /**
   * This method intializes a new <code>FileWriter</code> object to
   * write to the
   * specified named file.  This form of the constructor allows the caller
   * to determine whether data should be written starting at the beginning or
   * the end of the file.
   *
   * @param name The name of the file to write to
   * @param append <code>true</code> to start adding data at the end of the
   * file, <code>false</code> otherwise.
   *
   * @throws SecurityException If writing to this file is forbidden by the
   * <code>SecurityManager</code>.
   * @throws IOException If any other error occurs
   */
  public FileWriter(String name, boolean append) throws IOException
  {
    super(new FileOutputStream(name, append));
  }
}
