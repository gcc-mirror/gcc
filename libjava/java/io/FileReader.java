/* FileReader.java -- Convenience class for reading characters from a file
   Copyright (C) 1998, 2000, 2003 Free Software Foundation, Inc.

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


package java.io;

/**
 * This class provides a convenient way to set up a <code>Reader</code>
 * to read from a file.  It opens the specified file for reading and creates
 * the <code>InputStreamReader</code> to read from the 
 * resulting <code>FileInputStream</code>.  This class can only be used
 * to read from files using the default character encoding.  Use
 * <code>InputStreamReader</code> directly to use a non-default encoding.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class FileReader extends InputStreamReader
{
  /**
   * This method initializes a <code>FileReader</code> instance to read from
   * the specified <code>File</code> object.
   *
   * @param file The <code>File</code> object representing the file to read from
   *
   * @exception FileNotFoundException If the file is not found or some other 
   *            error occurs
   */
  public FileReader(File file) throws FileNotFoundException
  {
    super(new FileInputStream(file));
  }

  /**
   * This method initializes a <code>FileReader</code> instance to read from
   * this specified <code>FileDescriptor</code> object.
   *
   * @param fd The <code>FileDescriptor</code> to read from.
   */
  public FileReader(FileDescriptor fd)
  {
    super(new FileInputStream(fd));
  }

  /**
   * This method initializes a <code>FileReader</code> instance to read from
   * the specified named file.
   *
   * @param name The name of the file to read from
   *
   * @exception FileNotFoundException If the file is not found or some other 
   *            error occurs
   */
  public FileReader(String name) throws FileNotFoundException
  {
    super(new FileInputStream(name));
  }
} // class FileReader

