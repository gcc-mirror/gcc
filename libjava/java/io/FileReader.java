/* FileReader.java -- Convenience class for reading characters from a file
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
 * This class provides a convenient way to set up a <code>Reader</code>
 * to read from a file.  It opens the specified file for reading and creates
 * the <code>InputStreamReader</code> to read from the 
 * resulting <code>FileInputStream</code>.  This class can only be used
 * to read from files using the default character encoding.  Use
 * <code>InputStreamReader</code> directly to use a non-default encoding.
 *
 * @version 0.0
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
}
