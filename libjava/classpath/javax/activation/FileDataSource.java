/* FileDataSource.java -- Data source for a File object.
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package javax.activation;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * Data source encapsulating a file.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 * @version 1.1
 */
public class FileDataSource
  implements DataSource
{

  private final File file;
  private FileTypeMap typeMap;

  /**
   * Constructor.
   * @param file the underlying file to use
   */
  public FileDataSource(File file)
  {
    this.file = file;
  }

  /**
   * Constructor.
   * @param name the path to the underlying file to use
   */
  public FileDataSource(String name)
  {
    this(new File(name));
  }

  public InputStream getInputStream()
    throws IOException
  {
    return new FileInputStream(file);
  }

  public OutputStream getOutputStream()
    throws IOException
  {
    return new FileOutputStream(file);
  }

  public String getContentType()
  {
    if (typeMap == null)
      {
        FileTypeMap dftm = FileTypeMap.getDefaultFileTypeMap();
        return dftm.getContentType(file);
      }
    return typeMap.getContentType(file);
  }

  public String getName()
  {
    return file.getName();
  }

  /**
   * Returns the underlying file.
   */
  public File getFile()
  {
    return file;
  }

  /**
   * Sets the file type map to use to determine the content type of the file.
   * @param map the file type map
   */
  public void setFileTypeMap(FileTypeMap map)
  {
    typeMap = map;
  }

}
