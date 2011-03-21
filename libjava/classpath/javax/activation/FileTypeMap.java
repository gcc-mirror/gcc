/* FileTypeMap.java -- Classifies the MIME content of files.
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

/**
 * Classifier for the MIME content type of files.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 * @version 1.1
 */
public abstract class FileTypeMap
{

  /* Class scope */

  private static FileTypeMap defaultMap;

  /**
   * Returns the system default file type map.
   * If one has not been set, this returns a MimetypesFileTypeMap.
   */
  public static FileTypeMap getDefaultFileTypeMap()
  {
    if (defaultMap == null)
      {
        defaultMap = new MimetypesFileTypeMap();
      }
    return defaultMap;
  }

  /**
   * Sets the default file type map.
   * @param map the new file type map
   */
  public static void setDefaultFileTypeMap(FileTypeMap map)
  {
    SecurityManager security = System.getSecurityManager();
    if (security != null)
      {
        try
          {
            security.checkSetFactory();
          }
        catch (SecurityException e)
          {
            if (map != null && FileTypeMap.class.getClassLoader() !=
                map.getClass().getClassLoader())
              {
                throw e;
              }
          }
      }
    defaultMap = map;
  }

  /* Instance scope */

  /**
   * Returns the content type of the specified file.
   * @param file the file to classify
   */
  public abstract String getContentType(File file);

  /**
   * Returns the content type of the specified file path.
   * @param filename the path of the file to classify
   */
  public abstract String getContentType(String filename);

}
