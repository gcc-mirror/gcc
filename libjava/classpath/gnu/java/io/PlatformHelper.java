/* PlatformHelper.java -- Isolate OS-specific IO helper methods and variables
   Copyright (C) 1998, 2002, 2006 Free Software Foundation, Inc.

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

package gnu.java.io;

import java.util.StringTokenizer;

/**
 * We had many changes in File.java, URLStreamHandler.java etc. to handle
 * path representations on different platforms (Windows/Unix-family).
 * Finally we'd like to collect all these ad hoc codes into this utility class.
 *       --Gansha
 */
public class PlatformHelper
{
  public static final boolean isWindows = System.getProperty("os.name").indexOf("Windows") >= 0;
  public static final String separator = System.getProperty("file.separator");
  public static final char separatorChar = separator.charAt(0);
  public static final String pathSeparator = System.getProperty("path.separator");
  public static final char pathSeparatorChar = pathSeparator.charAt(0);

  /**
   * On most platforms 260 is equal or greater than a max path value, 
   * so we can set the initial buffer size of StringBuffer to half of this value
   * to improve performance.
   */
  public static final int INITIAL_MAX_PATH = 260/2;

  /**
   * This routine checks the input param "path" whether it begins with root path
   * prefix.
   * if not, return 0;
   * if yes, return the len of root path prefix;
   *   --for Unix-family platform, root path begins with "/" and len is 1
   *   --for Windows platform, root path begins with "drive:\\" and len is 3
   */
  public static final int beginWithRootPathPrefix(String path)
  {
    if (path.startsWith("/") || path.startsWith("\\"))
      return 1;

    if (!isWindows)
      return 0;

    if (path.length() > 2
        && Character.isLetter(path.charAt(0))
        && path.charAt(1) == ':'
        && (path.charAt(2) == '/' || path.charAt(2) == '\\'))
      return 3;

    return 0;
  }

  /**
   * This routine checks the input param "path" whether it's root directory.
   *  --for Unix-family platform, root directory is "/"
   *  --for Windows platform, root directory is "\\" or "drive:\\".
   */
  public static final boolean isRootDirectory(String path)
  {
    int len = path.length();
    return len > 0 && beginWithRootPathPrefix(path) == len;
  }

  /**
   * This routine checks whether input param "path" ends with separator
   */
  public static final boolean endWithSeparator(String path)
  {
    if (path.endsWith("\\") || path.endsWith("/"))
      return true;

    return false;
  }

  /**
   * This routine removes from input param "path" the tail separator if it exists, 
   * and return the remain part.
   */
  public static final String removeTailSeparator(String path)
  {
    if (endWithSeparator(path) && !isRootDirectory(path))
      return path.substring(0, path.length() - 1);

    return path;
  }

  /**
   * This routine returns last index of separator in input param "path", 
   * and return it.
   */
  public static final int lastIndexOfSeparator(String path)
  {
    return Math.max(path.lastIndexOf("/"), path.lastIndexOf("\\"));
  }

}
