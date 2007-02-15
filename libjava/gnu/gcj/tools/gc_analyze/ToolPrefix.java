/* ToolPrefix.java -- Container of the toolPrefix String.
   Copyright (C) 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.tools.gc_analyze;

import java.io.File;

class ToolPrefix
{
  /**
   * Private constructor.  No creation allowed.  This class has
   * Static methods only.
    */
  private ToolPrefix()
  {
  }
  
  static String toolPrefix = "";

  static String pathPrefix = "";
  
  static File fileForName(String filename)
  {
    File f = new File(pathPrefix + filename);
    if (!f.canRead())
      {
        // Try it without the prefix.
        f = new File(filename);
        if (!f.canRead())
          {
            // Try to find it in the current directory.
            f = new File(f.getName());
            if (!f.canRead())
              return null;
          }      
      }
    return f;
  }
}
