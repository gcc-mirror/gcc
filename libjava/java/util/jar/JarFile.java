/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.jar;

import java.util.zip.*;
import java.io.File;

/**
 * Does not implement any of the security.  Just a place holder, so
 * that I can implement URLClassLoader.
 *
 * @author Kresten Krab Thorup <krab@gnu.org>
 * @date August 10, 1999.
 */

public class JarFile extends ZipFile
{
  private boolean verify;

  public JarFile (String file) throws java.io.IOException
  {
    super (file);
  }
    
  public JarFile (File file) throws java.io.IOException
  {
    super (file);
  }

  public JarFile (String file, boolean verify) throws java.io.IOException
  {
    super (file);
    this.verify = verify;
  }
    
  public JarFile (File file, boolean verify) throws java.io.IOException
  {
    super (file);
    this.verify = verify;
  }

  public JarEntry getJarEntry (String name)
  {
    ZipEntry ent = getEntry(name);
    if (ent == null)
      return null;
    else
      return new JarEntry(ent);
  }
}
