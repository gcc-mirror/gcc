/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.jar;

import java.util.zip.*;

/**
 * Does not implement the security and manifest methods.
 *
 * @author Kresten Krab Thorup <krab@gnu.org>
 * @date August 10, 1999.
 */

public class JarEntry extends ZipEntry
{
  ZipEntry zip;

  public JarEntry (ZipEntry ent)
  {
    super (ent);
  }

  public JarEntry (JarEntry ent)
  {
    super (ent);
  }

  public JarEntry (String name)
  {
    super (name);
  }

}
