/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.jar;

import java.util.zip.*;

/**
 * Does not implement any of the security.  Just a place holder, so
 * that I can implement URLClassLoader.
 *
 * @author Kresten Krab Thorup <krab@gnu.org>
 * @date August 10, 1999.
 */

public class JarInputStream extends ZipInputStream
{
   public JarEntry getNextJarEntry () throws java.io.IOException
    {
       return new JarEntry (getNextEntry ());
    }

  public JarInputStream (java.io.InputStream is)
  {
    super(is);
  }
}
