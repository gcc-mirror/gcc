/* Copyright (C) 1999, 2001, 2002, 2003, 2004, 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

package gnu.gcj.runtime;

import java.net.URL;

// The extension loader for libgcj.  Class loader bootstrap is a bit
// tricky, see prims.cc and SystemClassLoader for some details.
public final class ExtensionClassLoader extends HelperClassLoader
{
  private ExtensionClassLoader ()
  {	
  }

  private void init() 
  {
    addDirectoriesFromProperty("java.ext.dirs");
  }

  // This can be package-private because we only call it from native
  // code during startup.
  static void initialize ()
  {
    instance.init();
    system_instance.init();
  }

  // The only ExtensionClassLoader that can exist.
  static ExtensionClassLoader instance = new ExtensionClassLoader();
  // The system class loader.
  static SystemClassLoader system_instance = new SystemClassLoader(instance);
}
