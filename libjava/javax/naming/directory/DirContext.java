/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.directory;

import javax.naming.*;

public interface DirContext extends Context
{
  public Attributes getAttributes (String name);
  public Attributes getAttributes (String name, String[] attrIds);
}

