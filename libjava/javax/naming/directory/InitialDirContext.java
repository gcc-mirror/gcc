/* Copyright (C) 2000 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package javax.naming.directory;

import javax.naming.*;
import java.util.Hashtable;

public class InitialDirContext extends InitialContext implements DirContext
{
  public InitialDirContext (Hashtable environment)
  {
    throw new Error ("javax.naming.directory.InitialDirContext not implemented");
  }
}
