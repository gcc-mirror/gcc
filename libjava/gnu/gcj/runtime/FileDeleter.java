/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;

import java.io.*;
import java.util.*;

public final class FileDeleter
{
  public synchronized static void add (File f)
  {
    if (deleteOnExitStack == null)
      deleteOnExitStack = new Stack ();

    deleteOnExitStack.push (f);
  }

  // Helper method called by java.lang.Runtime.exit() to perform
  // pending deletions.
  public synchronized static void deleteOnExitNow ()
  {
    if (deleteOnExitStack != null)
      while (!deleteOnExitStack.empty ())
	((File)(deleteOnExitStack.pop ())).delete ();
  }

  // A stack of files to delete upon normal termination.
  private static Stack deleteOnExitStack;
}
