/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.zip;
import java.io.*;

/** JUST AN INCOMPLETE STUB! */

public class ZipOutputStream extends DeflaterOutputStream
  implements ZipConstants
{
  public ZipOutputStream (OutputStream out)
  {
    super(out);
  }

  public void putNextEntry (ZipEntry entry) throws IOException
  {
    throw new Error ("java.util.zip.ZipOutputStream.putNextEntry:  not implemented");
  }
}
