/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.zip;

interface ZipConstants
{
  // Size in bytes of local file header, including signature.
  public static final int LOCAL_FILE_HEADER_SIZE = 30;

  // Size in bytes of the "end of central directory" record, with signature.
  public static final int END_CENTRAL_DIR_SIZE = 22;
}
