// FileWriter.java - Character output to a file.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.io;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date September 25, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.1.
 */

public class FileWriter extends OutputStreamWriter
{
  public FileWriter (String fileName) throws IOException
  {
    super (new FileOutputStream (fileName));
  }

  public FileWriter (String fileName, boolean append) throws IOException
  {
    super (new FileOutputStream (fileName, append));
  }

  public FileWriter (File file) throws IOException
  {
    super (new FileOutputStream (file));
  }

  public FileWriter (FileDescriptor fd)
  {
    super (new FileOutputStream (fd));
  }
}
