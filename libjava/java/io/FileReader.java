/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 22, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition.
 * Status:  Believed complete and correct.
 */

public class FileReader extends InputStreamReader
{
  public FileReader(String filename) throws IOException
  {
    super(new FileInputStream(filename));
  }

  public FileReader(File file) throws IOException
  {
    super(new FileInputStream(file));
  }

  public FileReader(FileDescriptor fd)
  {
    super(new FileInputStream(fd));
  }
}
