/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 17, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 * However, write(String, int, int) should be made a native method.
 */

public abstract class Writer
{
  protected Object lock;

  protected Writer ()
  {
    lock = this;
  }

  protected Writer (Object lock)
  {
    this.lock = lock;
  }

  abstract public void close() throws IOException;

  abstract public void flush() throws IOException;

  abstract public void write(char[] buf, int offset, int count)
     throws IOException;

  public void write(char[] buf) throws IOException
  {
    write(buf, 0, buf.length);
  }

  public void write(int ch) throws IOException
  {
    char[] buf = new char[1];
    buf[0] = (char) ch;
    write(buf, 0, 1);
  }

  // FIXME - re-write using native code to not require copied buffer.
  public void write (String str, int offset, int count) throws IOException
  {
    char[] buf = new char[count];
    str.getChars(offset, offset + count, buf, 0);
    write(buf, 0, count);
  }

  public void write (String str) throws IOException
  {
    write(str, 0, str.length());
  }

}
