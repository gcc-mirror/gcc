/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date April 21, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

public abstract class Reader
{
  protected Object lock;

  protected Reader()
  {
    this.lock = this;
  }

  protected Reader(Object lock)
  {
    this.lock = lock;
  }

  abstract public int read(char buf[], int offset, int count)
    throws IOException;

  public int read(char buf[]) throws IOException
  {
    return read(buf, 0, buf.length);
  }

  public int read() throws IOException
  {
    char[] buf = new char[1];
    int count = read(buf, 0, 1);
    return count > 0 ? buf[0] : -1;
  }

  abstract public void close() throws IOException;

  public boolean markSupported()
  {
    return false;
  }

  public void mark(int readLimit) throws IOException
  {
    throw new IOException("mark not supported");
  }

  public void reset() throws IOException
  {
    throw new IOException("reset not supported");
  }

  public boolean ready() throws IOException
  {
    return false;
  }

  public long skip(long count) throws IOException
  {
    if (count <= 0)
      return 0;
    int bsize = count > 1024 ? 1024 : (int) count;
    char[] buffer = new char[bsize];
    long todo = count;
    while (todo > 0)
      {
	int skipped = read(buffer, 0, bsize > todo ? (int) todo : bsize);
	if (skipped <= 0)
	  break;
	todo -= skipped;
      }
    return count - todo;
  }
}
