/* Copyright (C) 1998, 1999, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.io;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 15, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public abstract class FilterReader extends Reader
{
  /* The input stream to be filtered. */
  protected Reader in;

  protected FilterReader(Reader in)
  {
    super(in.lock);
    this.in = in; 
  }

  public void close() throws IOException
  {
    // We used to set `in = null' here.  We don't, though, because
    // that is the simplest way to ensure that read-after-close will
    // throw the appropriate exception -- we rely on the filtered
    // stream to do it.
    in.close();
  }

  public synchronized void mark(int readlimit) throws IOException
  {
    in.mark(readlimit);
  }

  public boolean markSupported()
  {
    return in.markSupported();
  }

  public int read() throws IOException
  {
    return in.read();
  }

  public int read(char[] b, int off, int len) throws IOException
  {
    return in.read(b, off, len);
  }

  public boolean ready() throws IOException
  {
    return in.ready();
  }

  public synchronized void reset() throws IOException
  {
    in.reset();
  }

  public long skip(long n) throws IOException
  {
    return in.skip(n);
  }
}
