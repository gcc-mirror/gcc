/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 8, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public class FilterInputStream extends InputStream
{
  /* The input stream to be filtered. */
  protected InputStream in;

  protected FilterInputStream(InputStream in)
  {
    this.in = in; 
  }

  public int available() throws IOException
  {
    return in.available();
  }

  public void close() throws IOException
  {
    in.close();
  }

  public synchronized void mark(int readlimit)
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

  public int read(byte[] b) throws IOException
  {
    return read(b, 0, b.length);
  }

  public int read(byte[] b, int off, int len) throws IOException
  {
    return in.read(b, off, len);
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
