/* Copyright (C) 1998, 1999, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package java.io;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date October 28, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
public class FileInputStream extends InputStream
{
  /* Contains the file descriptor for referencing the actual file. */
  private FileDescriptor fd;

  public FileInputStream(String name) throws FileNotFoundException
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkRead(name);
    fd = new FileDescriptor(name, FileDescriptor.READ);
  }

  public FileInputStream(File file) throws FileNotFoundException
  {
    this(file.getPath());
  }

  public FileInputStream(FileDescriptor fdObj)
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkRead(fdObj);
    fd = fdObj;
  }

  public int available() throws IOException
  {
    return fd.available();
  }

  public void close() throws IOException
  {
    if (fd.valid())
      fd.close();
  }

  protected void finalize() throws IOException
  {
    if (fd != null)
      fd.finalize();
  }

  public final FileDescriptor getFD() throws IOException
  {
    if (!fd.valid())
      throw new IOException();
    return fd;
  }

  public int read() throws IOException
  {
    return fd.read();
  }

  public int read(byte[] b) throws IOException
  {
    return fd.read(b, 0, b.length);
  }

  public int read(byte[] b, int off, int len) throws IOException
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException();

    return fd.read(b, off, len);
  }

  public long skip(long n) throws IOException
  {
    long startPos = fd.getFilePointer();
    long endPos = fd.seek(n, FileDescriptor.CUR, true);
    return endPos - startPos;
  }
}
