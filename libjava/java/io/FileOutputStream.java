// FileOutputStream.java - Write bytes to a file.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.io;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date September 24, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.1.
 */

public class FileOutputStream extends OutputStream
{
  public FileOutputStream (String path, boolean append)
    throws SecurityException, IOException
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkWrite(path);
    fd = new FileDescriptor (path, (append
				    ? FileDescriptor.APPEND
				    : FileDescriptor.WRITE));
  }

  public FileOutputStream (String path) throws SecurityException, IOException
  {
    this (path, false);
  }

  public FileOutputStream (File file) throws SecurityException, IOException
  {
    this (file.getPath(), false);
  }

  public FileOutputStream (FileDescriptor fdObj) throws SecurityException
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkWrite(fdObj);
    fd = fdObj;
  }

  protected void finalize () throws IOException
  {
    // We don't actually need this, but we include it because it is
    // mentioned in the JCL.
  }

  public final FileDescriptor getFD () throws IOException
  {
    if (! fd.valid())
      throw new IOException ();
    return fd;
  }

  public void write (int b) throws IOException
  {
    fd.write (b);
  }

  public void write (byte[] b) throws IOException, NullPointerException
  {
    fd.write (b, 0, b.length);
  }

  public void write (byte[] b, int off, int len)
    throws IOException, NullPointerException, IndexOutOfBoundsException
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException ();
    fd.write (b, off, len);
  }

  public void close () throws IOException
  {
    if (fd.valid())
      fd.close();
  }

  // Instance variables.
  private FileDescriptor fd;
}
