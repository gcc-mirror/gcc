// FileDescriptor.java - Open file or device

/* Copyright (C) 1998, 1999, 2000, 2001  Free Software Foundation

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
 * Status:  Complete to 1.1
 */

// For now we assume a POSIXy file system.  This can be changed later
// if need be.
public final class FileDescriptor
{

  public static final FileDescriptor in = null;
  public static final FileDescriptor out = null;
  public static final FileDescriptor err = null;

  private static native void init();
  static
  {
    init();
  }

  public native void sync () throws SyncFailedException;
  public native boolean valid ();

  // These are mode values for open().
  static final int READ   = 1;
  static final int WRITE  = 2;
  static final int APPEND = 4;
  // EXCL is used only when making a temp file.
  static final int EXCL   = 8;

  // These are WHENCE values for seek.
  static final int SET = 0;
  static final int CUR = 1;

  // This constructor is specified to create an invalid descriptor.
  public FileDescriptor ()
  {
  }

  // Open a file.  MODE is a combination of the above mode flags.
  FileDescriptor (String path, int mode) throws FileNotFoundException
  {
    fd = open (path, mode);
  }

  native int open (String path, int mode) throws FileNotFoundException;
  native void write (int b) throws IOException;
  native void write (byte[] b, int offset, int len)
    throws IOException, NullPointerException, IndexOutOfBoundsException;
  native void close () throws IOException;
  // EOF_TRUNC is true if a request to seek past the end of file
  // should actually stop at the end of file.  If false, then a seek
  // past the end is ok (and if a subsequent write occurs the file
  // will grow).
  native int seek (long pos, int whence, boolean eof_trunc) throws IOException;
  native long length () throws IOException;
  native long getFilePointer () throws IOException;
  native int read () throws IOException;
  native int read (byte[] bytes, int offset, int len) throws IOException;
  native int available () throws IOException;


  // When collected, close.
  protected void finalize () throws IOException
  {
    if (valid ())
      close ();
  }

  // Attach to an already-opened file.  This is not private because we
  // need access to it from other packages, for instance java.net.
  // Ordinarily that wouldn't work, either, but in our case we know
  // the access comes from C++, where "package private" is translated
  // into "public".  Eww.
  FileDescriptor (int desc)
  {
    fd = desc;
  }

  // System's notion of file descriptor.  It might seem redundant to
  // initialize this given that it is reassigned in the constructors.
  // However, this is necessary because if open() throws an exception
  // we want to make sure this has the value -1.  This is the most
  // efficient way to accomplish that.
  private int fd = -1;
}
