/* FileOutputStream.java -- Writes to a file on disk.
   Copyright (C) 1998, 2001, 2003 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package java.io;

import java.nio.channels.FileChannel;
import java.nio.channels.FileChannelImpl;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.1.
 */

/**
 * This classes allows a stream of data to be written to a disk file or
 * any open <code>FileDescriptor</code>.
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @author Tom Tromey <tromey@cygnus.com>
 */
public class FileOutputStream extends OutputStream
{
  private FileDescriptor fd;

  private FileChannel ch; /* cached associated file-channel */

  /**
   * This method initializes a <code>FileOutputStream</code> object to write
   * to the named file.  The file is created if it does not exist, and
   * the bytes written are written starting at the beginning of the file if
   * the <code>append</code> argument is <code>false</code> or at the end
   * of the file if the <code>append</code> argument is true.
   * <p>
   * Before opening a file, a security check is performed by calling the
   * <code>checkWrite</code> method of the <code>SecurityManager</code> (if
   * one exists) with the name of the file to be opened.  An exception is
   * thrown if writing is not allowed. 
   *
   * @param name The name of the file this stream should write to
   * @param append <code>true</code> to append bytes to the end of the file,
   * or <code>false</code> to write bytes to the beginning
   *
   * @exception SecurityException If write access to the file is not allowed
   * @exception FileNotFoundException If a non-security error occurs
   */
  public FileOutputStream (String path, boolean append)
    throws SecurityException, FileNotFoundException
  {
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkWrite(path);
    fd = new FileDescriptor (path, (append
				    ? FileDescriptor.WRITE
				      | FileDescriptor.APPEND
				    : FileDescriptor.WRITE));
  }

  /**
   * This method initializes a <code>FileOutputStream</code> object to write
   * to the named file.  The file is created if it does not exist, and
   * the bytes written are written starting at the beginning of the file.
   * <p>
   * Before opening a file, a security check is performed by calling the
   * <code>checkWrite</code> method of the <code>SecurityManager</code> (if
   * one exists) with the name of the file to be opened.  An exception is
   * thrown if writing is not allowed. 
   *
   * @param name The name of the file this stream should write to
   *
   * @exception SecurityException If write access to the file is not allowed
   * @exception FileNotFoundException If a non-security error occurs
   */
  public FileOutputStream (String path)
    throws SecurityException, FileNotFoundException
  {
    this (path, false);
  }

  /**
   * This method initializes a <code>FileOutputStream</code> object to write
   * to the specified <code>File</code> object.  The file is created if it 
   * does not exist, and the bytes written are written starting at the 
   * beginning of the file.
   * <p>
   * Before opening a file, a security check is performed by calling the
   * <code>checkWrite</code> method of the <code>SecurityManager</code> (if
   * one exists) with the name of the file to be opened.  An exception is
   * thrown if writing is not allowed. 
   *
   * @param file The <code>File</code> object this stream should write to
   *
   * @exception SecurityException If write access to the file is not allowed
   * @exception FileNotFoundException If a non-security error occurs
   */
  public FileOutputStream (File file)
    throws SecurityException, FileNotFoundException
  {
    this (file.getPath(), false);
  }

  /**
   * This method initializes a <code>FileOutputStream</code> object to write
   * to the specified <code>File</code> object.  The file is created if it 
   * does not exist, and the bytes written are written starting at the 
   * beginning of the file if the <code>append</code> parameter is 
   * <code>false</code>.  Otherwise bytes are written at the end of the
   * file.
   * <p>
   * Before opening a file, a security check is performed by calling the
   * <code>checkWrite</code> method of the <code>SecurityManager</code> (if
   * one exists) with the name of the file to be opened.  An exception is
   * thrown if writing is not allowed. 
   *
   * @param file The <code>File</code> object this stream should write to
   * @param append <code>true</code> to append bytes to the end of the file,
   * or <code>false</code> to write bytes to the beginning
   *
   * @exception SecurityException If write access to the file is not allowed
   * @exception FileNotFoundException If a non-security error occurs
   */
  public FileOutputStream (File file, boolean append)
    throws FileNotFoundException
  {
    this (file.getPath(), append);
  }

  /**
   * This method initializes a <code>FileOutputStream</code> object to write
   * to the file represented by the specified <code>FileDescriptor</code>
   * object.  This method does not create any underlying disk file or
   * reposition the file pointer of the given descriptor.  It assumes that
   * this descriptor is ready for writing as is.
   * <p>
   * Before opening a file, a security check is performed by calling the
   * <code>checkWrite</code> method of the <code>SecurityManager</code> (if
   * one exists) with the specified <code>FileDescriptor</code> as an argument.
   * An exception is thrown if writing is not allowed. 
   *
   * @param file The <code>FileDescriptor</code> this stream should write to
   *
   * @exception SecurityException If write access to the file is not allowed
   */
  public FileOutputStream (FileDescriptor fdObj)
    throws SecurityException
  {
    // Hmm, no other exception but this one to throw, but if the descriptor
    // isn't valid, we surely don't have "permission" to write to it.
    if (!fdObj.valid())
      throw new SecurityException("Invalid FileDescriptor");

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

  /**
   * This method returns a <code>FileDescriptor</code> object representing
   * the file that is currently being written to
   *
   * @return A <code>FileDescriptor</code> object for this stream
   *
   * @exception IOException If an error occurs
   */
  public final FileDescriptor getFD () throws IOException
  {
    if (! fd.valid())
      throw new IOException ();
    return fd;
  }

  /**
   * This method writes a single byte of data to the file.  
   *
   * @param b The byte of data to write, passed as an <code>int</code>
   *
   * @exception IOException If an error occurs
   */
  public void write (int b) throws IOException
  {
    fd.write (b);
  }

  /**
   * This method writes all the bytes in the specified array to the
   * file.
   *
   * @param buf The array of bytes to write to the file
   *
   * @exception IOException If an error occurs
   */
  public void write (byte[] buf)
    throws IOException
  {
    write (buf, 0, buf.length);
  }

  /**
   * This method writes <code>len</code> bytes from the byte array 
   * <code>buf</code> to the file starting at index <code>offset</code>.
   *
   * @param buf The array of bytes to write to the file
   * @param offset The offset into the array to start writing bytes from
   * @param len The number of bytes to write to the file
   *
   * @exception IOException If an error occurs
   */
  public void write (byte[] buf, int offset, int len)
    throws IOException
  {
    if (offset < 0
        || len < 0
        || offset + len > buf.length)
      throw new ArrayIndexOutOfBoundsException ();
    
    fd.write (buf, offset, len);
  }

  /**
   * This method closes the underlying file.  Any further attempts to
   * write to this stream will likely generate an exception since the
   * file is closed.
   *
   * @exception IOException If an error occurs
   */
  public void close () throws IOException
  {
    if (fd.valid())
      fd.close();
  }

  /**
   * This method creates a java.nio.channels.FileChannel.
   * Nio does not allow one to create a file channel directly.
   * A file channel must be created by first creating an instance of
   * Input/Output/RandomAccessFile and invoking the getChannel() method on it.
   */
  public synchronized FileChannel getChannel() 
  {
    if (ch == null)
      ch = new FileChannelImpl (fd, true, this);

    return ch;
  }

} // class FileOutputStream

