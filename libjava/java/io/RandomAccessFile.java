/* RandomAccessFile.java -- Class supporting random file I/O
   Copyright (C) 1998, 1999, 2001, 2002, 2003 Free Software Foundation, Inc.

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
import gnu.java.nio.FileChannelImpl;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date September 25, 1998 
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status: Believe complete and correct to 1.1.
 */

public class RandomAccessFile implements DataOutput, DataInput
{
  public void close () throws IOException
  {
    if (fd.valid())
      fd.close();
  }

  public final FileDescriptor getFD () throws IOException
  {
    if (! fd.valid())
      throw new IOException ();
    return fd;
  }

  public long getFilePointer () throws IOException
  {
    return fd.getFilePointer();
  }

  public void setLength (long pos) throws IOException
  {
    fd.setLength(pos);
  }

  public long length () throws IOException
  {
    return fd.length();
  }

  public RandomAccessFile (String fileName, String mode)
    throws FileNotFoundException
  {
    int fdmode;
    if (mode.compareTo ("r") == 0)
      fdmode = FileDescriptor.READ;
    else if (mode.compareTo ("rw") == 0)
      fdmode = FileDescriptor.READ | FileDescriptor.WRITE;
    else
      throw new IllegalArgumentException ("invalid mode: " + mode);

    SecurityManager s = System.getSecurityManager();
    if (s != null)
      {
	s.checkRead(fileName);
	if ((fdmode & FileDescriptor.WRITE) != 0)
	  s.checkWrite(fileName);
      }

    fd = new FileDescriptor (fileName, fdmode);
    out = new DataOutputStream (new FileOutputStream (fd));
    in = new DataInputStream (new FileInputStream (fd));
  }

  public RandomAccessFile (File file, String mode) throws FileNotFoundException
  {
    this (file.getPath(), mode);
  }

  public int read () throws IOException
  {
    return in.read();
  }

  public int read (byte[] buffer) throws IOException
  {
    return in.read(buffer);
  }

  public int read (byte[] buffer, int offset, int count) throws IOException
  {
    return in.read(buffer, offset, count);
  }

  public final boolean readBoolean () throws IOException
  {
    return in.readBoolean();
  }

  public final byte readByte () throws IOException
  {
    return in.readByte();
  }

  public final char readChar () throws IOException
  {
    return in.readChar();
  }

  public final double readDouble () throws IOException
  {
    return in.readDouble();
  }

  public final float readFloat () throws IOException
  {
    return in.readFloat();
  }

  public final void readFully (byte[] buffer) throws IOException
  {
    in.readFully(buffer);
  }

  public final void readFully (byte[] buffer, int offset, int count)
    throws IOException
  {
    in.readFully(buffer, offset, count);
  }

  public final int readInt () throws IOException
  {
    return in.readInt();
  }

  public final String readLine () throws IOException
  {
    return in.readLine();
  }

  public final long readLong () throws IOException
  {
    return in.readLong();
  }

  public final short readShort () throws IOException
  {
    return in.readShort();
  }

  public final int readUnsignedByte () throws IOException
  {
    return in.readUnsignedByte();
  }

  public final int readUnsignedShort () throws IOException
  {
    return in.readUnsignedShort();
  }

  public final String readUTF () throws IOException
  {
    return in.readUTF();
  }

  public void seek (long pos) throws IOException
  {
    fd.seek(pos, FileDescriptor.SET, false);
  }

  public int skipBytes (int count) throws IOException
  {
    if (count <= 0)
      return 0;
    long startPos = fd.getFilePointer();
    long endPos = fd.seek(count, FileDescriptor.CUR, true);
    return (int) (endPos - startPos);
  }

  public void write (int oneByte) throws IOException
  {
    out.write(oneByte);
  }

  public void write (byte[] buffer) throws IOException
  {
    out.write(buffer);
  }

  public void write (byte[] buffer, int offset, int count) throws IOException
  {
    out.write(buffer, offset, count);
  }

  public final void writeBoolean (boolean val) throws IOException
  {
    out.writeBoolean(val);
  }

  public final void writeByte (int v) throws IOException
  {
    out.writeByte(v);
  }

  public final void writeShort (int v) throws IOException
  {
    out.writeShort(v);
  }

  public final void writeChar (int v) throws IOException
  {
    out.writeChar(v);
  }

  public final void writeInt (int v) throws IOException
  {
    out.writeInt(v);
  }

  public final void writeLong (long v) throws IOException
  {
    out.writeLong(v);
  }

  public final void writeFloat (float v) throws IOException
  {
    out.writeFloat(v);
  }

  public final void writeDouble (double v) throws IOException
  {
    out.writeDouble(v);
  }

  public final void writeBytes (String s) throws IOException
  {
    out.writeBytes(s);
  }

  public final void writeChars (String s) throws IOException
  {
    out.writeChars(s);
  }
  
  public final void writeUTF (String s) throws IOException
  {
    out.writeUTF(s);
  }

  public FileChannel getChannel ()
  {
    synchronized (this)
      {
        if (ch == null)
          ch = new FileChannelImpl (fd, true, this);

	return ch;
      }
  }

  // The underlying file.
  private FileDescriptor fd;
  // The corresponding input and output streams.
  private DataOutputStream out;
  private DataInputStream in;
  private FileChannel ch;
}
