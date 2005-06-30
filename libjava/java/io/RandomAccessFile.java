/* RandomAccessFile.java -- Class supporting random file I/O
   Copyright (C) 1998, 1999, 2001, 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

import gnu.java.nio.channels.FileChannelImpl;

import java.nio.channels.FileChannel;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status: Believe complete and correct to 1.1.
 */

/**
 * This class allows reading and writing of files at random locations.
 * Most Java I/O classes are either pure sequential input or output.  This
 * class fulfills the need to be able to read the bytes of a file in an
 * arbitrary order.  In addition, this class implements the
 * <code>DataInput</code> and <code>DataOutput</code> interfaces to allow
 * the reading and writing of Java primitives.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Tom Tromey (tromey@cygnus.com)
 */
public class RandomAccessFile implements DataOutput, DataInput
{

  // The underlying file.
  private FileChannelImpl ch;
  private FileDescriptor fd;
  // The corresponding input and output streams.
  private DataOutputStream out;
  private DataInputStream in;
  
  
  /**
   * This method initializes a new instance of <code>RandomAccessFile</code>
   * to read from the specified <code>File</code> object with the specified 
   * access mode.   The access mode is either "r" for read only access or "rw" 
   * for read-write access.
   * <p>
   * Note that a <code>SecurityManager</code> check is made prior to
   * opening the file to determine whether or not this file is allowed to
   * be read or written.
   *
   * @param file The <code>File</code> object to read and/or write.
   * @param mode "r" for read only or "rw" for read-write access to the file
   *
   * @exception IllegalArgumentException If <code>mode</code> has an 
   * illegal value
   * @exception SecurityException If the requested access to the file 
   * is not allowed
   * @exception FileNotFoundException If the file is a directory, or 
   * any other error occurs
   */
  public RandomAccessFile (File file, String mode)
    throws FileNotFoundException
  {
    int fdmode;
    if (mode.equals("r"))
      fdmode = FileChannelImpl.READ;
    else if (mode.equals("rw"))
      fdmode = FileChannelImpl.READ | FileChannelImpl.WRITE;
    else if (mode.equals("rws"))
      {
	fdmode = (FileChannelImpl.READ | FileChannelImpl.WRITE
		  | FileChannelImpl.SYNC);
      }
    else if (mode.equals("rwd"))
      {
	fdmode = (FileChannelImpl.READ | FileChannelImpl.WRITE
		  | FileChannelImpl.DSYNC);
      }
    else
      throw new IllegalArgumentException ("invalid mode: " + mode);

    final String fileName = file.getPath();

    // The obligatory SecurityManager stuff
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      {
        s.checkRead(fileName);

        if ((fdmode & FileChannelImpl.WRITE) != 0)
          s.checkWrite(fileName);
      }

    ch = new FileChannelImpl (file, fdmode);
    fd = new FileDescriptor(ch);
    out = new DataOutputStream (new FileOutputStream (fd));
    in = new DataInputStream (new FileInputStream (fd));
  }

  /**
   * This method initializes a new instance of <code>RandomAccessFile</code>
   * to read from the specified file name with the specified access mode.
   * The access mode is either "r" for read only access, "rw" for read
   * write access, "rws" for synchronized read/write access of both
   * content and metadata, or "rwd" for read/write access
   * where only content is required to be synchronous.
   * <p>
   * Note that a <code>SecurityManager</code> check is made prior to
   * opening the file to determine whether or not this file is allowed to
   * be read or written.
   *
   * @param fileName The name of the file to read and/or write
   * @param mode "r", "rw", "rws", or "rwd"
   *
   * @exception IllegalArgumentException If <code>mode</code> has an 
   * illegal value
   * @exception SecurityException If the requested access to the file 
   * is not allowed
   * @exception FileNotFoundException If the file is a directory or 
   * any other error occurs
   */
  public RandomAccessFile (String fileName, String mode)
    throws FileNotFoundException
  {
    this (new File(fileName), mode);
  }

  /**
   * This method closes the file and frees up all file related system
   * resources.  Since most operating systems put a limit on how many files
   * may be opened at any given time, it is a good idea to close all files
   * when no longer needed to avoid hitting this limit
   */
  public void close () throws IOException
  {
    ch.close();
  }

  /**
   * This method returns a <code>FileDescriptor</code> object that 
   * represents the native file handle for this file.
   *
   * @return The <code>FileDescriptor</code> object for this file
   *
   * @exception IOException If an error occurs
   */
  public final FileDescriptor getFD () throws IOException
  {
    synchronized (this)
      {
	if (fd == null)
	  fd = new FileDescriptor (ch);
	return fd;
      }
  }

  /**
   * This method returns the current offset in the file at which the next
   * read or write will occur
   *
   * @return The current file position
   *
   * @exception IOException If an error occurs
   */
  public long getFilePointer () throws IOException
  {
    return ch.position();
  }

  /**
   * This method sets the length of the file to the specified length.
   * If the currently length of the file is longer than the specified
   * length, then the file is truncated to the specified length (the
   * file position is set to the end of file in this case).  If the
   * current length of the file is shorter than the specified length,
   * the file is extended with bytes of an undefined value (the file
   * position is unchanged in this case).
   * <p>
   * The file must be open for write access for this operation to succeed.
   *
   * @param newLen The new length of the file
   *
   * @exception IOException If an error occurs
   */
  public void setLength (long newLen) throws IOException
  {
    // FIXME: Extending a file should probably be done by one method call.

    // FileChannel.truncate() can only shrink a file.
    // To expand it we need to seek forward and write at least one byte.
    if (newLen < length())
      ch.truncate (newLen);
    else if (newLen > length())
      {
	long pos = getFilePointer();
	seek(newLen - 1);
	write(0);
	seek(pos);
      }
  }

  /**
   * This method returns the length of the file in bytes
   *
   * @return The length of the file
   *
   * @exception IOException If an error occurs
   */
  public long length () throws IOException
  {
    return ch.size();
  }

  /**
   * This method reads a single byte of data from the file and returns it
   * as an integer.
   *
   * @return The byte read as an int, or -1 if the end of the file was reached.
   *
   * @exception IOException If an error occurs
   */
  public int read () throws IOException
  {
    return in.read();
  }

  /**
   * This method reads bytes from the file into the specified array.  The
   * bytes are stored starting at the beginning of the array and up to 
   * <code>buf.length</code> bytes can be read.
   *
   * @param buffer The buffer to read bytes from the file into
   *
   * @return The actual number of bytes read or -1 if end of file
   *
   * @exception IOException If an error occurs
   */
  public int read (byte[] buffer) throws IOException
  {
    return in.read (buffer);
  }

  /**
   * This methods reads up to <code>len</code> bytes from the file into the
   * specified array starting at position <code>offset</code> into the array.
   *
   * @param buffer The array to read the bytes into
   * @param offset The index into the array to start storing bytes
   * @param len The requested number of bytes to read
   *
   * @return The actual number of bytes read, or -1 if end of file
   *
   * @exception IOException If an error occurs
   */
  public int read (byte[] buffer, int offset, int len) throws IOException
  {
    return in.read (buffer, offset, len);
  }

  /**
   * This method reads a Java boolean value from an input stream.  It does
   * so by reading a single byte of data.  If that byte is zero, then the
   * value returned is <code>false</code>  If the byte is non-zero, then
   * the value returned is <code>true</code>
   * <p>
   * This method can read a <code>boolean</code> written by an object 
   * implementing the
   * <code>writeBoolean()</code> method in the <code>DataOutput</code> 
   * interface.
   *
   * @return The <code>boolean</code> value read
   *
   * @exception EOFException If end of file is reached before reading the 
   * boolean
   * @exception IOException If any other error occurs
   */
  public final boolean readBoolean () throws IOException
  {
    return in.readBoolean ();
  }

  /**
   * This method reads a Java byte value from an input stream.  The value
   * is in the range of -128 to 127.
   * <p>
   * This method can read a <code>byte</code> written by an object 
   * implementing the 
   * <code>writeByte()</code> method in the <code>DataOutput</code> interface.
   *
   * @return The <code>byte</code> value read
   *
   * @exception EOFException If end of file is reached before reading the byte
   * @exception IOException If any other error occurs
   *
   * @see DataOutput
   */
  public final byte readByte () throws IOException
  {
    return in.readByte ();
  }

  /**
   * This method reads a Java <code>char</code> value from an input stream.  
   * It operates by reading two bytes from the stream and converting them to 
   * a single 16-bit Java <code>char</code>  The two bytes are stored most
   * significant byte first (i.e., "big endian") regardless of the native
   * host byte ordering. 
   * <p>
   * As an example, if <code>byte1</code> and <code>byte2</code> represent 
   * the first
   * and second byte read from the stream respectively, they will be
   * transformed to a <code>char</code> in the following manner:
   * <p>
   * <code>(char)(((byte1 &amp; 0xFF) &lt;&lt; 8) | (byte2 &amp; 0xFF)</code>
   * <p>
   * This method can read a <code>char</code> written by an object 
   * implementing the
   * <code>writeChar()</code> method in the <code>DataOutput</code> interface.
   *
   * @return The <code>char</code> value read 
   *
   * @exception EOFException If end of file is reached before reading the char
   * @exception IOException If any other error occurs
   *
   * @see DataOutput
   */
  public final char readChar () throws IOException
  {
    return in.readChar();
  }

  /**
   * This method reads a Java double value from an input stream.  It operates
   * by first reading a <code>logn</code> value from the stream by calling the
   * <code>readLong()</code> method in this interface, then 
   * converts that <code>long</code>
   * to a <code>double</code> using the <code>longBitsToDouble</code> 
   * method in the class <code>java.lang.Double</code>
   * <p>
   * This method can read a <code>double</code> written by an object 
   * implementing the
   * <code>writeDouble()</code> method in the <code>DataOutput</code> 
   * interface.
   *
   * @return The <code>double</code> value read
   *
   * @exception EOFException If end of file is reached before reading 
   * the double
   * @exception IOException If any other error occurs
   *
   * @see java.lang.Double
   * @see DataOutput
   */
  public final double readDouble () throws IOException
  {
    return in.readDouble ();
  }

  /**
   * This method reads a Java float value from an input stream.  It operates
   * by first reading an <code>int</code> value from the stream by calling the
   * <code>readInt()</code> method in this interface, then converts 
   * that <code>int</code>
   * to a <code>float</code> using the <code>intBitsToFloat</code> method in 
   * the class <code>java.lang.Float</code>
   * <p>
   * This method can read a <code>float</code> written by an object 
   * implementing the
   * <code>writeFloat()</code> method in the <code>DataOutput</code> interface.
   *
   * @return The <code>float</code> value read
   *
   * @exception EOFException If end of file is reached before reading the float
   * @exception IOException If any other error occurs
   *
   * @see java.lang.Float
   * @see DataOutput
   */
  public final float readFloat () throws IOException
  {
    return in.readFloat();
  }

  /**
   * This method reads raw bytes into the passed array until the array is
   * full.  Note that this method blocks until the data is available and
   * throws an exception if there is not enough data left in the stream to
   * fill the buffer
   *
   * @param buffer The buffer into which to read the data
   *
   * @exception EOFException If end of file is reached before filling the 
   * buffer
   * @exception IOException If any other error occurs
   */
  public final void readFully (byte[] buffer) throws IOException
  {
    in.readFully(buffer);
  }

  /**
   * This method reads raw bytes into the passed array <code>buf</code> 
   * starting
   * <code>offset</code> bytes into the buffer.  The number of bytes read 
   * will be
   * exactly <code>len</code>  Note that this method blocks until the data is 
   * available and throws an exception if there is not enough data left in 
   * the stream to read <code>len</code> bytes.
   *
   * @param buffer The buffer into which to read the data
   * @param offset The offset into the buffer to start storing data
   * @param count The number of bytes to read into the buffer
   *
   * @exception EOFException If end of file is reached before filling 
   * the buffer
   * @exception IOException If any other error occurs
   */
  public final void readFully (byte[] buffer, int offset, int count)
    throws IOException
  {
    in.readFully (buffer, offset, count);
  }

  /**
   * This method reads a Java <code>int</code> value from an input stream
   * It operates by reading four bytes from the stream and converting them to 
   * a single Java <code>int</code>  The bytes are stored most
   * significant byte first (i.e., "big endian") regardless of the native
   * host byte ordering. 
   * <p>
   * As an example, if <code>byte1</code> through <code>byte4</code> 
   * represent the first
   * four bytes read from the stream, they will be
   * transformed to an <code>int</code> in the following manner:
   * <p>
   * <code>(int)(((byte1 &amp; 0xFF) &lt;&lt; 24) + ((byte2 &amp; 0xFF) &lt;&lt; 16) + 
   * ((byte3 &amp; 0xFF) &lt;&lt; 8) + (byte4 &amp; 0xFF)))</code>
   * <p>
   * The value returned is in the range of 0 to 65535.
   * <p>
   * This method can read an <code>int</code> written by an object 
   * implementing the
   * <code>writeInt()</code> method in the <code>DataOutput</code> interface.
   *
   * @return The <code>int</code> value read
   *
   * @exception EOFException If end of file is reached before reading the int
   * @exception IOException If any other error occurs
   *
   * @see DataOutput
   */
  public final int readInt () throws IOException
  {
    return in.readInt();
  }

  /**
   * This method reads the next line of text data from an input stream.
   * It operates by reading bytes and converting those bytes to 
   * <code>char</code>
   * values by treating the byte read as the low eight bits of the 
   * <code>char</code>
   * and using <code>0</code> as the high eight bits.  Because of this, it does
   * not support the full 16-bit Unicode character set.
   * <p>
   * The reading of bytes ends when either the end of file or a line terminator
   * is encountered.  The bytes read are then returned as a <code>String</code>
   * A line terminator is a byte sequence consisting of either 
   * <code>\r</code> <code>\n</code> or <code>\r\n</code>  These 
   * termination charaters are
   * discarded and are not returned as part of the string.
   * <p>
   * This method can read data that was written by an object implementing the
   * <code>writeLine()</code> method in <code>DataOutput</code>
   *
   * @return The line read as a <code>String</code>
   *
   * @exception IOException If an error occurs
   *
   * @see DataOutput
   */
  public final String readLine () throws IOException
  {
    return in.readLine ();
  }

  /**
   * This method reads a Java long value from an input stream
   * It operates by reading eight bytes from the stream and converting them to 
   * a single Java <code>long</code>  The bytes are stored most
   * significant byte first (i.e., "big endian") regardless of the native
   * host byte ordering. 
   * <p>
   * As an example, if <code>byte1</code> through <code>byte8</code> 
   * represent the first
   * eight bytes read from the stream, they will be
   * transformed to an <code>long</code> in the following manner:
   * <p>
   * <code>
   * (long)((((long)byte1 &amp; 0xFF) &lt;&lt; 56) + (((long)byte2 &amp; 0xFF) &lt;&lt; 48) + 
   * (((long)byte3 &amp; 0xFF) &lt;&lt; 40) + (((long)byte4 &amp; 0xFF) &lt;&lt; 32) + 
   * (((long)byte5 &amp; 0xFF) &lt;&lt; 24) + (((long)byte6 &amp; 0xFF) &lt;&lt; 16) + 
   * (((long)byte7 &amp; 0xFF) &lt;&lt; 8) + ((long)byte9 &amp; 0xFF)))</code>
   * <p>
   * The value returned is in the range of 0 to 65535.
   * <p>
   * This method can read an <code>long</code> written by an object 
   * implementing the
   * <code>writeLong()</code> method in the <code>DataOutput</code> interface.
   *
   * @return The <code>long</code> value read
   *
   * @exception EOFException If end of file is reached before reading the long
   * @exception IOException If any other error occurs
   *
   * @see DataOutput
   */
  public final long readLong () throws IOException
  {
    return in.readLong();
  }

  /**
   * This method reads a signed 16-bit value into a Java in from the stream.
   * It operates by reading two bytes from the stream and converting them to 
   * a single 16-bit Java <code>short</code>  The two bytes are stored most
   * significant byte first (i.e., "big endian") regardless of the native
   * host byte ordering. 
   * <p>
   * As an example, if <code>byte1</code> and <code>byte2</code> 
   * represent the first
   * and second byte read from the stream respectively, they will be
   * transformed to a <code>short</code> in the following manner:
   * <p>
   * <code>(short)(((byte1 &amp; 0xFF) &lt;&lt; 8) | (byte2 &amp; 0xFF)</code>
   * <p>
   * The value returned is in the range of -32768 to 32767.
   * <p>
   * This method can read a <code>short</code> written by an object 
   * implementing the
   * <code>writeShort()</code> method in the <code>DataOutput</code> interface.
   *
   * @return The <code>short</code> value read
   *
   * @exception EOFException If end of file is reached before reading the value
   * @exception IOException If any other error occurs
   *
   * @see DataOutput
   */
  public final short readShort () throws IOException
  {
    return in.readShort();
  }

  /**
   * This method reads 8 unsigned bits into a Java <code>int</code> value 
   * from the 
   * stream. The value returned is in the range of 0 to 255.
   * <p>
   * This method can read an unsigned byte written by an object implementing 
   * the <code>writeUnsignedByte()</code> method in the 
   * <code>DataOutput</code> interface.
   *
   * @return The unsigned bytes value read as a Java <code>int</code>
   *
   * @exception EOFException If end of file is reached before reading the value
   * @exception IOException If any other error occurs
   *
   * @see DataOutput
   */
  public final int readUnsignedByte () throws IOException
  {
    return in.readUnsignedByte();
  }

  /**
   * This method reads 16 unsigned bits into a Java int value from the stream.
   * It operates by reading two bytes from the stream and converting them to 
   * a single Java <code>int</code>  The two bytes are stored most
   * significant byte first (i.e., "big endian") regardless of the native
   * host byte ordering. 
   * <p>
   * As an example, if <code>byte1</code> and <code>byte2</code> 
   * represent the first
   * and second byte read from the stream respectively, they will be
   * transformed to an <code>int</code> in the following manner:
   * <p>
   * <code>(int)(((byte1 &amp; 0xFF) &lt;&lt; 8) + (byte2 &amp; 0xFF))</code>
   * <p>
   * The value returned is in the range of 0 to 65535.
   * <p>
   * This method can read an unsigned short written by an object implementing
   * the <code>writeUnsignedShort()</code> method in the 
   * <code>DataOutput</code> interface.
   *
   * @return The unsigned short value read as a Java <code>int</code>
   *
   * @exception EOFException If end of file is reached before reading the value
   * @exception IOException If any other error occurs
   */
  public final int readUnsignedShort () throws IOException
  {
    return in.readUnsignedShort();
  }

  /**
   * This method reads a <code>String</code> from an input stream that 
   * is encoded in
   * a modified UTF-8 format.  This format has a leading two byte sequence
   * that contains the remaining number of bytes to read.  This two byte
   * sequence is read using the <code>readUnsignedShort()</code> method of this
   * interface.
   * <p>
   * After the number of remaining bytes have been determined, these bytes
   * are read an transformed into <code>char</code> values.  
   * These <code>char</code> values
   * are encoded in the stream using either a one, two, or three byte format.
   * The particular format in use can be determined by examining the first
   * byte read.  
   * <p>
   * If the first byte has a high order bit of 0 then
   * that character consists on only one byte.  This character value consists
   * of seven bits that are at positions 0 through 6 of the byte.  As an
   * example, if <code>byte1</code> is the byte read from the stream, it would
   * be converted to a <code>char</code> like so:
   * <p>
   * <code>(char)byte1</code>
   * <p>
   * If the first byte has <code>110</code> as its high order bits, then the 
   * character consists of two bytes.  The bits that make up the character
   * value are in positions 0 through 4 of the first byte and bit positions
   * 0 through 5 of the second byte.  (The second byte should have 
   * 10 as its high order bits).  These values are in most significant
   * byte first (i.e., "big endian") order.
   * <p>
   * As an example, if <code>byte1</code> and <code>byte2</code> 
   * are the first two bytes
   * read respectively, and the high order bits of them match the patterns
   * which indicate a two byte character encoding, then they would be
   * converted to a Java <code>char</code> like so:
   * <p>
   * <code>(char)(((byte1 & 0x1F) << 6) | (byte2 & 0x3F))</code>
   * <p>
   * If the first byte has a <code>1110</code> as its high order bits, then the
   * character consists of three bytes.  The bits that make up the character
   * value are in positions 0 through 3 of the first byte and bit positions
   * 0 through 5 of the other two bytes.  (The second and third bytes should
   * have <code>10</code> as their high order bits).  These values are in most
   * significant byte first (i.e., "big endian") order.
   * <p>
   * As an example, if <code>byte1</code> <code>byte2</code> 
   * and <code>byte3</code> are the
   * three bytes read, and the high order bits of them match the patterns
   * which indicate a three byte character encoding, then they would be
   * converted to a Java <code>char</code> like so:
   * <p>
   * <code>(char)(((byte1 & 0x0F) << 12) | ((byte2 & 0x3F) << 6) | 
   * (byte3 & 0x3F))</code>
   * <p>
   * Note that all characters are encoded in the method that requires the
   * fewest number of bytes with the exception of the character with the
   * value of <code>&#92;u0000</code> which is encoded as two bytes.  This is 
   * a  modification of the UTF standard used to prevent C language style
   * <code>NUL</code> values from appearing in the byte stream.
   * <p>
   * This method can read data that was written by an object implementing the
   * <code>writeUTF()</code> method in <code>DataOutput</code>
   * 
   * @return The <code>String</code> read
   *
   * @exception EOFException If end of file is reached before reading the 
   * String
   * @exception UTFDataFormatException If the data is not in UTF-8 format
   * @exception IOException If any other error occurs
   *
   * @see DataOutput
   */
  public final String readUTF () throws IOException
  {
    return in.readUTF();
  }

  /**
   * This method sets the current file position to the specified offset 
   * from the beginning of the file.  Note that some operating systems will
   * allow the file pointer to be set past the current end of the file.
   *
   * @param pos The offset from the beginning of the file at which to set 
   * the file pointer
   *
   * @exception IOException If an error occurs
   */
  public void seek (long pos) throws IOException
  {
    ch.position(pos);
  }

  /**
   * This method attempts to skip and discard the specified number of bytes 
   * in the input stream.  It may actually skip fewer bytes than requested. 
   * The actual number of bytes skipped is returned.  This method will not
   * skip any bytes if passed a negative number of bytes to skip.
   *
   * @param numBytes The requested number of bytes to skip.
   *
   * @return The number of bytes actually skipped.
   *
   * @exception IOException If an error occurs.
   */
  public int skipBytes (int numBytes) throws IOException
  {
    if (numBytes < 0)
      throw new IllegalArgumentException ("Can't skip negative bytes: " +
                                          numBytes);
    
    if (numBytes == 0)
      return 0;
    
    long oldPos = ch.position();
    long newPos = oldPos + numBytes;
    long size = ch.size();
    if (newPos > size)
      newPos = size;
    ch.position(newPos);
    return (int) (ch.position() - oldPos);
  }

  /**
   * This method writes a single byte of data to the file. The file must
   * be open for read-write in order for this operation to succeed.
   *
   * @param oneByte The byte of data to write, passed as an int.
   *
   * @exception IOException If an error occurs
   */
  public void write (int oneByte) throws IOException
  {
    out.write(oneByte);
  }

  /**
   * This method writes all the bytes in the specified array to the file.
   * The file must be open read-write in order for this operation to succeed.
   *
   * @param buffer The array of bytes to write to the file
   */
  public void write (byte[] buffer) throws IOException
  {
    out.write(buffer);
  }

  /**
   * This method writes <code>len</code> bytes to the file from the specified
   * array starting at index <code>offset</code> into the array.
   *
   * @param buffer The array of bytes to write to the file
   * @param offset The index into the array to start writing file
   * @param len The number of bytes to write
   *
   * @exception IOException If an error occurs
   */
  public void write (byte[] buffer, int offset, int len) throws IOException
  {
    out.write (buffer, offset, len);
  }

  /**
   * This method writes a Java <code>boolean</code> to the underlying output 
   * stream. For a value of <code>true</code>, 1 is written to the stream.
   * For a value of <code>false</code>, 0 is written.
   *
   * @param val The <code>boolean</code> value to write to the stream
   *
   * @exception IOException If an error occurs
   */
  public final void writeBoolean (boolean val) throws IOException
  {
    out.writeBoolean(val);
  }

  /**
   * This method writes a Java <code>byte</code> value to the underlying
   * output stream.
   *
   * @param val The <code>byte</code> to write to the stream, passed 
   * as an <code>int</code>.
   *
   * @exception IOException If an error occurs
   */
  public final void writeByte (int val) throws IOException
  {
    out.writeByte(val);
  }

  /**
   * This method writes a Java <code>short</code> to the stream, high byte
   * first.  This method requires two bytes to encode the value.
   *
   * @param val The <code>short</code> value to write to the stream, 
   * passed as an <code>int</code>.
   *
   * @exception IOException If an error occurs
   */
  public final void writeShort (int val) throws IOException
  {
    out.writeShort(val);
  }

  /**
   * This method writes a single <code>char</code> value to the stream,
   * high byte first.
   *
   * @param val The <code>char</code> value to write, passed as 
   * an <code>int</code>.
   *
   * @exception IOException If an error occurs
   */
  public final void writeChar (int val) throws IOException
  {
    out.writeChar(val);
  }

  /**
   * This method writes a Java <code>int</code> to the stream, high bytes
   * first.  This method requires four bytes to encode the value.
   *
   * @param val The <code>int</code> value to write to the stream.
   *
   * @exception IOException If an error occurs
   */
  public final void writeInt (int val) throws IOException
  {
    out.writeInt(val);
  }

  /**
   * This method writes a Java <code>long</code> to the stream, high bytes
   * first.  This method requires eight bytes to encode the value.
   *
   * @param val The <code>long</code> value to write to the stream.
   *
   * @exception IOException If an error occurs
   */
  public final void writeLong (long val) throws IOException
  {
    out.writeLong(val);
  }

  /**
   * This method writes a Java <code>float</code> value to the stream.  This
   * value is written by first calling the method 
   * <code>Float.floatToIntBits</code>
   * to retrieve an <code>int</code> representing the floating point number,
   * then writing this <code>int</code> value to the stream exactly the same
   * as the <code>writeInt()</code> method does.
   *
   * @param val The floating point number to write to the stream.
   *
   * @exception IOException If an error occurs
   *
   * @see #writeInt(int)
   */
  public final void writeFloat (float val) throws IOException
  {
    out.writeFloat(val);
  }

  /**
   * This method writes a Java <code>double</code> value to the stream.  This
   * value is written by first calling the method 
   * <code>Double.doubleToLongBits</code>
   * to retrieve an <code>long</code> representing the floating point number,
   * then writing this <code>long</code> value to the stream exactly the same
   * as the <code>writeLong()</code> method does.
   *
   * @param val The double precision floating point number to write to the 
   * stream.
   *
   * @exception IOException If an error occurs
   *
   * @see #writeLong(long)
   */
  public final void writeDouble (double val) throws IOException
  {
    out.writeDouble(val);
  }

  /**
   * This method writes all the bytes in a <code>String</code> out to the
   * stream.  One byte is written for each character in the <code>String</code>.
   * The high eight bits of each character are discarded.
   *
   * @param val The <code>String</code> to write to the stream
   *
   * @exception IOException If an error occurs
   */
  public final void writeBytes (String val) throws IOException
  {
    out.writeBytes(val);
  }
  
  /**
   * This method writes all the characters in a <code>String</code> to the
   * stream.  There will be two bytes for each character value.  The high
   * byte of the character will be written first.
   *
   * @param val The <code>String</code> to write to the stream.
   *
   * @exception IOException If an error occurs
   */
  public final void writeChars (String val) throws IOException
  {
    out.writeChars(val);
  }
  
  /**
   * This method writes a Java <code>String</code> to the stream in a modified
   * UTF-8 format.  First, two bytes are written to the stream indicating the
   * number of bytes to follow.  Note that this is the number of bytes in the
   * encoded <code>String</code> not the <code>String</code> length.  Next
   * come the encoded characters.  Each character in the <code>String</code>
   * is encoded as either one, two or three bytes.  For characters in the
   * range of <code>&#92;u0001</code> to <code>&#92;u007F</code>, 
   * one byte is used.  The character
   * value goes into bits 0-7 and bit eight is 0.  For characters in the range
   * of <code>&#92;u0080</code> to <code>&#92;u007FF</code>, two 
   * bytes are used.  Bits
   * 6-10 of the character value are encoded bits 0-4 of the first byte, with
   * the high bytes having a value of "110".  Bits 0-5 of the character value
   * are stored in bits 0-5 of the second byte, with the high bits set to
   * "10".  This type of encoding is also done for the null character
   * <code>&#92;u0000</code>.  This eliminates any C style NUL character values
   * in the output.  All remaining characters are stored as three bytes.
   * Bits 12-15 of the character value are stored in bits 0-3 of the first
   * byte.  The high bits of the first bytes are set to "1110".  Bits 6-11
   * of the character value are stored in bits 0-5 of the second byte.  The
   * high bits of the second byte are set to "10".  And bits 0-5 of the
   * character value are stored in bits 0-5 of byte three, with the high bits
   * of that byte set to "10".
   *
   * @param val The <code>String</code> to write to the output in UTF format
   *
   * @exception IOException If an error occurs
   */
  public final void writeUTF (String val) throws IOException
  {
    out.writeUTF(val);
  }
  
  /**
   * This method creates a java.nio.channels.FileChannel.
   * Nio does not allow one to create a file channel directly.
   * A file channel must be created by first creating an instance of
   * Input/Output/RandomAccessFile and invoking the getChannel() method on it.
   */
  public final synchronized FileChannel getChannel ()
  {
    return ch;
  }
}
