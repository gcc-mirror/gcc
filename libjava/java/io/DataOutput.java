/* DataOutput.java -- Interface for writing data from a stream
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.io;

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * Status:  Complete to version 1.1.
 */

/**
  * This interface is implemented by classes that can wrte data to streams 
  * from Java primitive types.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey <tromey@cygnus.com>
  */
public interface DataOutput
{

/**
  * This method writes a Java boolean value to an output stream
  *
  * @param value The boolean value to write
  *
  * @exception IOException If an error occurs
  */
void
writeBoolean(boolean value) throws IOException;

/*************************************************************************/

/**
  * This method writes a Java byte value to an output stream
  *
  * @param value The int value to write
  *
  * @exception IOException If an error occurs
  */
void
writeByte(int value) throws IOException;

/*************************************************************************/

/**
  * This method writes a Java char value to an output stream
  *
  * @param value The char value to write
  *
  * @exception IOException If an error occurs
  */
void
writeChar(int value) throws IOException;

/*************************************************************************/

/**
  * This method writes a Java int value to an output stream as a 16 bit value
  *
  * @param value The int value to write as a 16-bit value
  *
  * @exception IOException If an error occurs
  */
void
writeShort(int value) throws IOException;

/*************************************************************************/

/**
  * This method writes a Java int value to an output stream
  *
  * @param value The int value to write
  *
  * @exception IOException If an error occurs
  */
void
writeInt(int value) throws IOException;

/*************************************************************************/

/**
  * This method writes a Java long value to an output stream
  *
  * @param value The long value to write
  *
  * @exception IOException If an error occurs
  */
void
writeLong(long value) throws IOException;

/*************************************************************************/

/**
  * This method writes a Java float value to an output stream
  *
  * @param value The float value to write
  *
  * @exception IOException If an error occurs
  */
void
writeFloat(float value) throws IOException;

/*************************************************************************/

/**
  * This method writes a Java double value to an output stream
  *
  * @param value The double value to write
  *
  * @exception IOException If any other error occurs
  */
void
writeDouble(double value) throws IOException;

/*************************************************************************/

/**
  * This method writes a String to an output stream as an array of bytes
  *
  * @param value The String to write
  *
  * @exception IOException If an error occurs
  */
void
writeBytes(String value) throws IOException;

/*************************************************************************/

/**
  * This method writes a String to an output stream as an array of char's
  *
  * @param value The String to write
  *
  * @exception IOException If an error occurs
  */
void
writeChars(String value) throws IOException;

/*************************************************************************/

/**
  * This method writes a String to an output stream encoded in
  * UTF-8 format.
  *
  * @param value The String to write
  *
  * @exception IOException If an error occurs
  */
void
writeUTF(String value) throws IOException;

/*************************************************************************/

/**
  * This method writes an 8-bit value (passed into the method as a Java
  * int) to an output stream.
  *
  * @param value The byte to write to the output stream
  *
  * @exception IOException If an error occurs
  */
void
write(int value) throws IOException;

/*************************************************************************/

/**
  * This method writes the raw byte array passed in to the output stream.
  *
  * @param buf The byte array to write
  *
  * @exception IOException If an error occurs
  */
void
write(byte[] buf) throws IOException;

/*************************************************************************/

/**
  * This method writes raw bytes from the passed array <code>buf</code> starting
  * <code>offset</code> bytes into the buffer.  The number of bytes written will be
  * exactly <code>len</code>. 
  *
  * @param buf The buffer from which to write the data
  * @param offset The offset into the buffer to start writing data from
  * @param len The number of bytes to write from the buffer to the output stream
  *
  * @exception IOException If any other error occurs
  */
void
write(byte[] buf, int offset, int len) throws IOException;

} // interface DataOutput
