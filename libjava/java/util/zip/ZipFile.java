/* ZipFile.java - Read contents of a ZIP file
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

package java.util.zip;

import java.io.*;

/* Written using on-line Java Platform 1.2 API Specification
 * and JCL book.
 * Believed complete and correct.
 */

public class ZipFile implements ZipConstants
{
  public static final int OPEN_READ = 1;
  public static final int OPEN_DELETE = 4;

  public ZipFile (String fname) throws IOException
  {
    this(new File(fname));
  }

  public ZipFile (File f) throws IOException
  {
    this(f, OPEN_READ);
  }

  public ZipFile (File f, int mode) throws IOException
  {
    if (mode != OPEN_READ && mode != (OPEN_READ | OPEN_DELETE))
        throw new IllegalArgumentException
            ("mode can only be OPEN_READ or OPEN_READ | OPEN_DELETE");

    if ((mode & OPEN_DELETE) != 0)
      {
	delete_on_close = f;
	f.deleteOnExit();
      }
    else
      {
	delete_on_close = null;
      }

    file = new RandomAccessFile(f, "r");
    name = f.getName();
    readDirectory ();
  }

  void readDirectory () throws IOException
  {
    long size = file.length ();
    if (size < ZipConstants.END_CENTRAL_DIR_SIZE)
      throw new ZipException ("zipfile too short");
    // We do not handle a "zipfile comment", which the appnote says can
    // be at the end of a .zip file.  We could handle this by seeking
    // to the beginning and reading forwards.
    file.seek(size - ZipConstants.END_CENTRAL_DIR_SIZE);
    if (file.read() != 'P'
	|| file.read() != 'K'
	|| file.read() != '\005'
	|| file.read() != '\006')
      throw new ZipException("not a valid zipfile");
    file.skipBytes(6);
    numEntries = readu2();
    int dir_size = read4 ();  // Read "size of the central directory".
    file.seek(size - (dir_size + ZipConstants.END_CENTRAL_DIR_SIZE));

    ZipEntry last = null;
    for (int i = 0;  i < numEntries;  i++)
      {
	file.skipBytes(10);
	int method = readu2();
	int modtime = readu2();
	int moddate = readu2();
	int crc = read4();
	int compressedSize = read4();
	int uncompressedSize = read4();
	int filenameLength = readu2();
	int extraLength = readu2();
	int commentLength = readu2();
	int diskNumberStart = readu2();
	int intAttributes = readu2();
	int extAttributes = read4();
	int relativeOffset = read4();
	byte[] bname = new byte[filenameLength];
	file.readFully(bname);
	ZipEntry entry = new ZipEntry(new String(bname, "8859_1"));
	if (extraLength > 0)
	  {
	    byte[] bextra = new byte[extraLength];
	    file.readFully(bextra);
	    entry.extra = bextra;
	  }
	if (commentLength > 0)
	  {
	    byte[] bcomment = new byte[commentLength];
	    file.readFully(bcomment);
	    entry.comment = new String(bcomment, "8859_1");
	  }
	entry.compressedSize = compressedSize;
	entry.size = uncompressedSize;
	entry.crc = (long) crc & 0xffffffffL;
	entry.method = method;
	entry.relativeOffset = relativeOffset;
	entry.time = ZipEntry.timeFromDOS(moddate, modtime);
	if (last == null)
	  entries = entry;
	else
	  last.next = entry;
	last = entry;
      }
  }

  public java.util.Enumeration entries()
  {
    return new ZipEnumeration(this);
  }

  public void close() throws IOException
  {
    file.close();
    entries = null;
    numEntries = 0;
    if (delete_on_close != null)
	delete_on_close.delete();
  }

  public ZipEntry getEntry(String name)
  {
    for (ZipEntry entry = entries;  entry != null;  entry = entry.next)
      {
	if (name.equals(entry.getName()))
	  return entry;
      }
    return null;
  }

  public InputStream getInputStream(ZipEntry ze)  throws IOException
  {
    byte[] buffer = new byte[(int) ze.getCompressedSize()];

    /* Read the size of the extra field, and skip to the start of the
       data.  */
    file.seek (ze.relativeOffset + ZipConstants.LOCAL_FILE_HEADER_SIZE - 2);
    int extraFieldLength = readu2();
    file.skipBytes (ze.getName().length() + extraFieldLength);

    file.readFully(buffer);

    InputStream is = new ByteArrayInputStream (buffer);
    if (ze.getMethod() == ZipEntry.DEFLATED)
      // Data in zipfile entries does not have a zlib header, so construct
      // an Inflater with the `nowrapper' option.
      is = new InflaterInputStream (is, new Inflater (true), 512);
    return is;
  }

  public String getName ()
  {
    return name;
  }

  /**
   * Returns the number of entries in this ZipFile.
   * @exception IllegalStateException if the ZipFile has been closed.
   *
   * @since 1.2
   */
  public int size ()
  {
    if (entries == null)
      throw new IllegalStateException("ZipFile already closed");
    else
      return numEntries;
  }

  protected void finalize () throws IOException
  {
    close();
  }

  private int readu2 () throws IOException
  {
    int byte0 = file.read();
    int byte1 = file.read();
    if (byte0 < 0 || byte1 < 0)
      throw new ZipException (".zip archive ended prematurely");
    return ((byte1 & 0xFF) << 8) | (byte0 & 0xFF);
  }

  private int read4 () throws IOException
  {
    int byte0 = file.read();
    int byte1 = file.read();
    int byte2 = file.read();
    int byte3 = file.read();
    if (byte3 < 0)
      throw new ZipException (".zip archive ended prematurely");
    return ((byte3 & 0xFF) << 24) + ((byte2 & 0xFF) << 16)
      + ((byte1 & 0xFF) << 8) + (byte0 & 0xFF);
  }

  ZipEntry entries;
  int numEntries;
  RandomAccessFile file;
  String name;
  /** File to delete on close or null. */
  File delete_on_close;
    
}

final class ZipEnumeration implements java.util.Enumeration
{
  ZipEntry entry;

  ZipEnumeration (ZipFile zfile)
  {
    entry = zfile.entries;
  }

  public boolean hasMoreElements ()
  {
    return entry != null;
  }

  public Object nextElement ()
  {
    ZipEntry cur = entry;
    if (cur == null)
      throw new java.util.NoSuchElementException();
    entry = cur.next;
    return cur;
  }
}
