// ZipFile.java - Read contents of a ZIP file.

/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.zip;
import java.io.*;

/* Written using on-line Java Platform 1.2 API Specification
 * and JCL book.
 * Believed complete and correct.
 */

public class ZipFile implements ZipConstants
{
  public ZipFile (String fname) throws IOException
  {
    file = new RandomAccessFile(fname, "r");
    name = fname;
    readDirectory ();
  }

  public ZipFile (File f) throws IOException
  {
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
    byte[] buffer = new byte[(int) ze.getSize()];

    /* Read the size of the extra field, and skip to the start of the
       data.  */
    file.seek (ze.relativeOffset + ZipConstants.LOCAL_FILE_HEADER_SIZE - 2);
    int extraFieldLength = readu2();
    file.skipBytes (ze.getName().length() + extraFieldLength);

    file.readFully(buffer);

    InputStream is = new ByteArrayInputStream (buffer);
    if (ze.getMethod() == ZipEntry.DEFLATED)
      is = new InflaterInputStream (is);
    return is;
  }

  public String getName () { return name; }

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
