/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.zip;
import java.io.*;

/**
 * @author Per Bothner
 * @date May 1999.
 */

/*
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Quite incomplete, but can read uncompressed .zip archives.
 */

// JDK1.2 has "protected ZipEntry createZipEntry(String)" but is very
// vague about what the method does.  FIXME.
// We do not calculate the CRC and compare it with the specified value;
// we probably should.  FIXME.
   

public class ZipInputStream extends InflaterInputStream implements ZipConstants
{
  public ZipInputStream (InputStream in)
  {
    super (in, new Inflater (true));
  }

  public ZipEntry getNextEntry () throws IOException
  {
    if (current != null)
      closeEntry();
    if (in.read() != 'P'
	|| in.read() != 'K')
      return null;
    int code = in.read();
    while (code == '\001')
      {
	code = in.read();
	if (code != '\002')
	  return null;
	in.skip(16);
	int size = read4();
	in.skip(4);
	int fname_length = readu2();
	int extra_length = readu2();
	int fcomment_length = readu2();
	in.skip(12+fname_length+extra_length+fcomment_length+size);
	if (in.read() != 'P' || in.read() != 'K')
	  return null;
	code = in.read();
      }
    if (code == '\005')
      {
	if (in.read() != '\006')
	  return null;
	in.skip(16);
	int comment_size = readu2();
	in.skip(comment_size);
	if (in.read() != 'P' || in.read() != 'K')
	  return null;
	code = in.read();
      }
    if (code != '\003'
	|| in.read() != '\004')
      return null;
    int ex_version = readu2();
    current_flags = readu2();
    int method = readu2();
    int modtime = readu2();
    int moddate = readu2();
    int crc = read4();
    int compressedSize = read4();
    int uncompressedSize = read4();
    int filenameLength = readu2();
    int extraLength = readu2();
    byte[] bname = new byte[filenameLength];
    readFully(bname);
    ZipEntry entry = new ZipEntry(new String(bname, "8859_1"));
    if (extraLength > 0)
      {
	byte[] bextra = new byte[extraLength];
	readFully(bextra);
	entry.extra = bextra;
      }
    entry.compressedSize = compressedSize;
    entry.size = uncompressedSize;
    entry.crc = (long) crc & 0xffffffffL;
    entry.method = method;
    entry.time = ZipEntry.timeFromDOS(moddate, modtime);
    current = entry;
    avail = uncompressedSize;
    compressed_bytes = compressedSize;
    return entry;
  }

  // We override fill to let us control how much data gets read from
  // the underlying input stream.  This lets us avoid having to push
  // back data.
  protected void fill () throws IOException
  {
    int count = buf.length;
    if (count > compressed_bytes)
      count = compressed_bytes;
    len = in.read(buf, 0, count);
    if (len != -1)
      {
	compressed_bytes -= len;
	inf.setInput(buf, 0, len);
      }
  }

  public int read (byte[] b, int off, int len)  throws IOException
  {
    if (len > avail)
      len = avail;
    int count;
    if (current.method == Deflater.DEFLATED)
      count = super.read(b, off, len);
    else
      count = in.read(b, off, len);
    if (count == -1 || avail == 0)
      {
	inf.reset();
	count = -1;
      }
    else
      avail -= count;
    return count;
  }

  public long skip (long n)  throws IOException
  {
    if (n > avail)
      n = avail;
    long count;
    if (current.method == Deflater.DEFLATED)
      count = super.skip(n);
    else
      count = in.skip(n);
    avail = avail - (int) count;
    return count;
  }

  private void readFully (byte[] b)  throws IOException
  {
    int off = 0;
    int len = b.length;
    while (len > 0)
      {
	int count = in.read(b, off, len);
	if (count <= 0)
	  throw new EOFException(".zip archive ended prematurely");
	off += count;
	len -= count;
      }
  }

  private int readu2 ()  throws IOException
  {
    int byte0 = in.read();
    int byte1 = in.read();
    if (byte0 < 0 || byte1 < 0)
      throw new EOFException(".zip archive ended prematurely");
    return ((byte1 & 0xFF) << 8) | (byte0 & 0xFF);
  }

  private int read4 () throws IOException
  {
    int byte0 = in.read();
    int byte1 = in.read();
    int byte2 = in.read();
    int byte3 = in.read();
    if (byte3 < 0)
      throw new EOFException(".zip archive ended prematurely");
    return ((byte3 & 0xFF) << 24) + ((byte2 & 0xFF) << 16)
      + ((byte1 & 0xFF) << 8) + (byte0 & 0xFF);
  }

  public void closeEntry ()  throws IOException
  {
    if (current != null)
      {
	if (avail > 0)
	  skip (avail);
	if ((current_flags & 8) != 0)
	  {
	    int sig = read4();
	    if (sig != 0x04034b50)
	      throw new ZipException("bad/missing magic number at end of .zip entry");
	    int crc = read4();
	    int compressedSize = read4();
	    int uncompressedSize = read4();
	    if (current.compressedSize != compressedSize
		|| current.size != uncompressedSize
		|| current.crc != crc)
	      throw new ZipException("bad data descriptor at end of .zip entry");
	  }
	current = null;
	avail = 0;
      }
  }

  public void close ()  throws IOException
  {
    current = null;
    super.close();
  }

  private ZipEntry current;
  private int current_flags;
  // Number of uncompressed bytes to be read.
  private int avail;
  // Number of bytes we can read from underlying stream.
  private int compressed_bytes;
}
