// GZIPInputStream.java - Input tiler for reading gzip file.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.zip;

import java.io.InputStream;
import java.io.IOException;

/**
 * @author Tom Tromey
 * @date May 17, 1999
 */

/* Written using on-line Java Platform 1.2 API Specification
 * and JCL book.
 * Believed complete and correct.
 */

public class GZIPInputStream extends InflaterInputStream
{
  public static final int GZIP_MAGIC = 0x8b1f;

  public void close () throws IOException
  {
    // Nothing to do here.
    super.close();
  }

  public GZIPInputStream (InputStream istream) throws IOException
  {
    this (istream, 512);
  }

  private final int eof_read () throws IOException
  {
    int r = in.read();
    if (r == -1)
      throw new ZipException ("gzip header corrupted");
    return r & 0xff;
  }

  public GZIPInputStream (InputStream istream, int readsize)
    throws IOException
  {
    super (istream, new Inflater (true), readsize);

    // NOTE: header reading code taken from zlib's gzio.c.

    // Read the magic number.
    int magic = eof_read () | (eof_read () << 8);
    if (magic != GZIP_MAGIC)
      throw new ZipException ("gzip header corrupted");

    int method = eof_read ();
    int flags = eof_read ();
    // Test from zlib.
    if (method != Z_DEFLATED || (flags & RESERVED) != 0)
      throw new ZipException ("gzip header corrupted");

    // Discard time, xflags, OS code.
    for (int i = 0; i < 6; ++i)
      eof_read ();

    // Skip the extra field.
    if ((flags & EXTRA_FIELD) != 0)
      {
	int len = eof_read () | (eof_read () << 8);
	while (len-- != 0)
	  eof_read ();
      }

    if ((flags & ORIG_NAME) != 0)
      {
	while (true)
	  {
	    int c = eof_read ();
	    if (c == 0)
	      break;
	  }
      }

    if ((flags & COMMENT) != 0)
      {
	while (true)
	  {
	    int c = eof_read ();
	    if (c == 0)
	      break;
	  }
      }

    if ((flags & HEAD_CRC) != 0)
      {
	// FIXME: consider checking CRC of the header.
	eof_read ();
	eof_read ();
      }

    crc = new CRC32 ();
  }

  public int read (byte[] buf, int off, int len) throws IOException
  {
    if (eos)
      return -1;
    int r = super.read(buf, off, len);
    if (r == -1)
      {
	eos = true;
	int header_crc = read4 ();
	if (crc.getValue() != header_crc)
	  throw new ZipException ("corrupted gzip file");
	// Read final `ISIZE' field.
	// FIXME: should we check this length?
	read4 ();
	return -1;
      }
    crc.update(buf, off, r);
    return r;
  }

  private final int read4 () throws IOException
  {
    int byte0 = in.read();
    int byte1 = in.read();
    int byte2 = in.read();
    int byte3 = in.read();
    if (byte3 < 0)
      throw new ZipException (".zip archive ended prematurely");
    return ((byte3 & 0xFF) << 24) + ((byte2 & 0xFF) << 16)
      + ((byte1 & 0xFF) << 8) + (byte0 & 0xFF);
  }

  // Checksum used by this input stream.
  protected CRC32 crc;

  // Indicates whether end-of-stream has been reached.
  protected boolean eos;

  // Some constants from zlib.
  static final int Z_DEFLATED = 8;
  static final int HEAD_CRC    = 0x02;
  static final int EXTRA_FIELD = 0x04;
  static final int ORIG_NAME   = 0x08;
  static final int COMMENT     = 0x10;
  static final int RESERVED    = 0xe0;
}
