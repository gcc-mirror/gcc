/* Copyright (C) 1999, 2000  Red Hat, Inc.

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

public class ZipOutputStream extends DeflaterOutputStream
  implements ZipConstants
{
  public static final int STORED = 0;
  public static final int DEFLATED = 8;

  public void close () throws IOException
  {
    finish ();
    out.close();
  }

  public void closeEntry ()  throws IOException
  {
    int uncompressed_size = def.getTotalIn();
    int compressed_size = def.getTotalOut();
    int crc = (int) (filter.getChecksum().getValue());

    bytes_written += compressed_size;

    bytes_written += put4 (0x08074b50);
    if (current.getCrc() == -1 || current.getCompressedSize() == -1
	|| current.getSize() == -1)
      {
	current.setCrc(crc);
	current.compressedSize = compressed_size;
	current.setSize(uncompressed_size);
      }
    else
      {
	if (current.getCrc() != crc
	    || current.getCompressedSize() != compressed_size
	    || current.getSize() != uncompressed_size)
	  throw new ZipException ("zip entry field incorrect");
      }
    bytes_written += put4 ((int) (current.getCrc()));
    bytes_written += put4 ((int) (current.getCompressedSize()));
    bytes_written += put4 ((int) (current.getSize()));

    current.next = chain;
    chain = current;
    current = null;
    filter = null;
  }

  public void finish () throws IOException
  {
    if (current != null)
      closeEntry ();

    // Write the central directory.
    long offset = bytes_written;
    int count = 0;
    int bytes = 0;
    while (chain != null)
      {
	bytes += write_entry (chain, false);
	++count;
	chain = chain.next;
      }

    // Write the end of the central directory record.
    put4 (0x06054b50);
    // Disk number.
    put2 (0);
    // Another disk number.
    put2 (0);
    put2 (count);
    put4 (bytes);
    put4 ((int) offset);

    byte[] c = comment.getBytes("8859_1");
    put2 (c.length);
    out.write(c);
    out.write((byte) 0);
  }

  // Helper for finish and putNextEntry.
  private int write_entry (ZipEntry entry, boolean is_local)
    throws IOException
  {
    long offset = bytes_written;

    int bytes = put4 (is_local ? 0x04034b50 : 0x02014b50);
    if (! is_local)
      bytes += put_version ();
    bytes += put_version ();

    boolean crc_after = false;
    if (is_local
	&& (entry.getCrc() == -1 || entry.getCompressedSize() == -1
	    || entry.getSize() == -1))
      crc_after = true;
    // For the bits field we always indicate `normal' compression,
    // even if that isn't true.
    bytes += put2 (crc_after ? (1 << 3) : 0);
    bytes += put2 (entry.method);

    bytes += put2(0);  // time - FIXME
    bytes += put2(0);  // date - FIXME

    if (crc_after)
      {
	// CRC, compressedSize, and Size are always 0 in this header.
	// The actual values are given after the entry.
	bytes += put4 (0);
	bytes += put4 (0);
	bytes += put4 (0);
      }
    else
      {
	bytes += put4 ((int) (entry.getCrc()));
	bytes += put4 ((int) (entry.getCompressedSize()));
	bytes += put4 ((int) (entry.getSize()));
      }

    byte[] name = entry.name.getBytes("8859_1");
    bytes += put2 (name.length);
    bytes += put2 (entry.extra == null ? 0 : entry.extra.length);

    byte[] comment = null;
    if (! is_local)
      {
	if (entry.getComment() == null)
	  bytes += put2 (0);
	else
	  {
	    comment = entry.getComment().getBytes("8859_1");
	    bytes += put2 (comment.length);
	  }

	// Disk number start.
	bytes += put2 (0);
	// Internal file attributes.
	bytes += put2 (0);
	// External file attributes.
	bytes += put2 (0);
	// Relative offset of local header.
	bytes += put2 ((int) offset);
      }

    out.write (name);
    out.write ((byte) 0);
    bytes += name.length + 1;
    if (entry.extra != null)
      {
	out.write(entry.extra);
	out.write((byte) 0);
	bytes += entry.extra.length + 1;
      }
    if (comment != null)
      {
	out.write(comment);
	out.write((byte) 0);
	bytes += comment.length + 1;
      }

    bytes_written += bytes;
    return bytes;
  }

  public void putNextEntry (ZipEntry entry) throws IOException
  {
    if (current != null)
      closeEntry ();

    if (entry.method < 0 )
      entry.method = method;
    if (entry.method == STORED)
      {
	if (entry.getSize() == -1 || entry.getCrc() == -1)
	  throw new ZipException ("required entry not set");
	// Just in case.
	entry.compressedSize = entry.getSize();
      }
    write_entry (entry, true);
    current = entry;
    int compr = (method == STORED) ? Deflater.NO_COMPRESSION : level;
    def.reset();
    def.setLevel(compr);
    filter = new CheckedOutputStream (new DeflaterOutputStream (out, def),
				      new CRC32 ());
  }

  public void setLevel (int level)
  {
    if (level != Deflater.DEFAULT_COMPRESSION
	&& (level < Deflater.NO_COMPRESSION
	    || level > Deflater.BEST_COMPRESSION))
      throw new IllegalArgumentException ();
    this.level = level;
  }

  public void setMethod (int method)
  {
    if (method != DEFLATED && method != STORED)
      throw new IllegalArgumentException ();
    this.method = method;
  }

  public void setComment (String comment)
  {
    if (comment.length() > 65535)
      throw new IllegalArgumentException ();
    this.comment = comment;
  }

  public synchronized void write (byte[] buf, int off, int len)
    throws IOException
  {
    if (filter == null)
      throw new ZipException ("no open zip entry");
    filter.write(buf, off, len);
  }

  public ZipOutputStream (OutputStream out)
  {
    super (out);
    def = new Deflater (level, true);
  }

  private int put2 (int i) throws IOException
  {
    out.write (i);
    out.write (i >> 8);
    return 2;
  }

  private int put4 (int i) throws IOException
  {
    out.write (i);
    out.write (i >> 8);
    out.write (i >> 16);
    out.write (i >> 24);
    return 4;
  }

  private int put_version () throws IOException
  {
    // FIXME: for now we assume Unix, and we ignore the version
    // number.
    return put2 (3 << 8);
  }

  // The entry we are currently writing, or null if we've called
  // closeEntry.
  private ZipEntry current;
  // The chain of entries which have been written to this file.
  private ZipEntry chain;
  // The output stream to which data should be sent.
  private CheckedOutputStream filter;

  private int method = DEFLATED;
  private int level = Deflater.DEFAULT_COMPRESSION;
  private String comment = "";
  private long bytes_written;

  // The Deflater we use.
  private Deflater def;
}
