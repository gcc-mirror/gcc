/* ZipOutputStream.java - Create a file in zip format
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
    int compressed_size;
    if (current.method == STORED)
      {
	compressed_size = uncompressed_size;
      }
    else
      {
	super.finish();
	compressed_size = def.getTotalOut();
      }
    long crc = sum.getValue();

    bytes_written += compressed_size;

    if (current.getCrc() == -1 || current.getCompressedSize() == -1
	|| current.getSize() == -1)
      {
	current.setCrc(crc);
	current.compressedSize = compressed_size;
	current.setSize(uncompressed_size);
	put4 (0x08074b50);
	put4 ((int) (current.getCrc()));
	put4 ((int) (current.getCompressedSize()));
	put4 ((int) (current.getSize()));
	bytes_written += 16;
      }
    else if (current.getCrc() != crc
	     || current.getCompressedSize() != compressed_size
	     || current.getSize() != uncompressed_size)
      throw new ZipException ("zip entry field incorrect");

    current.next = chain;
    chain = current;
    current = null;
  }

  public void write (int bval) throws IOException
  {
    if (current.method == STORED)
      {
	out.write(bval);
      }
    else
      super.write(bval);
    sum.update(bval);
    uncompressed_size += 1;
  }

  public void write (byte[] buf, int off, int len) throws IOException
  {
    if (current.method == STORED)
      out.write(buf, off, len);
    else
      super.write(buf, off, len);
    sum.update(buf, off, len);
    uncompressed_size += len;
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
    put2 (count);
    put4 (bytes);
    put4 ((int) offset);

    byte[] c = comment.getBytes("8859_1");
    put2 (c.length);
    out.write(c);
  }

  // Helper for finish and putNextEntry.
  private int write_entry (ZipEntry entry, boolean is_local)
    throws IOException
  {
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
	bytes += put4 (0);
	// Relative offset of local header.
	bytes += put4 ((int) entry.relativeOffset);
      }

    out.write (name);
    bytes += name.length;
    if (entry.extra != null)
      {
	out.write(entry.extra);
	bytes += entry.extra.length;
      }
    if (comment != null)
      {
	out.write(comment);
	bytes += comment.length;
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
    entry.relativeOffset = bytes_written;
    write_entry (entry, true);
    current = entry;
    int compr = (method == STORED) ? Deflater.NO_COMPRESSION : level;
    def.reset();
    def.setLevel(compr);
    sum.reset();
    uncompressed_size = 0;
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

  public ZipOutputStream (OutputStream out)
  {
    super (out, new Deflater (Deflater.DEFAULT_COMPRESSION, true), 8192);
    sum = new CRC32 ();
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

  private int method = DEFLATED;
  private int level = Deflater.DEFAULT_COMPRESSION;
  private String comment = "";
  private long bytes_written;

  private int uncompressed_size;

  /** The checksum object. */
  private Checksum sum;
}
