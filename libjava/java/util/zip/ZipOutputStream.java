/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.zip;
import java.io.*;

/** JUST AN INCOMPLETE STUB! */

public class ZipOutputStream extends DeflaterOutputStream
  implements ZipConstants
{
  ZipEntry current;
  int method = DEFLATED;
  int level = 3;  // FIXME - should be DEFAULT_COMPRESSION
  String comment;

  public static final int STORED = 0;
  public static final int DEFLATED = 8;

  public ZipOutputStream (OutputStream out)
  {
    super(out);
  }

  public void setLevel (int level) { this.level = level; }
  public void setMethod (int method) { this.method = method; }
  public void setComment(String comment) { this.comment = comment; }

  public void putNextEntry (ZipEntry entry) throws IOException
  {
    put4(0x04034b50);
    put2(0);  // version - FIXME
    put2(0);  // bits - FIXME
    if (entry.method < 0 )
      entry.method = method;
    put2(entry.method);
    put2(0);  // time - FIXME
    put2(0);  // date - FIXME
    put4((int) entry.crc);
    put4((int) entry.compressedSize); // FIXME
    put4((int) entry.size); // FIXME
    put2(entry.name.length());
    put2(entry.extra == null ? 0 : entry.extra.length);
    byte[] name = entry.name.getBytes("8859_1");
    out.write(name);
    if (entry.extra != null)
      out.write(entry.extra);
    throw new Error ("java.util.zip.ZipOutputStream.putNextEntry:  not implemented");
  }

  public void closeEntry ()  throws IOException
  {
  }

  private void put2 (int i)  throws IOException
  {
    out.write (i);
    out.write (i >> 8);
  }

  private void put4 (int i)  throws IOException
  {
    out.write (i);
    out.write (i >> 8);
    out.write (i >> 16);
    out.write (i >> 24);
  }
}
