/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.zip;
import java.io.*;

/** JUST AN INCOMPLETE STUB! */

public class ZipFile implements ZipConstants
{

  String name;
  ZipEntry entries;

  public ZipFile (String fname) throws IOException
  {
    name = fname;
    // FIXME
  }

  public ZipFile (File f) throws IOException
  {
    this(f.getPath());
  }

  public java.util.Enumeration entries()
  {
    return new ZipEnumeration(this);
  }

  public void close() throws IOException
  {
    // FIXME
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
    return null; // FIXME
  }

  public String getName () { return name; }
}

class ZipEnumeration implements java.util.Enumeration
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
