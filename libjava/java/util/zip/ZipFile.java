/* java.util.zip.ZipFile
   Copyright (C) 2001 Free Software Foundation, Inc.

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
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.EOFException;
import java.io.RandomAccessFile;
import java.util.Enumeration;
import java.util.NoSuchElementException;

/**
 * This class represents a Zip archive.  You can ask for the contained
 * entries, or get an input stream for a file entry.  The entry is
 * automatically decompressed.
 *
 * This class is thread safe:  You can open input streams for arbitrary
 * entries in different threads.
 *
 * @author Jochen Hoenicke
 */
public class ZipFile implements ZipConstants
{

  /** Mode flag to open a zip file for reading 
   *
   */

  public static final int OPEN_READ = 0x1;

  /** Mode flag to delete a zip file after reading 
   *
   */

  public static final int OPEN_DELETE = 0x4;

  private String name;
  RandomAccessFile raf;
  ZipEntry[] entries;

  /**
   * Opens a Zip file with the given name for reading.
   * @exception IOException if a i/o error occured.
   * @exception ZipException if the file doesn't contain a valid zip
   * archive.  
   */
  public ZipFile(String name) throws ZipException, IOException
  {
    this.raf = new RandomAccessFile(name, "r");
    this.name = name;
    readEntries();
  }

  /**
   * Opens a Zip file reading the given File.
   * @exception IOException if a i/o error occured.
   * @exception ZipException if the file doesn't contain a valid zip
   * archive.  
   */
  public ZipFile(File file) throws ZipException, IOException
  {
    this.raf = new RandomAccessFile(file, "r");
    this.name = file.getName();
    readEntries();
  }

  /**
   * Opens a Zip file reading the given File in the given mode.
   *
   * If the OPEN_DELETE mode is specified, the zip file will be deleted at some time moment
   * after it is opened. It will be deleted before the zip file is closed or the Virtual Machine
   * exits.
   * 
   * The contents of the zip file will be accessible until it is closed.
   *
   * The OPEN_DELETE mode is currently unimplemented in this library
   * 
   * @since JDK1.3
   * @param mode Must be one of OPEN_READ or OPEN_READ | OPEN_DELETE
   *
   * @exception IOException if a i/o error occured.
   * @exception ZipException if the file doesn't contain a valid zip
   * archive.  
   */
  public ZipFile(File file, int mode) throws ZipException, IOException
  {
    if ((mode & OPEN_DELETE) != 0)
      {
	throw new IllegalArgumentException("OPEN_DELETE mode not supported yet in java.util.zip.ZipFile");
      }
    this.raf = new RandomAccessFile(file, "r");
    this.name = file.getName();
    readEntries();
  }

  /**
   * Read an unsigned short in little endian byte order.
   * @exception IOException if a i/o error occured.
   * @exception EOFException if the file ends prematurely
   */
  private final int readLeShort() throws IOException {
    return raf.readUnsignedByte() | raf.readUnsignedByte() << 8;
  }

  /**
   * Read an int in little endian byte order.
   * @exception IOException if a i/o error occured.
   * @exception EOFException if the file ends prematurely
   */
  private final int readLeInt() throws IOException {
    return readLeShort() | readLeShort() << 16;
  }

  /**
   * Read the central directory of a zip file and fill the entries
   * array.  This is called exactly once by the constructors.
   * @exception IOException if a i/o error occured.
   * @exception ZipException if the central directory is malformed 
   */
  private void readEntries() throws ZipException, IOException
  {
    /* Search for the End Of Central Directory.  When a zip comment is 
     * present the directory may start earlier.
     * FIXME: This searches the whole file in a very slow manner if the
     * file isn't a zip file.
     */
    long pos = raf.length() - ENDHDR;
    do
      {
	if (pos < 0)
	  throw new ZipException
	    ("central directory not found, probably not a zip file");
	raf.seek(pos--);
      }
    while (readLeInt() != ENDSIG);
    if (raf.skipBytes(ENDTOT - ENDNRD) != ENDTOT - ENDNRD)
      throw new EOFException();
    int count = readLeShort();
    if (raf.skipBytes(ENDOFF - ENDSIZ) != ENDOFF - ENDSIZ)
      throw new EOFException();
    int centralOffset = readLeInt();

    entries = new ZipEntry[count];
    raf.seek(centralOffset);
    for (int i = 0; i < count; i++)
      {
	if (readLeInt() != CENSIG)
	  throw new ZipException("Wrong Central Directory signature");
	if (raf.skipBytes(CENHOW - CENVEM) != CENHOW - CENVEM)
	  throw new EOFException();
	int method = readLeShort();
	int dostime = readLeInt();
	int crc = readLeInt();
	int csize = readLeInt();
	int size = readLeInt();
	int nameLen = readLeShort();
	int extraLen = readLeShort();
	int commentLen = readLeShort();
	if (raf.skipBytes(CENOFF - CENDSK) != CENOFF - CENDSK)
	  throw new EOFException();
	int offset = readLeInt();

	byte[] buffer = new byte[Math.max(nameLen, commentLen)];

	raf.readFully(buffer, 0, nameLen);
	String name = new String(buffer, 0, nameLen);

	ZipEntry entry = new ZipEntry(name);
	entry.setMethod(method);
	entry.setCrc(crc & 0xffffffffL);
	entry.setSize(size & 0xffffffffL);
	entry.setCompressedSize(csize & 0xffffffffL);
	entry.setDOSTime(dostime);
	if (extraLen > 0)
	  {
	    byte[] extra = new byte[extraLen];
	    raf.readFully(extra);
	    entry.setExtra(extra);
	  }
	if (commentLen > 0)
	  {
	    raf.readFully(buffer, 0, commentLen);
	    entry.setComment(new String(buffer, 0, commentLen));
	  }
	entry.zipFileIndex = i;
	entry.offset = offset;
	entries[i] = entry;
      }
  }

  /**
   * Closes the ZipFile.  This also closes all input streams given by
   * this class.  After this is called, no further method should be
   * called.
   * @exception IOException if a i/o error occured.
   */
  public void close() throws IOException
  {
    entries = null;
    synchronized (raf)
      {
	raf.close();
      }
  }

  /**
   * Returns an enumeration of all Zip entries in this Zip file.
   */
  public Enumeration entries()
  {
    if (entries == null)
      throw new IllegalStateException("ZipFile has closed");
    return new ZipEntryEnumeration(entries);
  }

  private int getEntryIndex(String name)
  {
    for (int i = 0; i < entries.length; i++)
      if (name.equals(entries[i].getName()))
	return i;
    return -1;
  }

  /**
   * Searches for a zip entry in this archive with the given name.
   * @param the name. May contain directory components separated by
   * slashes ('/').
   * @return the zip entry, or null if no entry with that name exists.
   * @see #entries */
  public ZipEntry getEntry(String name)
  {
    if (entries == null)
      throw new IllegalStateException("ZipFile has closed");
    int index = getEntryIndex(name);
    return index >= 0 ? (ZipEntry) entries[index].clone() : null;
  }

  /**
   * Checks, if the local header of the entry at index i matches the
   * central directory, and returns the offset to the data.
   * @return the start offset of the (compressed) data.
   * @exception IOException if a i/o error occured.
   * @exception ZipException if the local header doesn't match the 
   * central directory header
   */
  private long checkLocalHeader(ZipEntry entry) throws IOException
  {
    synchronized (raf)
      {
	raf.seek(entry.offset);
	if (readLeInt() != LOCSIG)
	  throw new ZipException("Wrong Local header signature");

	/* skip version and flags */
	if (raf.skipBytes(LOCHOW - LOCVER) != LOCHOW - LOCVER)
	  throw new EOFException();

	if (entry.getMethod() != readLeShort())
	  throw new ZipException("Compression method mismatch");

	/* Skip time, crc, size and csize */
	if (raf.skipBytes(LOCNAM - LOCTIM) != LOCNAM - LOCTIM)
	  throw new EOFException();

	if (entry.getName().length() != readLeShort())
	  throw new ZipException("file name length mismatch");

	int extraLen = entry.getName().length() + readLeShort();
	return entry.offset + LOCHDR + extraLen;
      }
  }

  /**
   * Creates an input stream reading the given zip entry as
   * uncompressed data.  Normally zip entry should be an entry
   * returned by getEntry() or entries().
   * @return the input stream.
   * @exception IOException if a i/o error occured.
   * @exception ZipException if the Zip archive is malformed.  
   */
  public InputStream getInputStream(ZipEntry entry) throws IOException
  {
    if (entries == null)
      throw new IllegalStateException("ZipFile has closed");
    int index = entry.zipFileIndex;
    if (index < 0 || index >= entries.length
	|| entries[index].getName() != entry.getName())
      {
	index = getEntryIndex(entry.getName());
	if (index < 0)
	  throw new NoSuchElementException();
      }

    long start = checkLocalHeader(entries[index]);
    int method = entries[index].getMethod();
    InputStream is = new PartialInputStream
      (raf, start, entries[index].getCompressedSize());
    switch (method)
      {
      case ZipOutputStream.STORED:
	return is;
      case ZipOutputStream.DEFLATED:
	return new InflaterInputStream(is, new Inflater(true));
      default:
	throw new ZipException("Unknown compression method " + method);
      }
  }
  
  /**
   * Returns the name of this zip file.
   */
  public String getName()
  {
    return name;
  }

  /**
   * Returns the number of entries in this zip file.
   */
  public int size()
  {
    try
      {
	return entries.length;
      }
    catch (NullPointerException ex)
      {
	throw new IllegalStateException("ZipFile has closed");
      }
  }
  
  private static class ZipEntryEnumeration implements Enumeration
  {
    ZipEntry[] array;
    int ptr = 0;

    public ZipEntryEnumeration(ZipEntry[] arr)
    {
      array = arr;
    }

    public boolean hasMoreElements()
    {
      return ptr < array.length;
    }

    public Object nextElement()
    {
      try
	{
	  /* We return a clone, just to be safe that the user doesn't
	   * change the entry.  
	   */
	  return array[ptr++].clone();
	}
      catch (ArrayIndexOutOfBoundsException ex)
	{
	  throw new NoSuchElementException();
	}
    }
  }

  private static class PartialInputStream extends InputStream
  {
    RandomAccessFile raf;
    long filepos, end;

    public PartialInputStream(RandomAccessFile raf, long start, long len)
    {
      this.raf = raf;
      filepos = start;
      end = start + len;
    }
    
    public int available()
    {
      long amount = end - filepos;
      if (amount > Integer.MAX_VALUE)
	return Integer.MAX_VALUE;
      return (int) amount;
    }
    
    public int read() throws IOException
    {
      if (filepos == end)
	return -1;
      synchronized (raf)
	{
	  raf.seek(filepos++);
	  return raf.read();
	}
    }

    public int read(byte[] b, int off, int len) throws IOException
    {
      if (len > end - filepos)
	{
	  len = (int) (end - filepos);
	  if (len == 0)
	    return -1;
	}
      synchronized (raf)
	{
	  raf.seek(filepos);
	  int count = raf.read(b, off, len);
	  if (count > 0)
	    filepos += len;
	  return count;
	}
    }

    public long skip(long amount)
    {
      if (amount < 0)
	throw new IllegalArgumentException();
      if (amount > end - filepos)
	amount = end - filepos;
      filepos += amount;
      return amount;
    }
  }
}
