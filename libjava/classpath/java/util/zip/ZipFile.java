/* ZipFile.java --
   Copyright (C) 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

import gnu.java.util.EmptyEnumeration;

import java.io.BufferedInputStream;
import java.io.DataInput;
import java.io.EOFException;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.io.UnsupportedEncodingException;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;

/**
 * This class represents a Zip archive.  You can ask for the contained
 * entries, or get an input stream for a file entry.  The entry is
 * automatically decompressed.
 *
 * This class is thread safe:  You can open input streams for arbitrary
 * entries in different threads.
 *
 * @author Jochen Hoenicke
 * @author Artur Biesiadowski
 */
public class ZipFile implements ZipConstants
{

  /**
   * Mode flag to open a zip file for reading.
   */
  public static final int OPEN_READ = 0x1;

  /**
   * Mode flag to delete a zip file after reading.
   */
  public static final int OPEN_DELETE = 0x4;

  // Name of this zip file.
  private final String name;

  // File from which zip entries are read.
  private final RandomAccessFile raf;

  // The entries of this zip file when initialized and not yet closed.
  private HashMap entries;

  private boolean closed = false;

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
    checkZipFile();
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
    this.name = file.getPath();
    checkZipFile();
  }

  /**
   * Opens a Zip file reading the given File in the given mode.
   *
   * If the OPEN_DELETE mode is specified, the zip file will be deleted at
   * some time moment after it is opened. It will be deleted before the zip
   * file is closed or the Virtual Machine exits.
   * 
   * The contents of the zip file will be accessible until it is closed.
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
    if (mode != OPEN_READ && mode != (OPEN_READ | OPEN_DELETE))
      throw new IllegalArgumentException("invalid mode");
    if ((mode & OPEN_DELETE) != 0)
      file.deleteOnExit();
    this.raf = new RandomAccessFile(file, "r");
    this.name = file.getPath();
    checkZipFile();
  }

  private void checkZipFile() throws IOException, ZipException
  {
    byte[] magicBuf = new byte[4];
    boolean validRead = true;

    try 
      {
	raf.readFully(magicBuf);
      } 
    catch (EOFException eof) 
      {
	validRead = false;
      } 

    if (validRead == false || readLeInt(magicBuf, 0) != LOCSIG)
      {
	raf.close();
	throw new ZipException("Not a valid zip file");
      }
  }

  /**
   * Checks if file is closed and throws an exception.
   */
  private void checkClosed()
  {
    if (closed)
      throw new IllegalStateException("ZipFile has closed: " + name);
  }

  /**
   * Read an unsigned short in little endian byte order from the given
   * DataInput stream using the given byte buffer.
   *
   * @param di DataInput stream to read from.
   * @param b the byte buffer to read in (must be at least 2 bytes long).
   * @return The value read.
   *
   * @exception IOException if a i/o error occured.
   * @exception EOFException if the file ends prematurely
   */
  private int readLeShort(DataInput di, byte[] b) throws IOException
  {
    di.readFully(b, 0, 2);
    return (b[0] & 0xff) | (b[1] & 0xff) << 8;
  }

  /**
   * Read an int in little endian byte order from the given
   * DataInput stream using the given byte buffer.
   *
   * @param di DataInput stream to read from.
   * @param b the byte buffer to read in (must be at least 4 bytes long).
   * @return The value read.
   *
   * @exception IOException if a i/o error occured.
   * @exception EOFException if the file ends prematurely
   */
  private int readLeInt(DataInput di, byte[] b) throws IOException
  {
    di.readFully(b, 0, 4);
    return ((b[0] & 0xff) | (b[1] & 0xff) << 8)
	    | ((b[2] & 0xff) | (b[3] & 0xff) << 8) << 16;
  }

  /**
   * Read an unsigned short in little endian byte order from the given
   * byte buffer at the given offset.
   *
   * @param b the byte array to read from.
   * @param off the offset to read from.
   * @return The value read.
   */
  private int readLeShort(byte[] b, int off)
  {
    return (b[off] & 0xff) | (b[off+1] & 0xff) << 8;
  }

  /**
   * Read an int in little endian byte order from the given
   * byte buffer at the given offset.
   *
   * @param b the byte array to read from.
   * @param off the offset to read from.
   * @return The value read.
   */
  private int readLeInt(byte[] b, int off)
  {
    return ((b[off] & 0xff) | (b[off+1] & 0xff) << 8)
	    | ((b[off+2] & 0xff) | (b[off+3] & 0xff) << 8) << 16;
  }
  

  /**
   * Read the central directory of a zip file and fill the entries
   * array.  This is called exactly once when first needed. It is called
   * while holding the lock on <code>raf</code>.
   *
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
    byte[] ebs  = new byte[CENHDR];
    
    do
      {
	if (pos < 0)
	  throw new ZipException
	    ("central directory not found, probably not a zip file: " + name);
	raf.seek(pos--);
      }
    while (readLeInt(raf, ebs) != ENDSIG);
    
    if (raf.skipBytes(ENDTOT - ENDNRD) != ENDTOT - ENDNRD)
      throw new EOFException(name);
    int count = readLeShort(raf, ebs);
    if (raf.skipBytes(ENDOFF - ENDSIZ) != ENDOFF - ENDSIZ)
      throw new EOFException(name);
    int centralOffset = readLeInt(raf, ebs);

    entries = new HashMap(count+count/2);
    raf.seek(centralOffset);
    
    byte[] buffer = new byte[16];
    for (int i = 0; i < count; i++)
      {
      	raf.readFully(ebs);
	if (readLeInt(ebs, 0) != CENSIG)
	  throw new ZipException("Wrong Central Directory signature: " + name);

	int method = readLeShort(ebs, CENHOW);
	int dostime = readLeInt(ebs, CENTIM);
	int crc = readLeInt(ebs, CENCRC);
	int csize = readLeInt(ebs, CENSIZ);
	int size = readLeInt(ebs, CENLEN);
	int nameLen = readLeShort(ebs, CENNAM);
	int extraLen = readLeShort(ebs, CENEXT);
	int commentLen = readLeShort(ebs, CENCOM);
	
	int offset = readLeInt(ebs, CENOFF);

	int needBuffer = Math.max(nameLen, commentLen);
	if (buffer.length < needBuffer)
	  buffer = new byte[needBuffer];

	raf.readFully(buffer, 0, nameLen);
	String name;
	try
	  {
	    name = new String(buffer, 0, nameLen, "UTF-8");
	  }
	catch (UnsupportedEncodingException uee)
	  {
	    throw new AssertionError(uee);
	  }

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
	    try
	      {
		entry.setComment(new String(buffer, 0, commentLen, "UTF-8"));
	      }
	    catch (UnsupportedEncodingException uee)
	      {
		throw new AssertionError(uee);
	      }
	  }
	entry.offset = offset;
	entries.put(name, entry);
      }
  }

  /**
   * Closes the ZipFile.  This also closes all input streams given by
   * this class.  After this is called, no further method should be
   * called.
   * 
   * @exception IOException if a i/o error occured.
   */
  public void close() throws IOException
  {
    RandomAccessFile raf = this.raf;
    if (raf == null)
      return;

    synchronized (raf)
      {
	closed = true;
	entries = null;
	raf.close();
      }
  }

  /**
   * Calls the <code>close()</code> method when this ZipFile has not yet
   * been explicitly closed.
   */
  protected void finalize() throws IOException
  {
    if (!closed && raf != null) close();
  }

  /**
   * Returns an enumeration of all Zip entries in this Zip file.
   *
   * @exception IllegalStateException when the ZipFile has already been closed
   */
  public Enumeration entries()
  {
    checkClosed();
    
    try
      {
	return new ZipEntryEnumeration(getEntries().values().iterator());
      }
    catch (IOException ioe)
      {
	return EmptyEnumeration.getInstance();
      }
  }

  /**
   * Checks that the ZipFile is still open and reads entries when necessary.
   *
   * @exception IllegalStateException when the ZipFile has already been closed.
   * @exception IOException when the entries could not be read.
   */
  private HashMap getEntries() throws IOException
  {
    synchronized(raf)
      {
	checkClosed();

	if (entries == null)
	  readEntries();

	return entries;
      }
  }

  /**
   * Searches for a zip entry in this archive with the given name.
   *
   * @param name the name. May contain directory components separated by
   * slashes ('/').
   * @return the zip entry, or null if no entry with that name exists.
   *
   * @exception IllegalStateException when the ZipFile has already been closed
   */
  public ZipEntry getEntry(String name)
  {
    checkClosed();

    try
      {
	HashMap entries = getEntries();
	ZipEntry entry = (ZipEntry) entries.get(name);
        // If we didn't find it, maybe it's a directory.
        if (entry == null && !name.endsWith("/"))
            entry = (ZipEntry) entries.get(name + '/');
	return entry != null ? new ZipEntry(entry, name) : null;
      }
    catch (IOException ioe)
      {
	return null;
      }
  }


  //access should be protected by synchronized(raf)
  private byte[] locBuf = new byte[LOCHDR];

  /**
   * Checks, if the local header of the entry at index i matches the
   * central directory, and returns the offset to the data.
   * 
   * @param entry to check.
   * @return the start offset of the (compressed) data.
   *
   * @exception IOException if a i/o error occured.
   * @exception ZipException if the local header doesn't match the 
   * central directory header
   */
  private long checkLocalHeader(ZipEntry entry) throws IOException
  {
    synchronized (raf)
      {
	raf.seek(entry.offset);
	raf.readFully(locBuf);
	
	if (readLeInt(locBuf, 0) != LOCSIG)
	  throw new ZipException("Wrong Local header signature: " + name);

	if (entry.getMethod() != readLeShort(locBuf, LOCHOW))
	  throw new ZipException("Compression method mismatch: " + name);

	if (entry.getName().length() != readLeShort(locBuf, LOCNAM))
	  throw new ZipException("file name length mismatch: " + name);

	int extraLen = entry.getName().length() + readLeShort(locBuf, LOCEXT);
	return entry.offset + LOCHDR + extraLen;
      }
  }

  /**
   * Creates an input stream reading the given zip entry as
   * uncompressed data.  Normally zip entry should be an entry
   * returned by getEntry() or entries().
   *
   * This implementation returns null if the requested entry does not
   * exist.  This decision is not obviously correct, however, it does
   * appear to mirror Sun's implementation, and it is consistant with
   * their javadoc.  On the other hand, the old JCL book, 2nd Edition,
   * claims that this should return a "non-null ZIP entry".  We have
   * chosen for now ignore the old book, as modern versions of Ant (an
   * important application) depend on this behaviour.  See discussion
   * in this thread:
   * http://gcc.gnu.org/ml/java-patches/2004-q2/msg00602.html
   *
   * @param entry the entry to create an InputStream for.
   * @return the input stream, or null if the requested entry does not exist.
   *
   * @exception IllegalStateException when the ZipFile has already been closed
   * @exception IOException if a i/o error occured.
   * @exception ZipException if the Zip archive is malformed.  
   */
  public InputStream getInputStream(ZipEntry entry) throws IOException
  {
    checkClosed();

    HashMap entries = getEntries();
    String name = entry.getName();
    ZipEntry zipEntry = (ZipEntry) entries.get(name);
    if (zipEntry == null)
      return null;

    long start = checkLocalHeader(zipEntry);
    int method = zipEntry.getMethod();
    InputStream is = new BufferedInputStream(new PartialInputStream
      (raf, start, zipEntry.getCompressedSize()));
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
   * Returns the (path) name of this zip file.
   */
  public String getName()
  {
    return name;
  }

  /**
   * Returns the number of entries in this zip file.
   *
   * @exception IllegalStateException when the ZipFile has already been closed
   */
  public int size()
  {
    checkClosed();
    
    try
      {
	return getEntries().size();
      }
    catch (IOException ioe)
      {
	return 0;
      }
  }
  
  private static class ZipEntryEnumeration implements Enumeration
  {
    private final Iterator elements;

    public ZipEntryEnumeration(Iterator elements)
    {
      this.elements = elements;
    }

    public boolean hasMoreElements()
    {
      return elements.hasNext();
    }

    public Object nextElement()
    {
      /* We return a clone, just to be safe that the user doesn't
       * change the entry.  
       */
      return ((ZipEntry)elements.next()).clone();
    }
  }

  private static class PartialInputStream extends InputStream
  {
    private final RandomAccessFile raf;
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
