/* Copyright (C) 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */



/*  A PersistentByteMap maps a byte array to another byte array.  It
uses a file that does not need to be serialized but may be
memory-mapped and read in-place.  So, even if there are many instances
of gcj applications running, the can share PersistentByteMaps.

The idea is to make searches as fast as possible: opening a
PersistentByteMap is cheap and search time doesn't grow with the
number of entries in the table.  On the other hand, enumerating the
map is slow, but that is a relatively uncommon operation.

The main use of this class is to provide a way to map the
MessageDigest of a class file to the location of a DSO that contains
the compiled version of that class.  It is up the the installer of an
application to keep the DSO up to date with the jar.  

USAGE:
        MessageDigest md = MessageDigest.getInstance("MD5");
        digest = md.digest(bytes);

        PersistentByteMap map 
          = new PersistentByteMap
            (fileName, PersistentByteMap.AccessMode.READ_ONLY);

        byte[] soName = map.get(digest);
        if (soName)
          {
            String SharedLibraryName = new String(soName);

BUGS/FEATURES:
        remove() isn't written yet.

	we can't change the capacity of a PersistentByteMap.

        0x12345678 is a bad choice for the magic number.

        capacity is fixed once the map has been created.

        We use linear probing to resolve collisions.  It might be
        better to use a scheme that results in fewer probes to
        determine that an item isn't found.  However, even when the
        table is half full there are only on average 1.5 probes for a
        successful search and 2.5 probes for an unsuccessful one.

        We don't use unique strings.  This wastes space.

        capacity should probably be prime, but we don't check that.

	we don't do any locking at all: adding to a PersistentByteMap
	at runtime is possible, but it requires filesystem locks
	around get(), put(), and remove().
*/

package gnu.gcj.runtime;

import java.io.*;
import java.nio.*;
import java.nio.channels.*;
import java.util.*;
import java.security.MessageDigest;

public class PersistentByteMap
{
  private MappedByteBuffer buf;

  static private final int MAGIC = 0;
  static private final int VERSION = 4;
  static private final int CAPACITY = 8;
  static private final int TABLE_BASE = 12;
  static private final int STRING_BASE = 16;
  static private final int STRING_SIZE = 20;
  static private final int FILE_SIZE = 24;
  static private final int ELEMENTS = 28;
  
  static private final int INT_SIZE = 4;

  static private final int TABLE_ENTRY_SIZE = 2 * INT_SIZE;

  private int capacity;   // number of entries
  private int table_base;   // offset from start of file, in bytes
  private int string_base;  // offset from start of file, in bytes
  private int string_size;  // size of string table, in bytes
  private int file_size;    // size of file, in bytes;
  private int elements;     // number of elements in table

  private long length;      // the length of the underlying file

  static private final int UNUSED_ENTRY = -1; 

  static public final int KEYS = 0;
  static public final int VALUES = 1;
  static public final int ENTRIES = 2;

  static final public class AccessMode
  {
    private final FileChannel.MapMode mapMode;

    static
    {
      READ_ONLY = new AccessMode(FileChannel.MapMode.READ_ONLY);
      READ_WRITE = new AccessMode(FileChannel.MapMode.READ_WRITE);
    }

    public static final AccessMode READ_ONLY;
    public static final AccessMode READ_WRITE; 

    private AccessMode(FileChannel.MapMode mode)
    {
      this.mapMode = mode;
    }
  }

  private PersistentByteMap()
  {
  }

  public PersistentByteMap(String filename, AccessMode mode)
    throws IOException 
  {
    this(new File(filename), mode);
  }

  public PersistentByteMap(File f, AccessMode mode)
    throws IOException 
  {
    FileChannel fc;

    if (mode == AccessMode.READ_ONLY)
      {
        FileInputStream fis = new FileInputStream(f);
        fc = fis.getChannel();
      }
    else
      {
        RandomAccessFile fos = new RandomAccessFile(f, "rw");
        fc = fos.getChannel();
      }

    length = fc.size();
    buf = fc.map(mode.mapMode, 0, length);

    int magic = getWord (MAGIC);
    if (magic != 0x12345678)
      throw new IllegalArgumentException(f.getName());

    table_base = getWord (TABLE_BASE);
    capacity = getWord (CAPACITY);
    string_base = getWord (STRING_BASE);
    string_size = getWord (STRING_SIZE);
    file_size = getWord (FILE_SIZE);
    elements = getWord (ELEMENTS);

    // FIXME:  Insert a bunch of sanity checks here
  }

  private void init (PersistentByteMap m, File f, int capacity, int strtabSize)
    throws IOException 
  {
    f.createNewFile();
    RandomAccessFile raf = new RandomAccessFile(f, "rw");
        
    this.capacity = capacity;
    table_base = 64;
    string_base = table_base + capacity * TABLE_ENTRY_SIZE;
    string_size = 0;
    file_size = string_base;
    elements = 0;

    int totalFileSize = string_base + strtabSize;

    // Create the file; this rounds up the size of the file to a fixed
    // number of 4k pages.
    byte[] _4k = new byte[4096];
    for (long i = 0; i < totalFileSize; i+= 4096)
      raf.write(_4k);
        
    FileChannel fc = raf.getChannel();
    buf = fc.map(FileChannel.MapMode.READ_WRITE, 0, raf.length());

    for (int i = 0; i < capacity; i++)
      putKeyPos(UNUSED_ENTRY, i);
        
    putWord(0x12345678, MAGIC);
    putWord(0x01, VERSION);
    putWord(capacity, CAPACITY);
    putWord(table_base, TABLE_BASE);
    putWord(string_base, STRING_BASE);
    putWord(file_size, FILE_SIZE);
    putWord(elements, ELEMENTS);
    buf.force();
  }     

  static public PersistentByteMap emptyPersistentByteMap(String filename, 
                                                         int capacity, int strtabSize)
    throws IOException 
  {
    File f = new File(filename);
    PersistentByteMap m = new PersistentByteMap();
    m.init(m, f, capacity, strtabSize);
    return m;
  }     

  private int getWord (int index)
  {
    buf.position(index);
    byte[] wordBuf = new byte[4];
    buf.get(wordBuf);

    int result = (int)wordBuf[0]&0xff;
    result += ((int)wordBuf[1]&0xff) << 8;
    result += ((int)wordBuf[2]&0xff) << 16;
    result += ((int)wordBuf[3]&0xff) << 24;
    return result;
  }

  private void putWord (int word, int index)
  {
    buf.position(index);
    byte[] wordBuf = new byte[4];
    wordBuf[0] = (byte)(word);
    wordBuf[1] = (byte)(word >>> 8);
    wordBuf[2] = (byte)(word >>> 16);
    wordBuf[3] = (byte)(word >>> 24);
    buf.put(wordBuf);
  }

  public Set entrySet()
  {
    return null;
  }

  private int getBucket(int n)
  {
    return table_base + (2*n * INT_SIZE);
  }

  private int getKeyPos(int n)
  {
    return getWord(getBucket(n));
  }
  
  private int getValuePos(int n)
  {
    return getWord(getBucket(n) + INT_SIZE);
  }

  private void putKeyPos(int index, int n)
  {
    putWord(index, getBucket(n));
  }

  private void putValuePos(int index, int n)
  {
    putWord(index, getBucket(n) + INT_SIZE);
  }

  private byte[] getBytes(int n)
  {
    int len = getWord (string_base + n);
    int base = string_base + n + INT_SIZE;
    byte[] key = new byte[len];
    buf.position(base);
    buf.get(key, 0, len);
    return key;
  }

  private int hash (byte[] b)
  {    
    // We assume that the message digest is evenly distributed, so we
    // only need to use a few bytes of it as the hash function.
    long hashIndex 
      = ((b[0]&0xffL)
         + ((b[1]&0xffL)<<8) 
         + ((b[2]&0xffL)<<16) 
         + ((b[3]&0xffL)<<24));
    long result = hashIndex % (long)capacity;
    return (int)result;
  }
        
  public byte[] get(byte[] digest)
  {
    int hashIndex = hash(digest);

    do
      {
        int k = getKeyPos(hashIndex);
        if (k == UNUSED_ENTRY)
          return null;

        if (Arrays.equals ((byte[])digest, getBytes(k)))
          return getBytes(getValuePos(hashIndex));
                
        // Use linear probing to resolve hash collisions.  This may
        // not be theoretically as good as open addressing, but it has
        // good cache behviour.
        hashIndex++;
        hashIndex %= capacity;
      }
    while (true);
  }

  public void put(byte[] digest, byte[] value)
    throws IllegalAccessException
  {
    int hashIndex = hash(digest);

    // With the the table 2/3 full there will be on average 2 probes
    // for a successful search and 5 probes for an unsuccessful one.
    if (elements >= capacity * 2/3)
      throw new IllegalAccessException("Table Full: " + elements);

    do
      {
        int k = getKeyPos(hashIndex);
        if (k == UNUSED_ENTRY)
          {
            int newKey = addBytes(digest);
            putKeyPos(newKey, hashIndex);
            int newValue = addBytes(value);
            putValuePos(newValue, hashIndex);
            elements++;
            putWord(elements, ELEMENTS);            
            return;
          }
        else if (Arrays.equals (digest, getBytes(k)))
          {
            int newValue = addBytes((byte[])value);
            putValuePos(newValue, hashIndex);
            return;
          }         
                
        hashIndex++;
        hashIndex %= capacity;
      }
    while (true);
  }

  private int addBytes (byte[] data)
    throws IllegalAccessException
  {
    if (data.length + INT_SIZE >= this.length)
      throw new IllegalAccessException("String table Full");

    int extent = string_base+string_size;
    int top = extent;
    putWord(data.length, extent);
    extent += INT_SIZE;
    buf.position(extent);
    buf.put(data, 0, data.length);
    extent += data.length;
    extent += INT_SIZE-1;
    extent &= ~(INT_SIZE-1); // align
    string_size = extent - string_base;
    file_size = extent;
    putWord (string_size, STRING_SIZE);
    putWord (file_size, FILE_SIZE);
        
    return top - string_base;
  }

  public Iterator iterator(int type)
  {
    return new HashIterator(type);
  }

  public int size()
  {
    return elements;
  }

  public int capacity()
  {
    return capacity;
  }

  private final class HashIterator implements Iterator
  {
    /** Current index in the physical hash table. */

    private int idx;
    private int count;
    private final int type;

    /**
     * Construct a new HashIterator with the supplied type.
     * @param type {@link #KEYS}, {@link #VALUES}, or {@link #ENTRIES}
     */
    HashIterator(int type)
    {
      this.type = type;
      count = elements;
      idx = 0;
    }

    /**
     * Returns true if the Iterator has more elements.
     * @return true if there are more elements
     * @throws ConcurrentModificationException if the HashMap was modified
     */
    public boolean hasNext()
    {
      return count > 0;
    }

    /**
     * Returns the next element in the Iterator's sequential view.
     * @return the next element
     * @throws ConcurrentModificationException if the HashMap was modified
     * @throws NoSuchElementException if there is none
     */
    public Object next()
    {
      count--;
      for (int i = idx; i < capacity; i++)
        if (getKeyPos(i) != UNUSED_ENTRY)
          {
            idx = i+1;
            if (type == VALUES)
              return getBytes(getValuePos(i));
            if (type == KEYS)
              return getBytes(getKeyPos(i));
            return new MapEntry(i,
                                getBytes(getKeyPos(i)),
                                getBytes(getValuePos(i)));
          }
      return null;
    }    

    /**
     * Remove from the underlying collection the last element returned
     * by next (optional operation). This method can be called only
     * once after each call to <code>next()</code>. It does not affect
     * what will be returned by subsequent calls to next.
     *
     * @throws IllegalStateException if next has not yet been called
     *         or remove has already been called since the last call
     *         to next.
     * @throws UnsupportedOperationException if this Iterator does not
     *         support the remove operation.
     */
     public void remove()
    {
      throw new UnsupportedOperationException();
    }
  }

  static public final class MapEntry
  {
    private final Object key;
    private final Object value;
    private final int bucket;

    public MapEntry(int bucket, Object newKey, Object newValue)
    {
      this.key = newKey;
      this.value = newValue;
      this.bucket = bucket;
    }

    public final Object getKey()
    {
      return key;
    }

    public final Object getValue()
    {
      return value;
    }

    public final int getBucket()
    {
      return bucket;
    }
  }
}
