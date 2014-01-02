/* Copyright (C) 2004-2014 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.tools.gcj_dbtool;


import gnu.gcj.runtime.PersistentByteMap;
import java.io.*;
import java.nio.channels.*;
import java.util.*;
import java.util.jar.*;
import java.security.MessageDigest;

public class Main
{
  static private boolean verbose = false;

  public static void main (String[] s)
  {
    boolean fileListFromStdin = false;
    char filenameSeparator = ' ';

    insist (s.length >= 1);

    if (s[0].equals("-") ||
	s[0].equals("-0"))
      {
	if (s[0].equals("-0"))
	  filenameSeparator = (char)0;
	fileListFromStdin = true;
	String[] newArgs = new String[s.length - 1];
	System.arraycopy(s, 1, newArgs, 0, s.length - 1);
	s = newArgs;
      }

    if (s[0].equals("-v") || s[0].equals("--version"))
      {
	insist (s.length == 1);
	System.out.println("gcj-dbtool ("
			   + System.getProperty("java.vm.name")
			   + ") "
			   + System.getProperty("java.vm.version"));
	System.out.println();
	System.out.println("Copyright 2014 Free Software Foundation, Inc.");
	System.out.println("This is free software; see the source for copying conditions.  There is NO");
	System.out.println("warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.");
	return;
      }
    if (s[0].equals("--help"))
      {
	usage(System.out);
	return;
      }

    if (s[0].equals("-n"))
      {
	// Create a new database.
	insist (s.length >= 2 && s.length <= 3);

	int capacity = 32749;

	if (s.length == 3)
	  {	    
	    capacity = Integer.parseInt(s[2]);

	    if (capacity <= 2)
	      {
		usage(System.err);
		System.exit(1);
	      }
	  }
	    
	try
	  {
	    PersistentByteMap b 
	      = PersistentByteMap.emptyPersistentByteMap(new File(s[1]), 
							 capacity, capacity*32);
	  }
	catch (Exception e)
	  {
	    System.err.println ("error: could not create " 
				+ s[1] + ": " + e.toString());
	    System.exit(2);
	  }
	return;
      }

    if (s[0].equals("-a") || s[0].equals("-f"))
      {
	// Add a jar file to a database, creating it if necessary.
	// Copies the database, adds the jar file to the copy, and
	// then renames the new database over the old.
	try
	  {
	    insist (s.length == 4);
	    File database = new File(s[1]);
	    database = database.getAbsoluteFile();
	    File jar = new File(s[2]);	
	    PersistentByteMap map; 
	    if (database.isFile())
	      map = new PersistentByteMap(database, 
					  PersistentByteMap.AccessMode.READ_ONLY);
	    else
	      map = PersistentByteMap.emptyPersistentByteMap(database, 
							     100, 100*32);
	    File soFile = new File(s[3]);
	    if (! s[0].equals("-f") && ! soFile.isFile())
	      throw new IllegalArgumentException(s[3] + " is not a file");
 	    map = addJar(jar, map, soFile);
	  }
	catch (Exception e)
	  {
	    System.err.println ("error: could not update " + s[1] 
				+ ": " + e.toString());
	    System.exit(2);
	  }
	return;
      }

    if (s[0].equals("-t"))
      {
	// Test
	try
	  {
	    insist (s.length == 2);
	    PersistentByteMap b 
	      = new PersistentByteMap(new File(s[1]),
				      PersistentByteMap.AccessMode.READ_ONLY);
	    Iterator iterator = b.iterator(PersistentByteMap.ENTRIES);
	
	    while (iterator.hasNext())
	      {
		PersistentByteMap.MapEntry entry 
		  = (PersistentByteMap.MapEntry)iterator.next();
		byte[] key = (byte[])entry.getKey();
		byte[] value = (byte[])b.get(key);
		if (! Arrays.equals (value, (byte[])entry.getValue()))
		  {
		    String err 
		      = ("Key " + bytesToString(key) + " at bucket " 
			 + entry.getBucket());
		  
		    throw new RuntimeException(err);
		  }
	      }
	  }
	catch (Exception e)
	  {
	    e.printStackTrace();
	    System.exit(3);
	  }
	return;
      }
	 
    if (s[0].equals("-m"))
      {
	// Merge databases.
	insist (s.length >= 3
		|| fileListFromStdin && s.length == 2);
	try
	  {
	    File database = new File(s[1]);
	    database = database.getAbsoluteFile();
	    File temp = File.createTempFile(database.getName(), "", 
					    database.getParentFile());
	    	
	    int newSize = 0;
	    int newStringTableSize = 0;
	    Fileset files = getFiles(s, 2, fileListFromStdin, 
				     filenameSeparator);
	    PersistentByteMap[] sourceMaps 
	      = new PersistentByteMap[files.size()];

	    // Scan all the input files, calculating worst case string
	    // table and hash table use.
	    {
	      Iterator it = files.iterator();
	      int i = 0;
	      while (it.hasNext())
		{
		  PersistentByteMap b 
		    = new PersistentByteMap((File)it.next(),
					    PersistentByteMap.AccessMode.READ_ONLY);
		  newSize += b.size();
		  newStringTableSize += b.stringTableSize();
		  sourceMaps[i++] = b;
		}
	    }
	    
	    newSize *= 1.5; // Scaling the new size by 1.5 results in
			    // fewer collisions.
	    PersistentByteMap map 
	      = PersistentByteMap.emptyPersistentByteMap
	      (temp, newSize, newStringTableSize);

	    for (int i = 0; i < sourceMaps.length; i++)
	      {
		if (verbose)
		  System.err.println("adding " + sourceMaps[i].size() 
				     + " elements from "
				     + sourceMaps[i].getFile());
		map.putAll(sourceMaps[i]);
	      }
	    map.close();
	    temp.renameTo(database);
	  }
	catch (Exception e)
	  {
	    e.printStackTrace();
	    System.exit(3);
	  }
	return;
      }

    if (s[0].equals("-l"))
      {
	// List a database.
	insist (s.length == 2);
	try
	  {
	    PersistentByteMap b 
	      = new PersistentByteMap(new File(s[1]),
				      PersistentByteMap.AccessMode.READ_ONLY);

	    System.out.println ("Capacity: " + b.capacity());
	    System.out.println ("Size: " + b.size());
	    System.out.println ();

	    System.out.println ("Elements: ");
	    Iterator iterator = b.iterator(PersistentByteMap.ENTRIES);
    
	    while (iterator.hasNext())
	      {
		PersistentByteMap.MapEntry entry 
		  = (PersistentByteMap.MapEntry)iterator.next();
		byte[] digest = (byte[])entry.getKey();
		System.out.print ("[" + entry.getBucket() + "] " 
				  + bytesToString(digest)
				  + " -> ");
		System.out.println (new String((byte[])entry.getValue()));
	      }
	  }
	catch (Exception e)
	  {
	    System.err.println ("error: could not list " 
				+ s[1] + ": " + e.toString());
	    System.exit(2);
	  }
	return;
      }

    if (s[0].equals("-d"))
      {
	// For testing only: fill the byte map with random data.
	insist (s.length == 2);
	try
	  {    
	    MessageDigest md = MessageDigest.getInstance("MD5");
	    PersistentByteMap b 
	      = new PersistentByteMap(new File(s[1]), 
				      PersistentByteMap.AccessMode.READ_WRITE);
	    int N = b.capacity();
	    byte[] bytes = new byte[1];
	    byte digest[] = md.digest(bytes);
	    for (int i = 0; i < N; i++)
	      {
		digest = md.digest(digest);
		b.put(digest, digest);
	      }
	  }
	catch (Exception e)
	  {
	    e.printStackTrace();
	    System.exit(3);
	  }	    
	return;
      }

    if (s[0].equals("-p"))
      {
	insist (s.length == 1 || s.length == 2);
	String result;
	
	if (s.length == 1)
	  result = System.getProperty("gnu.gcj.precompiled.db.path", "");
	else 
	  result = (s[1] 
		    + (s[1].endsWith(File.separator) ? "" : File.separator)
		    + getDbPathTail ());

	System.out.println (result);
	return;
      }

    usage(System.err);
    System.exit(1);	    
  }

  private static native String getDbPathTail ();
    
  private static void insist(boolean ok)
  {
    if (! ok)
      {
	usage(System.err);
	System.exit(1);
      }	    
  }

  private static void usage(PrintStream out)
  {
    out.println
      ("gcj-dbtool: Manipulate gcj map database files\n"
       + "\n"
       + "  Usage: \n"
       + "    gcj-dbtool -n file.gcjdb [size]     - Create a new gcj map database\n"
       + "    gcj-dbtool -a file.gcjdb file.jar file.so\n"
       + "            - Add the contents of file.jar to a gcj map database\n"
       + "    gcj-dbtool -f file.gcjdb file.jar file.so\n"
       + "            - Add the contents of file.jar to a gcj map database\n"
       + "    gcj-dbtool -t file.gcjdb            - Test a gcj map database\n"
       + "    gcj-dbtool -l file.gcjdb            - List a gcj map database\n"
       + "    gcj-dbtool [-][-0] -m dest.gcjdb [source.gcjdb]...\n"
       + "            - Merge gcj map databases into dest\n"
       + "              Replaces dest\n"
       + "              To add to dest, include dest in the list of sources\n"
       + "              If the first arg is -, read the list from stdin\n"
       + "              If the first arg is -0, filenames separated by nul\n"
       + "    gcj-dbtool -p [LIBDIR]              - Print default database name"
       );
  }

  // Add a jar to a map.  This copies the map first and returns a
  // different map that contains the data.  The original map is
  // closed.

  private static PersistentByteMap 
  addJar(File f, PersistentByteMap b, File soFile)
    throws Exception
  {
    MessageDigest md = MessageDigest.getInstance("MD5");

    JarFile jar = new JarFile (f);

    int count = 0;
    {
      Enumeration entries = jar.entries();      
      while (entries.hasMoreElements())
	{
	  JarEntry classfile = (JarEntry)entries.nextElement();
	  if (classfile.getName().endsWith(".class"))
	    count++;
	}
    }

    if (verbose)
      System.err.println("adding " + count + " elements from "
			 + f + " to " + b.getFile());
    
    // Maybe resize the destination map.  We're allowing plenty of
    // extra space by using a loadFactor of 2.  
    b = resizeMap(b, (b.size() + count) * 2, true);

    Enumeration entries = jar.entries();

    byte[] soFileName = soFile.getCanonicalPath().getBytes("UTF-8");
    while (entries.hasMoreElements())
      {
	JarEntry classfile = (JarEntry)entries.nextElement();
	if (classfile.getName().endsWith(".class"))
	  {
	    InputStream str = jar.getInputStream(classfile);
	    int length = (int) classfile.getSize();
	    if (length == -1)
	      throw new EOFException();

	    byte[] data = new byte[length];
	    int pos = 0;
	    while (length - pos > 0)
	      {
		int len = str.read(data, pos, length - pos);
		if (len == -1)
		  throw new EOFException("Not enough data reading from: "
					 + classfile.getName());
		pos += len;
	      }
	    b.put(md.digest(data), soFileName);
	  }
      }
    return b;
  }    

  // Resize a map by creating a new one with the same data and
  // renaming it.  If close is true, close the original map.

  static PersistentByteMap resizeMap(PersistentByteMap m, int newCapacity, boolean close)
    throws IOException, IllegalAccessException
  {
    newCapacity = Math.max(m.capacity(), newCapacity);
    File name = m.getFile();
    File copy = File.createTempFile(name.getName(), "", name.getParentFile());
    try
      {
	PersistentByteMap dest 
	  = PersistentByteMap.emptyPersistentByteMap
	  (copy, newCapacity, newCapacity*32);
	dest.putAll(m);
	dest.force();
	if (close)
	  m.close();
	copy.renameTo(name);
	return dest;
      }
    catch (Exception e)
      {
	copy.delete();
      }
    return null;
  }
    
	 
  static String bytesToString(byte[] b)
  {
    StringBuffer hexBytes = new StringBuffer();
    int length = b.length;
    for (int i = 0; i < length; ++i)
      {
	int v = b[i] & 0xff;
	if (v < 16)
	  hexBytes.append('0');
	hexBytes.append(Integer.toHexString(v));
      }
    return hexBytes.toString();
  }


  // Return a Fileset, either from a String array or from System.in,
  // depending on fileListFromStdin.
  private static final Fileset getFiles(String[] s, int startPos,
					boolean fileListFromStdin,
					char separator)
  {
    if (fileListFromStdin)
      return new Fileset(System.in, separator);
    else
      return new Fileset(s, startPos, s.length);
  }
}

// Parse a stream into tokens.  The separator can be any char, and
// space is equivalent to any whitepace character.
class Tokenizer
{
  final Reader r;
  final char separator;

  Tokenizer(Reader r, char separator)
  {
    this.r = r;
    this.separator = separator;
  }

  boolean isSeparator(int c)
  {
    if (Character.isWhitespace(separator))
      return Character.isWhitespace((char)c);
    else
      return c == separator;
  }

  // Parse a token from the input stream.  Return the empty string
  // when the stream is exhausted.
  String nextToken ()
  {
    StringBuffer buf = new StringBuffer();
    int c;
    try
      {
	while ((c = r.read()) != -1)
	  {
	    if (! isSeparator(c))
	      {
		buf.append((char)c);
		break;
	      }
	  }
	while ((c = r.read()) != -1)
	  {
	    if (isSeparator(c))
	      break;
	    else
	      buf.append((char)c);
	  }
      }
    catch (java.io.IOException e)
      {
      }
    return buf.toString();
  }
}

// A Fileset is a container for a set of files; it can be created
// either from a string array or from an input stream, given a
// separator character.
class Fileset
{
  LinkedHashSet files = new LinkedHashSet();
  
  Fileset (String[] s, int start, int end)
  {
    for (int i = start; i < end; i++)
      {
	files.add(new File(s[i]));
      }
  }

  Fileset (InputStream is, char separator)
  {
    Reader r = new BufferedReader(new InputStreamReader(is));
    Tokenizer st = new Tokenizer(r, separator);
    String name;
    while (! "".equals(name = st.nextToken()))
      files.add(new File(name));
  }

  Iterator iterator()
  {
    return files.iterator();
  }

  int size()
  {
    return files.size();
  }
}
