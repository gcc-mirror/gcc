/* Copyright (C) 2004  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.tools.gcj_dbtool;


import gnu.gcj.runtime.PersistentByteMap;
import java.io.*;
import java.util.*;
import java.util.jar.*;
import java.security.MessageDigest;
import java.math.BigInteger;

public class Main
{
  public static void main (String[] s)
  {
    insist (s.length >= 1);
    if (s[0].equals("-v") || s[0].equals("--version"))
      {
	insist (s.length == 1);
	System.out.println("gcj-dbtool ("
			   + System.getProperty("java.vm.name")
			   + ") "
			   + System.getProperty("java.vm.version"));
	System.out.println();
	System.out.println("Copyright 2004 Free Software Foundation, Inc.");
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
	insist (s.length >= 2 && s.length <= 3);

	int capacity = 32749;

	if (s.length == 3)
	  {
	    // The user has explicitly provided a size for the table.
	    // We're going to make that size prime.  This isn't
	    // strictly necessary but it can't hurt.

	    BigInteger size = new BigInteger(s[2], 10);
	    BigInteger two = BigInteger.ONE.add(BigInteger.ONE);

	    if (size.getLowestSetBit() != 0) // A hard way to say isEven()
	      size = size.add(BigInteger.ONE);

	    while (! size.isProbablePrime(10))
	      size = size.add(two);

	    capacity = size.intValue();

	    if (capacity <= 2)
	      {
		usage(System.err);
		System.exit(1);
	      }
	  }
	    
	try
	  {
	    PersistentByteMap b 
	      = PersistentByteMap.emptyPersistentByteMap (s[1], capacity, capacity*64);
	  }
	catch (Exception e)
	  {
	    System.err.println ("error: could not create " 
				+ s[1] + ": " + e.toString());
	    System.exit(2);
	  }
	return;
      }

    if (s[0].equals("-a"))
      {
	try
	  {
	    insist (s.length == 4);
	    File jar = new File(s[2]);
	    PersistentByteMap b 
	      = new PersistentByteMap(new File(s[1]), 
				      PersistentByteMap.AccessMode.READ_WRITE);
	    File soFile = new File(s[3]);
	    if (! soFile.isFile())
	      throw new IllegalArgumentException(s[3] + " is not a file");
	    
	    addJar(jar, b, soFile);
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
	 
    if (s[0].equals("-l"))
      {
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
    
    usage(System.err);
    System.exit(1);	    
  }

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
       + "            - Add the contents of file.jar to the database\n"
       + "    gcj-dbtool -t file.gcjdb            - Test a gcj map database\n"
       + "    gcj-dbtool -l file.gcjdb            - List a gcj map database\n");
  }
      

  private static void addJar(File f, PersistentByteMap b, File soFile)
   throws Exception
  {
    MessageDigest md = MessageDigest.getInstance("MD5");

    JarFile jar = new JarFile (f);
    Enumeration entries = jar.entries();

    while (entries.hasMoreElements())
      {
	JarEntry classfile = (JarEntry)entries.nextElement();
	if (classfile.getName().endsWith(".class"))
	  {
	    InputStream str = jar.getInputStream(classfile);
	    long length = classfile.getSize();
	    if (length == -1)
	      throw new EOFException();

	    byte[] data = new byte[length];
	    int pos = 0;
	    while (length - pos > 0)
	      {
		int len = str.read(data, pos, (int)(length - pos));
		if (len == -1)
		  throw new EOFException("Not enough data reading from: "
					 + classfile.getName());
		pos += len;
	      }
	    b.put(md.digest(data), 
		  soFile.getCanonicalPath().getBytes());
	  }
      }	      
  }    

  static String bytesToString(byte[] b)
  {
    StringBuffer hexBytes = new StringBuffer();
    int length = b.length;
    for (int i = 0; i < length; ++i)
      hexBytes.append(Integer.toHexString(b[i] & 0xff));
    return hexBytes.toString();
  }
}
    
