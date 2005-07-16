/*************************************************************************
/* File.java -- Tests File class
/*
/* Copyright (c) 1998 Free Software Foundation, Inc.
/* Written by Aaron M. Renn (arenn@urbanophile.com)
/*
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published 
/* by the Free Software Foundation, version 2. (see COPYING)
/*
/* This program is distributed in the hope that it will be useful, but
/* WITHOUT ANY WARRANTY; without even the implied warranty of
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/* GNU General Public License for more details.
/*
/* You should have received a copy of the GNU General Public License
/* along with this program; if not, write to the Free Software Foundation
/* Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
/*************************************************************************/

import java.io.*;

public class FileTest
{

static PrintWriter pw;

public static void
dumpFile(File f) throws IOException
{
    pw.println("Name: " + f.getName());
    pw.println("Parent: " + f.getParent());
    pw.println("Path: " + f.getPath());
    pw.println("Absolute: " + f.getAbsolutePath());
    pw.println("Canonical: " + f.getCanonicalPath());
    pw.println("String: " + f.toString());
}

public static void
deleteTempDirs() throws IOException
{
  File f = new File("tempfiletest/tmp/tmp");
  if (!f.delete())
    throw new IOException("Could not delete " + f.getPath());

  f = new File("tempfiletest/tmp");
  if (!f.delete())
    throw new IOException("Could not delete " + f.getPath());

  f = new File("tempfiletest/");
  if (!f.delete())
    throw new IOException("Could not delete " + f.getPath());
}

public static void main(String[] argv)
{
  System.out.println("Started test of File");

  // This test writes a bunch of things to a file.  That file should
  // be "diff-ed" against one generated when this test is run against
  // the JDK java.io package.
  System.out.println("Test 1: Path Operations Test");
  try
    {
      pw = new PrintWriter(new OutputStreamWriter(new 
                    FileOutputStream("./file-test.out"))); 

      dumpFile(new File("/"));
      dumpFile(new File("~arenn/foo"));
      dumpFile(new File("foo"));
      dumpFile(new File("../../../jcl/"));
      dumpFile(new File("/tmp/bar.txt"));
      dumpFile(new File("/usr"));
      dumpFile(new File("../.."));
      pw.flush();

      File f = new File("gimme");
      if (f.isAbsolute())
        throw new IOException("isAbsolute() failed");

      f = new File("/etc/services");
      if (!f.isFile())
        throw new IOException("isFile() failed");

      pw.println("length: " + f.length());
      pw.println("lastModified: " + f.lastModified());
      pw.println("hashCode: " + f.hashCode());

      f = new File("/etc/");
      if (!f.isDirectory())
        throw new IOException("isDirectory() failed");

      pw.close();
      System.out.println("PASSED: Conditionally Passed Path Operations Test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: Path Operations Test: " + e);
      pw.close();
    }

  System.out.println("Test 2: File/Directory Manipulation Test");
  try
    {
      File f = new File("filetest");
      if (!f.exists())
        throw new IOException("The filetest directory doesn't exist");

      String[] filelist = f.list();
      if ((filelist == null) || (filelist.length != 3))
        throw new IOException("Failed to read directory list");

      for (int i = 0; i < filelist.length; i++)
        System.out.println(filelist[i]);

      System.out.println("Listing /etc/");
      f = new File("/etc/");
      filelist = f.list();
      for (int i = 0; i < filelist.length; i++)
        System.out.println(filelist[i]);

      f = new File("tempfiletest/tmp/tmp");
      if (!f.mkdirs())
        throw new IOException("Failed to create directories: " + f.getPath());

      deleteTempDirs();

      f = new File("tempfiletest/tmp/tmp/");
      if (!f.mkdirs())
        throw new IOException("Failed to create directories: " + f.getPath());

      deleteTempDirs();

      //f = File.createTempFile("tempfile#old", new File("."));
      f = new File("000000");
      
      if (!f.renameTo(new File("tempfiletemp")))
        throw new IOException("Failed to rename file: " + f.getPath());
        
      if (!f.delete())
        throw new IOException("Failed to delete file: " + f.getPath());

      System.out.println("PASSED: File/Directory Manipulation Test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: File/Directory Manipulation Test: " + e);
    }

  System.out.println("Test 3: Read/Write Permissions Test");
  try
    {
      if ((new File("/")).canWrite() == true) 
        throw new IOException("Permission to write / unexpectedly");

      if ((new File("/etc/services")).canRead() == false) 
        throw new IOException("No permission to read /etc/services");

      System.out.println("PASSED: Read/Write Permissions Test");
    }
  catch (IOException e) 
    {
      System.out.println("FAILED: Read/Write Permissions Test: " + e);
    }

  System.out.println("Test 4: Name Comparison Tests");
  try
    {
      File f1, f2;

      f1 = new File("/etc/");
      f2 = new File("/etc/");
      if (!f1.equals(f2))
        throw new IOException(f1 + " " + f2);

      f2 = new File("/etc");
      if (f1.equals(f2))
        throw new IOException(f1 + " " + f2);
/*
      f1 = new File("a");
      f2 = new File("b");
      if (f1.compareTo(f2) >= 0)
        throw new IOException(f1 + " " + f2);

      f1 = new File("z");
      f2 = new File("y");
      if (f1.compareTo(f2) <= 0)
        throw new IOException(f1 + " " + f2);

      f1 = new File("../../jcl/");
      f2 = new File(".././.././jcl/.");
      if (f1.compareTo(f2) != 0)
        throw new IOException(f1 + " " + f2);
*/
      System.out.println("PASSED: Name Comparison Tests");
    }
  catch (IOException e)
    {
      System.out.println("FAILED: Name Comparison Tests: " + e);
    }

  System.out.println("Finished test of File");
}
}

