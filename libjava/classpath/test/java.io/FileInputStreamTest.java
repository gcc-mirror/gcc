/*************************************************************************
/* FileInputStreamTest.java -- Test of FileInputStream class
/*
/* Copyright (c) 1998 Free Software Foundation, Inc.
/* Written by Aaron M. Renn (arenn@urbanophile.com)
/*
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published 
/* by the Free Software Foundation, either version 2 of the License, or
/* (at your option) any later version.
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

public class FileInputStreamTest
{

public static void
main(String[] argv)
{
  System.out.println("Starting test of FileInputStream");

  System.out.println("Test 1: File Read Test");
  try
    {
      FileInputStream fis = new FileInputStream("/etc/services");

      System.out.println("Available: " + fis.available());
      System.out.println("FileDescriptor: " + fis.getFD());
      System.out.println("Dumping file. Note that first 100 bytes will be skipped");
      fis.skip(100);

      byte[] buf = new byte[32];
      int bytes_read = 0;

      while((bytes_read = fis.read(buf)) != -1)
        System.out.print(new String(buf, 0, bytes_read));

      fis.close();
      System.out.println("PASSED: File read test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: File read test: " + e);
    }

  System.out.println("Test 2: File Not Found Test");
  try
    {
      FileInputStream fis = new FileInputStream("/etc/yourmommasawhore");
      System.out.println("FAILED: File Not Found Test");
    }
  catch (FileNotFoundException e)
    {
      System.out.println("PASSED: File Not Found Test");
    }

  System.out.println("Finished test of FileInputStream");
}

} // class FileInputStreamTest

