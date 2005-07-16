/*************************************************************************
/* UTF8EncodingTest.java -- A quick test of the UTF8 encoding
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

public class UTF8EncodingTest
{

public static void main(String[] argv)
{
  System.out.println("Started test of UTF8 encoding handling");

  String str1 = "This is the first line of text\n";
  String str2 = "This has some \u01FF\uA000\u6666\u0200 weird characters\n";

  System.out.println("Test 1: Write test");
  try
    {
      FileOutputStream fos = new FileOutputStream("utf8test.out");
      OutputStreamWriter osr = new OutputStreamWriter(fos, "UTF8");
      osr.write(str1);
      osr.write(str2);
      osr.close();

      System.out.println("PASSED: Write test (conditionally)");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: Write Test: " + e);
    }

  System.out.println("Test 2: Read JDK file test");
  try
    {
      FileInputStream fis = new FileInputStream("utf8test-jdk.out");
      InputStreamReader isr = new InputStreamReader(fis, "UTF8");
      char[] buf = new char[255];

      int chars_read = isr.read(buf, 0, str1.length());
      String str3 = new String(buf, 0, chars_read);

      chars_read = isr.read(buf, 0, str2.length());
      String str4 = new String(buf, 0, chars_read);

      if (!str1.equals(str3) || !str2.equals(str4))
        System.out.println("FAILED: Read JDK file test");
      else
        System.out.println("PASSED: Read JDK file test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: Read JDK file test: " + e); 
    }

  System.out.println("Test 3: Read classpath file test");
  try
    {
      FileInputStream fis = new FileInputStream("utf8test.out");
      InputStreamReader isr = new InputStreamReader(fis, "UTF8");
      char[] buf = new char[255];

      int chars_read = isr.read(buf, 0, str1.length());
      String str3 = new String(buf, 0, chars_read);

      chars_read = isr.read(buf, 0, str2.length());
      String str4 = new String(buf, 0, chars_read);

      if (!str1.equals(str3) || !str2.equals(str4))
        System.out.println("FAILED: Read classpath file test");
      else
        System.out.println("PASSED: Read classpath file test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: Read classpath file test: " + e); 
    }

  System.out.println("Finished test of UTF8 encoding handling");
}

} // class UTF8EncodingTest

