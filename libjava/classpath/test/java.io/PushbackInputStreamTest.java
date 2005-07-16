/*************************************************************************
/* PushbackInputStreamTest.java -- Tests PushbackInputStream's of course
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

public class PushbackInputStreamTest extends PushbackInputStream
{

public
PushbackInputStreamTest(InputStream is, int size)
{
  super(is, size);
}

public static void
main(String[] argv)
{
  System.out.println("Started test of PushbackInputStream");

  String str = "Once when I was in fourth grade, my friend Lloyd\n" +
    "Saltsgaver and I got in trouble for kicking a bunch of\n" +
    "Kindergartners off the horse swings so we could play a game\n" +
    "of 'road hog'\n";

  System.out.println("Test 1: Protected Variables Test");
  {
  PushbackInputStreamTest pist = new PushbackInputStreamTest(
    new StringBufferInputStream(str), 15);

  boolean passed = true;
  if (pist.pos != pist.buf.length)
    {
       passed = false;
       System.out.println("The pos variable is wrong.  Expected " +
             pist.buf.length + " but got " + pist.pos);
    }
  if (pist.buf.length != 15)
    {
       passed = false;
       System.out.println("The buf.length is wrong.  Expected 15" +
             " but got " + pist.buf.length);
    }

   if (passed)
     System.out.println("PASSED: Protected Variables Test");
   else
     System.out.println("FAILED: Protected Variables Test");
  }
  
  System.out.println("Test 2: Basic Unread Tests");
  try
    {
      PushbackInputStreamTest pist = new PushbackInputStreamTest(
        new StringBufferInputStream(str), 15);

      byte[] read_buf1 = new byte[12]; 
      byte[] read_buf2 = new byte[12]; 
      
      boolean passed = true;

      pist.read(read_buf1); 
      pist.unread(read_buf1);
      pist.read(read_buf2);
      
      for (int i = 0; i < read_buf1.length; i++)
        {
          if (read_buf1[i] != read_buf2[i])
            passed = false;
        }

      pist.unread(read_buf2, 1, read_buf2.length - 1);      
      pist.unread(read_buf2[0]);

      int bytes_read, total_read = 0;
      while ((bytes_read = pist.read(read_buf1)) != -1)
        {
          System.out.print(new String(read_buf1, 0, bytes_read));
          total_read += bytes_read;
        }

      if (total_read != str.length())
        passed = false;

      if (passed)
        System.out.println("PASSED: Basic Unread Tests");
      else
        System.out.println("FAILED: Basic Unread Tests");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: Basic Unread Tests: " + e);
    }

  System.out.println("Test 3: Buffer Overflow Test");
  try
    {
      PushbackInputStreamTest pist = new PushbackInputStreamTest(
        new StringBufferInputStream(str), 10);

      byte[] read_buf = new byte[12]; 

      pist.read(read_buf);
      pist.unread(read_buf);
      System.out.println("FAILED: Buffer Overflow Test");
    }
  catch(IOException e)
    {
      System.out.println("PASSED: Buffer Overflow Test: " + e);
    }
  
  System.out.println("Finished tests of PushbackInputStream");
} // main

} // class PushbackInputStreamTest

