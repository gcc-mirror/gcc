/*************************************************************************
/* PushbackReaderTest.java -- Tests PushbackReader's of course
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

public class PushbackReaderTest extends PushbackReader
{

public
PushbackReaderTest(Reader r, int size)
{
  super(r, size);
}

public static void
main(String[] argv)
{
  System.out.println("Started test of PushbackReader");

  String str = "I used to idolize my older cousin Kurt.  I wanted to be\n" +
    "just like him when I was a kid.  (Now we are as different as night\n" +
    "and day - but still like each other).  One thing he did for a while\n" +
    "was set traps for foxes thinking he would make money off sellnig furs.\n" +
    "Now I never saw a fox in all my years of Southern Indiana.  That\n" +
    "didn't deter us.  One time we went out in the middle of winter to\n" +
    "check our traps.  It was freezing and I stepped onto a frozen over\n" +
    "stream. The ice broke and I got my foot soak.  Despite the fact that\n" +
    "it made me look like a girl, I turned around and went straight home.\n" +
    "Good thing too since I couldn't even feel my foot by the time I got\n" +
    "there.\n";

  System.out.println("Test 1: Basic Unread Tests");
  try
    {
      PushbackReaderTest prt = new PushbackReaderTest(
        new StringReader(str), 15);

      char[] read_buf1 = new char[12]; 
      char[] read_buf2 = new char[12]; 
      
      boolean passed = true;

      prt.read(read_buf1); 
      prt.unread(read_buf1);
      prt.read(read_buf2);
      
      for (int i = 0; i < read_buf1.length; i++)
        {
          if (read_buf1[i] != read_buf2[i])
            passed = false;
        }

      prt.unread(read_buf2, 1, read_buf2.length - 1);      
      prt.unread(read_buf2[0]);

      int chars_read, total_read = 0;
      while ((chars_read = prt.read(read_buf1)) != -1)
        {
          System.out.print(new String(read_buf1, 0, chars_read));
          total_read += chars_read;
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
      PushbackReaderTest prt = new PushbackReaderTest(
        new StringReader(str), 10);

      char[] read_buf = new char[12]; 

      prt.read(read_buf);
      prt.unread(read_buf);
      System.out.println("FAILED: Buffer Overflow Test");
    }
  catch(IOException e)
    {
      System.out.println("PASSED: Buffer Overflow Test: " + e);
    }
  
  System.out.println("Finished tests of PushbackReader");
} // main

} // class PushbackReaderTest

