/*************************************************************************
/* BufferedReaderTest.java -- Tests BufferedReader's
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

public class BufferedReaderTest extends CharArrayReader
{

// Hehe.  We override CharArrayReader.markSupported() in order to return
// false so that we can test BufferedReader's handling of mark/reset in
// both the case where the underlying stream does and does not support
// mark/reset
public boolean
markSupported()
{
  return(false);
}

public
BufferedReaderTest(char[] buf)
{
  super(buf);
}

public static int
marktest(Reader ins) throws IOException
{
  BufferedReader br = new BufferedReader(ins, 15);

  int chars_read;  
  int total_read = 0;
  char[] buf = new char[12];

  chars_read = br.read(buf);
  total_read += chars_read;
  System.out.print(new String(buf, 0, chars_read));

  chars_read = br.read(buf);
  total_read += chars_read;
  System.out.print(new String(buf, 0, chars_read));

  br.mark(75);
  br.read();
  br.read(buf);
  br.read(buf);
  br.read(buf);
  br.reset();

  chars_read = br.read(buf);
  total_read += chars_read;
  System.out.print(new String(buf, 0, chars_read));

  br.mark(555);

  chars_read = br.read(buf);
  total_read += chars_read;
  System.out.print(new String(buf, 0, chars_read));

  br.reset();

  br.read(buf);
  chars_read = br.read(buf);
  total_read += chars_read;
  System.out.print(new String(buf, 0, chars_read));

  chars_read = br.read(buf);
  total_read += chars_read;
  System.out.print(new String(buf, 0, chars_read));

  br.mark(14);

  br.read(buf);

  br.reset();

  chars_read = br.read(buf);
  total_read += chars_read;
  System.out.print(new String(buf, 0, chars_read));

  while ((chars_read = br.read(buf)) != -1)
    {
      System.out.print(new String(buf, 0, chars_read));
      total_read += chars_read;
    }

  return(total_read);
}

public static void
main(String[] argv)
{
  System.out.println("Started test of BufferedReader");

  try
    {
      System.out.println("Test 1: Simple Read Test");

      String str = "My 5th grade teacher was named Mr. Thompson.  Terry\n" +
        "George Thompson to be precise.  He had these sideburns like\n" +
        "Isaac Asimov's, only uglier.  One time he had a contest and said\n" +
        "that if any kid who could lift 50lbs worth of weights on a barbell\n" +
        "all the way over their head, he would shave it off.  Nobody could\n" +
        "though.  One time I guess I made a comment about how stupid his\n" +
        "sideburns worked and he not only kicked me out of class, he called\n" +
        "my mother.  Jerk.\n";

      StringReader sr = new StringReader(str);
      BufferedReader br = new BufferedReader(sr, 15);

      char[] buf = new char[12];
      int chars_read;
      while((chars_read = br.read(buf)) != -1)
        System.out.print(new String(buf, 0, chars_read));

      br.close();
      System.out.println("PASSED: Simple Read Test");
    }
  catch (IOException e)
    {
      System.out.println("FAILED: Simple Read Test: " + e);
    }

  try
    {
      System.out.println("Test 2: First mark/reset Test");

      String str = "Growing up in a rural area brings such delights.  One\n" +
        "time my uncle called me up and asked me to come over and help him\n" +
        "out with something.  Since he lived right across the field, I\n" +
        "walked right over.  Turned out he wanted me to come down to the\n" +
        "barn and help him castrate a calf.  Oh, that was fun.  Not.\n";

      StringReader sr = new StringReader(str);
//      BufferedReader br = new BufferedReader(sr);

      int total_read = marktest(sr);

      if (total_read == str.length())
        System.out.println("PASSED: First mark/reset Test");
      else
        System.out.println("FAILED: First mark/reset Test");
    }
  catch (IOException e)
    {
      System.out.println("FAILED: First mark/reset Test: " + e);
    }
   
  try
    {
      System.out.println("Test 3: Second mark/reset Test");

      String str = "Growing up we heated our house with a wood stove.  That\n" +
        "thing could pump out some BTU's, let me tell you.  No matter how\n" +
        "cold it got outside, it was always warm inside.  Of course the\n" +
        "downside is that somebody had to chop the wood for the stove. That\n" +
        "somebody was me.  I was slave labor.  My uncle would go back and\n" +
        "chain saw up dead trees and I would load the wood in wagons and\n" +
        "split it with a maul. Somehow my no account brother always seemed\n" +
        "to get out of having to work.\n";

      char[] buf = new char[str.length()];
      str.getChars(0, str.length(), buf, 0);
      BufferedReaderTest brt = new BufferedReaderTest(buf);
//      BufferedReader br = new BufferedReader(car);

      int total_read = marktest(brt);

      if (total_read == str.length())
        System.out.println("PASSED: Second mark/reset Test");
      else
        System.out.println("FAILED: Second mark/reset Test");
    }
  catch (IOException e)
    {
      System.out.println("FAILED: Second mark/reset Test: " + e);
    }

  System.out.println("Finished test of BufferedReader");
} // main

} // class BufferedReaderTest

