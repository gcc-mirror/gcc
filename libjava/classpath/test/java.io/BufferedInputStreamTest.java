/*************************************************************************
/* BufferedInputStreamTest.java -- Tests BufferedInputStream's
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

public class BufferedInputStreamTest extends BufferedInputStream
{

public
BufferedInputStreamTest(InputStream in, int size)
{
  super(in, size);
}

public static int
marktest(InputStream ins) throws IOException
{
  BufferedInputStream bis = new BufferedInputStream(ins, 15);

  int bytes_read;  
  int total_read = 0;
  byte[] buf = new byte[12];

  bytes_read = bis.read(buf);
  total_read += bytes_read;
  System.out.print(new String(buf, 0, bytes_read));

  bytes_read = bis.read(buf);
  total_read += bytes_read;
  System.out.print(new String(buf, 0, bytes_read));

  bis.mark(75);
  bis.read();
  bis.read(buf);
  bis.read(buf);
  bis.read(buf);
  bis.reset();

  bytes_read = bis.read(buf);
  total_read += bytes_read;
  System.out.print(new String(buf, 0, bytes_read));

  bis.mark(555);

  bytes_read = bis.read(buf);
  total_read += bytes_read;
  System.out.print(new String(buf, 0, bytes_read));

  bis.reset();

  bis.read(buf);
  bytes_read = bis.read(buf);
  total_read += bytes_read;
  System.out.print(new String(buf, 0, bytes_read));

  bytes_read = bis.read(buf);
  total_read += bytes_read;
  System.out.print(new String(buf, 0, bytes_read));

  bis.mark(14);

  bis.read(buf);

  bis.reset();

  bytes_read = bis.read(buf);
  total_read += bytes_read;
  System.out.print(new String(buf, 0, bytes_read));

  while ((bytes_read = bis.read(buf)) != -1)
    {
      System.out.print(new String(buf, 0, bytes_read));
      total_read += bytes_read;
    }

  return(total_read);
}

public static void
main(String[] argv)
{
  System.out.println("Started test of BufferedInputStream");

  try
    {
      System.out.println("Test 1: Protected Variables Test");

      String str = "This is a test line of text for this pass";

      StringBufferInputStream sbis = new StringBufferInputStream(str);
      BufferedInputStreamTest bist = new BufferedInputStreamTest(sbis, 12); 

      bist.read();
      bist.mark(5);

      boolean passed = true;

      if (bist.buf.length != 12)
        {
          passed = false;
          System.out.println("buf.length is wrong.  Expected 12, but got " + 
                             bist.buf.length);
        }
      if (bist.count != 12)
        {
          passed = false;
          System.out.println("count is wrong.  Expected 12, but got " + 
                             bist.count);
        }
      if (bist.marklimit != 5)
        {
          passed = false;
          System.out.println("marklimit is wrong.  Expected 5, but got " + 
                             bist.marklimit);
        }
      if (bist.markpos != 1)
        {
          passed = false;
          System.out.println("markpos is wrong.  Expected 5, but got " + 
                             bist.markpos);
        }
      if (bist.pos != 1)
        {
          passed = false;
          System.out.println("pos is wrong.  Expected 1, but got " + 
                             bist.pos);
        }

      if (passed)
        System.out.println("PASSED: Protected Variables Test");
      else
        System.out.println("FAILED: Protected Variables Test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: Protected Variables Test: " + e);
    }

  try
    {
      System.out.println("Test 2: Simple Read Test");

      String str = "One of my 8th grade teachers was Mr. Russell.\n" +
         "He used to start each year off by telling the class that the\n" +
         "earth was flat.  He did it to teach people to question\n" +
         "things they are told.  But everybody knew that he did it\n" +
         "so it lost its effect.\n";

      StringBufferInputStream sbis = new StringBufferInputStream(str);
      BufferedInputStream bis = new BufferedInputStream(sbis, 15);

      byte[] buf = new byte[12];
      int bytes_read;
      while((bytes_read = bis.read(buf)) != -1)
        System.out.print(new String(buf, 0, bytes_read));

      bis.close();
      System.out.println("PASSED: Simple Read Test");
    }
  catch (IOException e)
    {
      System.out.println("FAILED: Simple Read Test: " + e);
    }

  try
    {
      System.out.println("Test 3: First mark/reset Test");

      String str = "My 6th grade teacher was named Mrs. Hostetler.\n" +
        "She had a whole list of rules that you were supposed to follow\n" +
        "in class and if you broke a rule she would make you write the\n" +
        "rules out several times.  The number varied depending on what\n" +
        "rule you broke.  Since I knew I would get in trouble, I would\n" +
        "just go ahead and write out a few sets on the long school bus\n" +
        "ride home so that if had to stay in during recess and write\n" +
        "rules, five minutes later I could just tell the teacher I was\n" +
        "done so I could go outside and play kickball.\n";

      StringBufferInputStream sbis = new StringBufferInputStream(str);

      int total_read = marktest(sbis);

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
      System.out.println("Test 4: Second mark/reset Test");

      String str = "My first day of college was fun.  A bunch of us\n" +
         "got pretty drunk, then this guy named Rick Flake (I'm not\n" +
         "making that name up) took a piss in the bed of a Physical\n" +
         "Plant dept pickup truck.  Later on we were walking across\n" +
         "campus, saw a cop, and took off running for no reason.\n" +
         "When we got back to the dorm we found an even drunker guy\n" +
         "passed out in a shopping cart outside.\n";

      ByteArrayInputStream sbis = new ByteArrayInputStream(str.getBytes());

      int total_read = marktest(sbis);

      if (total_read == str.length())
        System.out.println("PASSED: Second mark/reset Test");
      else
        System.out.println("FAILED: Second mark/reset Test");
    }
  catch (IOException e)
    {
      System.out.println("FAILED: Second mark/reset Test: " + e);
    }

  System.out.println("Finished test of BufferedInputStream");
} // main

} // class BufferedInputStreamTest

