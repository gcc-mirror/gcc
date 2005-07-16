/*************************************************************************
/* CharArrayReaderTest.java -- Test CharArrayReaders's of course
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

public class CharArrayReaderTest extends CharArrayReader
{

public
CharArrayReaderTest(char[] b)
{
  super(b);
}

public static void
main(String[] argv)
{
  System.out.println("Starting test of CharArrayReader.");
  System.out.flush();

  String str = "In junior high, I did a lot writing.  I wrote a science\n" +
     "fiction novel length story that was called 'The Destruction of\n" +
     "Planet Earth'.  All the characters in the story were my friends \n" +
     "from school because I couldn't think up any cool names.";

  char[] str_chars = new char[str.length()];
  str.getChars(0, str.length(), str_chars, 0);

  System.out.println("Test 1: Protected Variables"); 

  CharArrayReaderTest car = new CharArrayReaderTest(str_chars);
  char[] read_buf = new char[12];

  try 
    {
      car.read(read_buf);
      car.mark(0);
    
      boolean passed = true;
    
      if (car.markedPos != read_buf.length)
        {
          passed = false;
          System.out.println("The mark variable is wrong.  Expected " +
                             read_buf.length + " and got " + car.markedPos);
        }
      car.read(read_buf);
      if (car.pos != (read_buf.length * 2))
        {
          passed = false;
          System.out.println("The pos variable is wrong.  Expected 24 and got " + 
                             car.pos);
        }
      if (car.count != str_chars.length)
        {
          passed = false;
          System.out.println("The count variable is wrong.  Expected " +
                             str_chars.length + " and got " + car.pos);
        }
      if (car.buf != str_chars)
        {
          passed = false;
          System.out.println("The buf variable is not correct");
        }
    
      if (passed)
        System.out.println("PASSED: Protected Variables Test");
      else
        System.out.println("FAILED: Protected Variables Test");
    }
  catch (IOException e)
    {
      System.out.println("FAILED: Protected Variables Test: " + e);
    }

  System.out.println("Test 2: Simple Read Test");

  car = new CharArrayReaderTest(str_chars);

  try
    {
      int chars_read, total_read = 0;
      while ((chars_read = car.read(read_buf, 0, read_buf.length)) != -1)
        {
          System.out.print(new String(read_buf, 0, chars_read));
          total_read += chars_read;
        }

      car.close();
      if (total_read == str.length())
        System.out.println("PASSED: Simple Read Test");
      else
        System.out.println("FAILED: Simple Read Test");
    }
  catch (IOException e)
    {
      System.out.println("FAILED: Simple Read Test: " + e);
    }

  System.out.println("Test 3: mark/reset/available/skip test");
  car = new CharArrayReaderTest(str_chars);

  try
    {
      boolean passed = true;

      car.read(read_buf);      
      if (!car.ready())
        {
          passed = false;
          System.out.println("ready() reported false and should have " +
                             "reported true.");
        }

      if (car.skip(5) != 5)
        {
          passed = false;
          System.out.println("skip() didn't work");
        }

      if (!car.markSupported())
        {
          passed = false;
          System.out.println("markSupported() should have returned true but returned false");
        }
    
      car.mark(0);
      int pos_save = car.pos;
      car.read();
      car.reset();
      if (car.pos != pos_save) 
        {
          passed = false;
          System.out.println("mark/reset failed to work");
        }

      if (passed)
        System.out.println("PASSED: mark/reset/available/skip test");
      else
        System.out.println("FAILED: mark/reset/available/skip test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: mark/reset/available/skip test: " + e);
    }

  System.out.println("Finished CharArrayReader test");
}

}

