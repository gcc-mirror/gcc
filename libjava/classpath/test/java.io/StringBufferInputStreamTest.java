/*************************************************************************
/* StringBufferInputStreamTest.java -- Test StringBufferInputStream's of course
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

public class StringBufferInputStreamTest extends StringBufferInputStream
{

public
StringBufferInputStreamTest(String b)
{
  super(b);
}

public static void
main(String[] argv)
{
  System.out.println("Starting test of StringBufferInputStream.");
  System.out.flush();

  String str = "Between my freshman and sophomore years of high school\n" +
    "we moved into a brand new building.  The old high school was turned\n" +
    "into an elementary school.\n";

  System.out.println("Test 1: Protected Variables"); 

  StringBufferInputStreamTest sbis = new StringBufferInputStreamTest(str);
  byte[] read_buf = new byte[12];

  try 
    {
      sbis.read(read_buf);
      sbis.mark(0);
    
      boolean passed = true;
    
      sbis.read(read_buf);
      if (sbis.pos != (read_buf.length * 2))
        {
          passed = false;
          System.out.println("The pos variable is wrong.  Expected 24 and got " + 
                             sbis.pos);
        }
      if (sbis.count != str.length())
        {
          passed = false;
          System.out.println("The count variable is wrong.  Expected " +
                             str.length() + " and got " + sbis.pos);
        }
      if (sbis.buffer != str)
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

  sbis = new StringBufferInputStreamTest(str);

  try
    {
      int bytes_read, total_read = 0;
      while ((bytes_read = sbis.read(read_buf, 0, read_buf.length)) != -1)
        {
          System.out.print(new String(read_buf, 0, bytes_read));
          total_read += bytes_read;
        }

      sbis.close();
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
  sbis = new StringBufferInputStreamTest(str);

  try
    {
      boolean passed = true;

      sbis.read(read_buf);      
      if (sbis.available() != (str.length() - read_buf.length))
        {
          passed = false;
          System.out.println("available() reported " + sbis.available() +
                             " and " + (str.length() - read_buf.length) +
                             " was expected");
        }

      if (sbis.skip(5) != 5)
        {
          passed = false;
          System.out.println("skip() didn't work");
        }
      if (sbis.available() != (str.length() - (read_buf.length + 5)))
        {
          passed = false;
          System.out.println("skip() lied");
        }

      if (sbis.markSupported())
        {
          passed = false;
          System.out.println("markSupported() should have returned false but returned true");
        }
    
      int availsave = sbis.available();
      sbis.reset();
      if (sbis.pos != 0) 
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

  System.out.println("Finished StringBufferInputStream test");
}

}

