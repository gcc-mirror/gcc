/*************************************************************************
/* ByteArrayInputStreamTest.java -- Test ByteArrayInputStream's of course
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

public class ByteArrayInputStreamTest extends ByteArrayInputStream
{

public
ByteArrayInputStreamTest(byte[] b)
{
  super(b);
}

public static void
main(String[] argv)
{
  System.out.println("Starting test of ByteArrayInputStream.");
  System.out.flush();

  String str = "My sophomore year of college I moved out of the dorms. I\n" +
     "moved in with three friends into a brand new townhouse in east\n" +
     "Bloomington at 771 Woodbridge Drive.  To this day that was the\n" +
     "nicest place I've ever lived.\n";

  byte[] str_bytes = str.getBytes();  

  System.out.println("Test 1: Protected Variables"); 

  ByteArrayInputStreamTest bais = new ByteArrayInputStreamTest(str_bytes);
  byte[] read_buf = new byte[12];

  try 
    {
      bais.read(read_buf);
      bais.mark(0);
    
      boolean passed = true;
    
      if (bais.mark != read_buf.length)
        {
          passed = false;
          System.out.println("The mark variable is wrong.  Expected " +
                             read_buf.length + " and got " + bais.mark);
        }
      bais.read(read_buf);
      if (bais.pos != (read_buf.length * 2))
        {
          passed = false;
          System.out.println("The pos variable is wrong.  Expected 24 and got " + 
                             bais.pos);
        }
      if (bais.count != str_bytes.length)
        {
          passed = false;
          System.out.println("The count variable is wrong.  Expected " +
                             str_bytes.length + " and got " + bais.pos);
        }
      if (bais.buf != str_bytes)
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

  bais = new ByteArrayInputStreamTest(str_bytes);

  try
    {
      int bytes_read, total_read = 0;
      while ((bytes_read = bais.read(read_buf, 0, read_buf.length)) != -1)
        {
          System.out.print(new String(read_buf, 0, bytes_read));
          total_read += bytes_read;
        }

      bais.close();
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
  bais = new ByteArrayInputStreamTest(str_bytes);

  try
    {
      boolean passed = true;

      bais.read(read_buf);      
      if (bais.available() != (str_bytes.length - read_buf.length))
        {
          passed = false;
          System.out.println("available() reported " + bais.available() +
                             " and " + (str_bytes.length - read_buf.length) +
                             " was expected");
        }

      if (bais.skip(5) != 5)
        {
          passed = false;
          System.out.println("skip() didn't work");
        }
      if (bais.available() != (str_bytes.length - (read_buf.length + 5)))
        {
          passed = false;
          System.out.println("skip() lied");
        }

      if (!bais.markSupported())
        {
          passed = false;
          System.out.println("markSupported() should have returned true but returned false");
        }
    
      bais.mark(0);
      int availsave = bais.available();
      bais.read();
      bais.reset();
      if (bais.available() != availsave) 
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

  System.out.println("Finished ByteArrayInputStream test");
}

}

