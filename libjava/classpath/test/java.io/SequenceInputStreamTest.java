/*************************************************************************
/* SequenceInputStreamTest.java -- Tests SequenceInputStream's
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

public class SequenceInputStreamTest
{

public static void
main(String argv[])
{
  System.out.println("Started test of SequenceInputStream");

  String str1 = "I don't believe in going to chain restaurants.  I think\n" +
    "they are evil.  I can't believe all the suburban folks who go to \n";

  String str2 = "places like the Olive Garden.  Not only does the food make\n" +
    "me want to puke, non of these chains has the slightest bit of character.\n";

  byte[] buf = new byte[10];

  System.out.println("Test 1: Simple read test");
  try
    {
      StringBufferInputStream is1 = new StringBufferInputStream(str1);
      ByteArrayInputStream is2 = new ByteArrayInputStream(str2.getBytes());
      SequenceInputStream sis = new SequenceInputStream(is1, is2);

      int bytes_read;
      while((bytes_read = sis.read(buf)) != -1)
        {
          System.out.print(new String(buf,0,bytes_read));
        }

      sis.close();
      System.out.println("PASSED: Simple read test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: Simple read test: " + e);
    }

  System.out.println("Test 2: close() test");

  try
    { 
      StringBufferInputStream is1 = new StringBufferInputStream(str1);
      ByteArrayInputStream is2 = new ByteArrayInputStream(str2.getBytes());
      SequenceInputStream sis = new SequenceInputStream(is1, is2);

      sis.read(buf);
      sis.close();
      if (sis.read() != -1)
         System.out.println("FAILED: close() test");
      else
         System.out.println("PASSED: close() test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: close() test: " + e);
    }

  System.out.println("Finished test of SequenceInputStream");
}

} // class SequenceInputStream

