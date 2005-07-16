/*************************************************************************
/* BufferedByteOutputStreamTest.java -- Test {Buffered,ByteArray}OutputStream
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

/**
  * Class to test BufferedOutputStream and ByteOutputStream
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class BufferedByteOutputStreamTest
{

public static void 
main(String argv[])
{
  System.out.println("Started test of BufferedOutputStream and ByteArrayOutputStream");

  try
    {
      System.out.println("Test 1: Write Tests");

      ByteArrayOutputStream baos = new ByteArrayOutputStream(24);
      BufferedOutputStream bos = new BufferedOutputStream(baos, 12);

      String str = "The Kroger on College Mall Rd. in Bloomington " +
        "used to sell Kroger brand froze pizzas for 68 cents. " +
        "I ate a lot of those in college.  It was kind of embarrassing " +
        "walking out of the grocery with nothing but 15 frozen pizzas.\n";

      boolean passed = true;

      byte[] buf = str.getBytes();
      bos.write(buf, 0, 5);
      if (baos.toByteArray().length != 0)
        {
          passed = false;
          System.out.println("ByteArrayOutputStream has too many bytes #1");
        }
      bos.write(buf, 5, 8);
      bos.write(buf, 13, 12);
      bos.write(buf[25]);
      bos.write(buf, 26, buf.length - 26);
      bos.close();

      String str2 = new String(baos.toByteArray());
      if (!str.equals(str2))
        {
          passed = false;
          System.out.println("Unexpected string: " + str2);
        }

      if (passed)
        System.out.println("PASSED: Write Tests");
      else
        System.out.println("FAILED: Write Tests");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: Write Tests: " + e);
    }

  System.out.println("Finished test of BufferedOutputStream and ByteArrayOutputStream");
}

} // class BufferedByteOutputStreamTest

