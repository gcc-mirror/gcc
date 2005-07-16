/*************************************************************************
/* StringWriterTest.java -- Test StringWriter
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
  * Class to test StringWriter. This is just a rehash of the
  * BufferedCharWriterTest using a StringWriter instead of a 
  * CharArrayWriter underneath.
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class StringWriterTest
{

public static void 
main(String argv[])
{
  System.out.println("Started test of StringWriter");

  try
    {
      System.out.println("Test 1: Write Tests");

      StringWriter sw = new StringWriter(24);
      BufferedWriter bw = new BufferedWriter(sw, 12);

      String str = "There are a ton of great places to see live, original\n" +
        "music in Chicago.  Places like Lounge Ax, Schuba's, the Empty\n" +
        "Bottle, and even the dreaded Metro with their sometimes asshole\n" +
        "bouncers.\n";

      boolean passed = true;

      char[] buf = new char[str.length()];
      str.getChars(0, str.length(), buf, 0);

      bw.write(buf, 0, 5);
      if (sw.toString().length() != 0)
        {
          passed = false;
          System.out.println("StringWriter has too many bytes #1");
        }
      bw.write(buf, 5, 8);
      bw.write(buf, 13, 12);
      bw.write(buf[25]);
      bw.write(buf, 26, buf.length - 26);
      bw.close();

      String str2 = sw.toString();
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

