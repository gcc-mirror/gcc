/*************************************************************************
/* BufferedCharWriterTest.java -- Test {Buffered,CharArray}Writer
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
  * Class to test BufferedWriter and CharArrayWriter
  *
  * @version 0.0
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class BufferedCharWriterTest
{

public static void 
main(String argv[])
{
  System.out.println("Started test of BufferedWriter and CharArrayWriter");

  try
    {
      System.out.println("Test 1: Write Tests");

      CharArrayWriter caw = new CharArrayWriter(24);
      BufferedWriter bw = new BufferedWriter(caw, 12);

      String str = "I used to live right behind this super-cool bar in\n" +
        "Chicago called Lounge Ax.  They have the best music of pretty\n" +
        "much anyplace in town with a great atmosphere and $1 Huber\n" +
        "on tap.  I go to tons of shows there, even though I moved.\n";

      boolean passed = true;

      char[] buf = new char[str.length()];
      str.getChars(0, str.length(), buf, 0);

      bw.write(buf, 0, 5);
      if (caw.toCharArray().length != 0)
        {
          passed = false;
          System.out.println("CharArrayWriter has too many bytes #1");
        }
      bw.write(buf, 5, 8);
      bw.write(buf, 13, 12);
      bw.write(buf[25]);
      bw.write(buf, 26, buf.length - 26);
      bw.close();

      String str2 = new String(caw.toCharArray());
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

