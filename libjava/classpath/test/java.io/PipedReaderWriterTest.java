/*************************************************************************
/* PipedReaderWriterTest.java -- Tests Piped{Reader,Writers}'s
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

public class PipedReaderWriterTest
{

public static void
main(String[] argv) throws InterruptedException
{
  // Set up a reasonable buffer size for this test if one is not already
  // specified
  String prop = System.getProperty("gnu.java.io.pipe_size");
//  if (prop == null)
//    System.setProperty("gnu.java.io.pipe_size", "32");

  try
    {
      System.out.println("Started test of PipedReader and PipedWriter");

      System.out.println("Test 1: Basic pipe test");

      // Set up the thread to write
      PipedTestWriter ptw = new PipedTestWriter();
      String str = ptw.getStr();
      PipedWriter pw = ptw.getWriter();

      // Now set up our reader
      PipedReader pr = new PipedReader();
      pr.connect(pw); 
      new Thread(ptw).start();

      char[] buf = new char[12];
      int chars_read, total_read = 0;
      while((chars_read = pr.read(buf)) != -1)
        {
          System.out.print(new String(buf, 0, chars_read));
          System.out.flush();
	  Thread.sleep(10); // A short delay
          total_read += chars_read;
        }

      if (total_read == str.length())
        System.out.println("PASSED: Basic pipe test");
      else
        System.out.println("FAILED: Basic pipe test");
    }
  catch (IOException e) 
    {
      System.out.println("FAILED: Basic pipe test: " + e);
    }
}

} // class PipedReaderWriterTest

class PipedTestWriter implements Runnable
{

String str;
StringReader sbr;
PipedWriter out;

public
PipedTestWriter()
{
  str = "In college, there was a tradition going for a while that people\n" +
    "would get together and hang out at Showalter Fountain - in the center\n" +
    "of Indiana University's campus - around midnight.  It was mostly folks\n" +
    "from the computer lab and just people who liked to use the Forum\n" +
    "bbs system on the VAX.  IU pulled the plug on the Forum after I left\n" +
    "despite its huge popularity.  Now they claim they are just giving\n" +
    "students what they want by cutting deals to make the campus all\n" +
    "Microsoft.\n";

  sbr = new StringReader(str);

  out = new PipedWriter();
}

public PipedWriter
getWriter()
{
  return(out);
}

public String
getStr()
{
  return(str);
}

public void
run() 
{
  char[] buf = new char[32];

  int chars_read;

  try
    {
      int b = sbr.read();
      out.write(b);

      while ((chars_read = sbr.read(buf)) != -1)
        out.write(buf, 0, chars_read);

      out.close();
    }
  catch(IOException e)
    {
      System.out.println("FAILED: Basic pipe test: " + e);
    }

}

} // PipedTestWriter

