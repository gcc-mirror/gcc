/*************************************************************************
/* PipedStreamTest.java -- Tests Piped{Input,Output}Stream's
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

public class PipedStreamTest
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
      System.out.println("Started test of PipedInputStream and " +
                         "PipedOutputStream");

      System.out.println("Test 1: Basic piped stream test");

      // Set up the thread to write
      PipedStreamTestWriter pstw = new PipedStreamTestWriter();
      String str = pstw.getStr();
      PipedOutputStream pos = pstw.getStream();

      // Now set up our reader
      PipedInputStream pis = new PipedInputStream();
      pis.connect(pos); 
      new Thread(pstw).start();

      byte[] buf = new byte[12];
      int bytes_read, total_read = 0;
      while((bytes_read = pis.read(buf)) != -1)
        {
          System.out.print(new String(buf, 0, bytes_read));
          System.out.flush();
          Thread.sleep(10); // A short delay
          total_read += bytes_read;
        }

      if (total_read == str.length())
        System.out.println("PASSED: Basic piped stream test");
      else
        System.out.println("FAILED: Basic piped stream test");
    }
  catch (IOException e) 
    {
      System.out.println("FAILED: Basic piped stream test: " + e);
    }
}

} // class PipedStreamTest

class PipedStreamTestWriter implements Runnable
{

String str;
StringBufferInputStream sbis;
PipedOutputStream out;

public
PipedStreamTestWriter()
{
  str = "I went to work for Andersen Consulting after I graduated\n" +
     "from college.  They sent me to their training facility in St. Charles,\n" +
     "Illinois and tried to teach me COBOL.  I didn't want to learn it.\n" +
     "The instructors said I had a bad attitude and I got a green sheet\n" +
     "which is a nasty note in your file saying what a jerk you are.\n";

  sbis = new StringBufferInputStream(str);

  out = new PipedOutputStream();
}

public PipedOutputStream
getStream()
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
  byte[] buf = new byte[32];

  int bytes_read;

  try
    {
      int b = sbis.read();
      out.write(b);

      while ((bytes_read = sbis.read(buf)) != -1)
        out.write(buf, 0, bytes_read);

      out.close();
    }
  catch(IOException e)
    {
      System.out.println("FAILED: Basic piped stream test: " + e);
    }

}

}

