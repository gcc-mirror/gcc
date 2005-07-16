/*************************************************************************
/* LineNumberInputStreamTest.java -- Tests LineNumberInputStream's
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

public class LineNumberInputStreamTest
{

public static void
main(String[] argv)
{
  System.out.println("Started test of LineNumberInputStream");

  try
    {
      System.out.println("Test 1: First test series");

      boolean passed = true;

      String str = "I grew up by a small town called Laconia, Indiana\r" +
         "which has a population of about 64 people.  But I didn't live\r\n" +
         "in town.  I lived on a gravel road about 4 miles away\n" +
         "They paved that road\n";

      StringBufferInputStream sbis = new StringBufferInputStream(str);
      LineNumberInputStream lnis = new LineNumberInputStream(sbis);

      lnis.setLineNumber(2);

      byte[] buf = new byte[32];
      int bytes_read; 
      while ((bytes_read = lnis.read(buf)) != -1)
        {
          str = new String(buf, 0, bytes_read);
          if (str.indexOf("\r") != -1)
            {
              passed = false;
              System.out.println("\nFound an unexpected \\r\n");
            }
            
          System.out.print(str);
        }

      if (lnis.getLineNumber() != 6)
        {
          passed = false;
          System.out.println("Line number was wrong.  Expected 6 but got " +
            lnis.getLineNumber());
        }

      if (passed)
        System.out.println("PASSED: First test series");
      else
        System.out.println("FAILED: First test series");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: First test series: " + e);
    }

  try
    {
      System.out.println("Test 2: Second test series");

      boolean passed = true;

      String str = "One time I was playing kickball on the playground\n" +
         "in 4th grade and my friends kept talking about how they smelled\n" +
         "pot.  I kept asking them what they smelled because I couldn't\n" +
         "figure out how a pot could have a smell";

      StringBufferInputStream sbis = new StringBufferInputStream(str);
      LineNumberInputStream lnis = new LineNumberInputStream(sbis);

      byte[] buf = new byte[32];
      int bytes_read; 
      while ((bytes_read = lnis.read(buf)) != -1)
        System.out.print(new String(buf, 0, bytes_read));

      if (lnis.getLineNumber() != 3)
        {
          passed = false;
          System.out.println("\nLine number was wrong.  Expected 3 but got " +
            lnis.getLineNumber());
        }

      if (passed)
        System.out.println("PASSED: Second test series");
      else
        System.out.println("FAILED: Second test series");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: Second test series: " + e);
    }

  System.out.println("Finished test of LineNumberInputStream");
}

} // class LineNumberInputStreamTest

