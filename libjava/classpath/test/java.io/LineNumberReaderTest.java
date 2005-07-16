/*************************************************************************
/* LineNumberReaderTest.java -- Tests LineNumberReader's
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

public class LineNumberReaderTest
{

public static void
main(String[] argv)
{
  System.out.println("Started test of LineNumberReader");

  try
    {
      System.out.println("Test 1: First test series");

      boolean passed = true;

      String str = "In 6th grade I had a crush on this girl named Leanne\n" +
        "Dean.  I thought she was pretty hot.  I saw her at my ten year\n" +
        "high school reunion.  I still think she's pretty hot.  (She's\n" +
        "married to my brother's college roommate).\n";

      StringReader sbr = new StringReader(str);
      LineNumberReader lnr = new LineNumberReader(sbr);

      lnr.setLineNumber(2);

      char[] buf = new char[32];
      int chars_read; 
      while ((chars_read = lnr.read(buf)) != -1)
        {
          str = new String(buf, 0, chars_read);
          if (str.indexOf("\r") != -1)
            {
              passed = false;
              System.out.println("\nFound an unexpected \\r\n");
            }
            
          System.out.print(str);
        }

      if (lnr.getLineNumber() != 6)
        {
          passed = false;
          System.out.println("Line number was wrong.  Expected 6 but got " +
            lnr.getLineNumber());
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

      String str = "Exiting off the expressway in Chicago is not an easy\n" +
        "thing to do.  For example, at Fullerton you have to run a\n" +
        "gauntlet of people selling flowers, begging for money, or trying\n" +
        "to 'clean' your windshield for tips.";

      StringReader sbr = new StringReader(str);
      LineNumberReader lnr = new LineNumberReader(sbr);

      char[] buf = new char[32];
      int chars_read; 
      while ((chars_read = lnr.read(buf)) != -1)
        System.out.print(new String(buf, 0, chars_read));
      System.out.println("");

      if (lnr.getLineNumber() != 3)
        {
          passed = false;
          System.out.println("\nLine number was wrong.  Expected 3 but got " +
            lnr.getLineNumber());
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

  System.out.println("Finished test of LineNumberReader");
}

} // class LineNumberReaderTest

