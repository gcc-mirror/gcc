/*************************************************************************
/* FileWriterTest.java -- Test of FileWriter and OutputStreamWriter classes
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

public class FileWriterTest
{

public static void
main(String[] argv)
{
  System.out.println("Starting test of FileWriter and OutputStreamWriter");

  System.out.println("Test 1: File Write Test");
  try
    {
      String s1 = "Ok, what are some of the great flame wars that we have " +
         "had lately.  Let us see, there was emacs vs. xemacs, " +
         "KDE vs. Gnome, and Tcl vs. Guile";

      String s2 = "Operating systems I have known include: solaris, sco, " +
         "hp-ux, linux, freebsd, winblows, os400, mvs, tpf, its, multics";

      //File f = File.createTempFile("fostest", new File("/tmp"));
      File f = new File("/tmp/000001");
      FileWriter fw = new FileWriter(f.getPath());

      char buf[] = new char[s1.length()];
      s1.getChars(0, s1.length(), buf, 0);
      fw.write(buf, 0, 32);
      fw.write(buf, 32, s1.getBytes().length - 32);
      fw.close();

      fw = new FileWriter(f.getPath(), true);
      buf = new char[s2.length()];
      s2.getChars(0, s2.length(), buf, 0);
      fw.write(buf);
      fw.close();

      if (f.length() != (s1.length() + s2.length()))
        throw new IOException("Incorrect number of chars written");

      f.delete();

      System.out.println("PASSED: File Write Test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: File Write Test: " + e);
    }

  System.out.println("Test 2: Permission Denied Test");
  try
    {
      FileWriter fw = new FileWriter("/etc/newtempfile");
      System.out.println("FAILED: Permission Denied Test");
    }
  catch (IOException e)
    {
      System.out.println("PASSED: Permission Denied Test: " + e);
    }

  System.out.println("Finished test of FileWriter");
}

} // class FileWriter

