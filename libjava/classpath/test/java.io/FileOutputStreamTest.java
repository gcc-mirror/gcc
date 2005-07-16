/*************************************************************************
/* FileOutputStreamTest.java -- Test of FileOutputStream class
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

public class FileOutputStreamTest
{

public static void
main(String[] argv)
{
  System.out.println("Starting test of FileOutputStream");

  System.out.println("Test 1: File Write Test");
  try
    {
      String s1 = "Ok, what are some of the great flame wars that we have " +
         "had lately.  Let us see, there was emacs vs. xemacs, " +
         "KDE vs. Gnome, and Tcl vs. Guile";

      String s2 = "Operating systems I have known include: solaris, sco, " +
         "hp-ux, linux, freebsd, winblows, os400, mvs, tpf, its, multics";

      //File f = File.createTempFile("fostest", new File("/tmp"));
      File f = new File("/tmp/000000");
      FileOutputStream fos = new FileOutputStream(f.getPath());

      fos.write(s1.getBytes(), 0, 32);
      fos.write(s1.getBytes(), 32, s1.getBytes().length - 32);
      fos.close();

      fos = new FileOutputStream(f.getPath(), true);
      fos.write(s2.getBytes());
      fos.close();

      if (f.length() != (s1.length() + s2.length()))
        throw new IOException("Incorrect number of bytes written");

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
      FileOutputStream fos = new FileOutputStream("/etc/newtempfile");
      System.out.println("FAILED: Permission Denied Test");
    }
  catch (IOException e)
    {
      System.out.println("PASSED: Permission Denied Test: " + e);
    }

  System.out.println("Finished test of FileOutputStream");
}

} // class FileOutputStreamTest

