/*************************************************************************
/* PrintWriterTest.java -- Test of the PrintWriter class
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

public class PrintWriterTest
{

public static void main(String[] argv) throws IOException
{
  System.out.println("Started test of PrintWriter");
  System.out.println("Test 1: Printing Test");

  char[] carray = { 'h', 'i' };
  char[] carray2 = { 'b', 'y', 'e' };

  PrintWriter pw = new PrintWriter(new FileWriter("printwriter.out"));
  pw.print(true);
  pw.print('|');
  pw.print(false);
  pw.print('|');
  pw.print('A');
  pw.print('|');
  pw.flush();
  pw.print(0xFFFFF);
  pw.print('|');
  pw.print(0xFFFFFFFFFFL);
  pw.print('|');
  pw.print(3.141592);
  pw.print('|');
  pw.print((double)99999999999.9999);
  pw.print('|');
  pw.print(carray);
  pw.print('|');
  pw.print("This is a string"); 
  pw.print('|');
  pw.print(pw);
  pw.println();
  pw.println(true);
  pw.println(false);
  pw.println('A');
  pw.flush();
  pw.println(0xFFFFF);
  pw.println(0xFFFFFFFFFFL);
  pw.println(3.141592);
  pw.println((double)99999999999.9999);
  pw.println(carray);
  pw.println("This is a string"); 
  pw.println(pw);
  pw.write('B');
  pw.println();
  pw.write(carray2, 0, carray2.length);
  pw.println();
  pw.close();

  if (pw.checkError())
    System.out.println("FAILED: Printing Test");
  else
    System.out.println("PASSED: Printing Test");
 
  System.out.println("PASSED: Test of PrintWriter");
}

} // class PrintWriterTest

