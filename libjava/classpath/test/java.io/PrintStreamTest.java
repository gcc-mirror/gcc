/*************************************************************************
/* PrintStreamTest.java -- Test of the PrintStream class
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

public class PrintStreamTest
{

public static void main(String[] argv) throws IOException
{
  System.out.println("Started test of PrintStream");
  System.out.println("Test 1: Printing Test");

  char[] carray = { 'h', 'i' };
  byte[] barray = { 'b', 'y', 'e' };

  PrintStream ps = new PrintStream(new FileOutputStream("printstream.out"));
  ps.print(true);
  ps.print('|');
  ps.print(false);
  ps.print('|');
  ps.print('A');
  ps.print('|');
  ps.flush();
  ps.print(0xFFFFF);
  ps.print('|');
  ps.print(0xFFFFFFFFFFL);
  ps.print('|');
  ps.print(3.141592);
  ps.print('|');
  ps.print((double)99999999999.9999);
  ps.print('|');
  ps.print(carray);
  ps.print('|');
  ps.print("This is a string"); 
  ps.print('|');
  ps.print(ps);
  ps.println();
  ps.println(true);
  ps.println(false);
  ps.println('A');
  ps.flush();
  ps.println(0xFFFFF);
  ps.println(0xFFFFFFFFFFL);
  ps.println(3.141592);
  ps.println((double)99999999999.9999);
  ps.println(carray);
  ps.println("This is a string"); 
  ps.println(ps);
  ps.write('B');
  ps.println();
  ps.write(barray, 0, barray.length);
  ps.println();
  ps.close();

  if (ps.checkError())
    System.out.println("FAILED: Printing Test");
  else
    System.out.println("PASSED: Printing Test");
 
  System.out.println("PASSED: Test of PrintStream");
}

} // class PrintStreamTest

