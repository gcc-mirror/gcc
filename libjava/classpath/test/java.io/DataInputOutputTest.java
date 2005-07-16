/*************************************************************************
/* DataInputOutputTest.java -- Tests Data{Input,Output}Stream's
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

// Write some data using DataOutput and read it using DataInput.

public class DataInputOutputTest
{

public static void
runReadTest(String filename, int seq, String testname)
{
  try
    {
      System.out.println("Test " + seq + ": " + testname);

      FileInputStream fis = new FileInputStream(filename);
      DataInputStream dis = new DataInputStream(fis); 

      boolean passed = true;

      boolean b = dis.readBoolean();
      if (b != true)
        {
          passed = false;
          System.out.println("Failed to read boolean. Expected true and got false");
        }
      b = dis.readBoolean();
      if (b != false)
        {
          passed = false;
          System.out.println("Failed to read boolean. Expected false and got true");
        }
      byte bt = dis.readByte();
      if (bt != 8)
        {
          passed = false;
          System.out.println("Failed to read byte. Expected 8 and got "+ bt);
        }
      bt = dis.readByte();
      if (bt != -122)
        {
          passed = false;
          System.out.println("Failed to read byte. Expected -122 and got "+ bt);
        }
      char c = dis.readChar();
      if (c != 'a')
        {
          passed = false;
          System.out.println("Failed to read char. Expected a and got " + c);
        }
      c = dis.readChar();
      if (c != '\uE2D2')
        {
          passed = false;
          System.out.println("Failed to read char. Expected \\uE2D2 and got " + c);
        }
      short s = dis.readShort();
      if (s != 32000)
        {
          passed = false;
          System.out.println("Failed to read short. Expected 32000 and got " + s);
        }
      int i = dis.readInt();
      if (i != 8675309)
        {
          passed = false;
          System.out.println("Failed to read int. Expected 8675309 and got " + i);
        }
      long l = dis.readLong();
      if (l != 696969696969L)
        {
          passed = false;
          System.out.println("Failed to read long. Expected 696969696969 and got " + l);
        }
      float f = dis.readFloat();
      if (!Float.toString(f).equals("3.1415"))
        {
          passed = false;
          System.out.println("Failed to read float. Expected 3.1415 and got " + f);
        }
      double d = dis.readDouble();
      if (d != 999999999.999)
        {
          passed = false;
          System.out.println("Failed to read double.  Expected 999999999.999 and got " + d);
        }
      String str = dis.readUTF();
      if (!str.equals("Testing code is such a boring activity but it must be done"))
        {
          passed = false;
          System.out.println("Read unexpected String: " + str);
        }
      str = dis.readUTF();
      if (!str.equals("a-->\u01FF\uA000\u6666\u0200RRR"))
        {
          passed = false;
          System.out.println("Read unexpected String: " + str);
        }
    
      if (passed)
        System.out.println("PASSED: " + testname + " read test");
      else
        System.out.println("FAILED: " + testname + " read test");
    }
  catch (IOException e)
    {
      System.out.println("FAILED: " + testname + " read test: " + e);
    }

}

public static void 
main(String[] argv)
{
  System.out.println("Started test of DataInputStream and DataOutputStream");

  try
    {
      System.out.println("Test 1: DataOutputStream write test");

      FileOutputStream fos = new FileOutputStream("dataoutput.out");
      DataOutputStream dos = new DataOutputStream(fos);

      dos.writeBoolean(true);
      dos.writeBoolean(false);
      dos.writeByte((byte)8);
      dos.writeByte((byte)-122);
      dos.writeChar((char)'a');
      dos.writeChar((char)'\uE2D2');
      dos.writeShort((short)32000);
      dos.writeInt((int)8675309);
      dos.writeLong(696969696969L);
      dos.writeFloat((float)3.1415);
      dos.writeDouble((double)999999999.999);
      dos.writeUTF("Testing code is such a boring activity but it must be done");
      dos.writeUTF("a-->\u01FF\uA000\u6666\u0200RRR");
      dos.close();

      // We'll find out if this was really right later, but conditionally
      // report success for now
      System.out.println("PASSED: DataOutputStream write test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: DataOutputStream write test: " + e);
    }

  runReadTest("dataoutput.out", 2, "Read of JCL written data file");
  runReadTest("dataoutput-jdk.out", 3, "Read of JDK written data file");

} // main

} // class DataInputOutputTest

