/*************************************************************************
/* Test.java -- Base class for test classes
/*
/* Copyright (c) 1998 by Free Software Foundation, Inc.
/*
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published 
/* by the Free Software Foundation, version 2. (see COPYING)
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

public class Test
{
  public static void error()
  {
    System.out.print( "ERROR: " );
  }
  
  public static void pass()
  {
    System.out.print( "PASSED: " );
  }
  
  public static void fail()
  {
    System.out.print( "FAILED: " );
  }

  public static void pass( boolean exp_pass )
  {
    if( exp_pass )
      pass();
    else
      System.out.print( "XPASSED: " );
  }
  
  public static void fail( boolean exp_pass )
  {
    if( exp_pass )
      fail();
    else
      System.out.print( "XFAIL: " );
  }
}
