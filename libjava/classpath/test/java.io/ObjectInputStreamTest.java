/*************************************************************************
/* ObjectInputStreamTest.java -- Tests ObjectInputStream class
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

import java.io.FileInputStream;
import java.io.ObjectInputStream;

public class ObjectInputStreamTest extends Test
{
  public static void testSerial( Object obj, String filename )
  {
    try
    {
      ObjectInputStream ois =
	new ObjectInputStream( new FileInputStream( filename ) );

      Object read_object = ois.readObject();
      ois.close();

      if( read_object.equals( obj ) )
	pass();
      else
	fail();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      fail();
    }
  }
  
  
  public static void main( String[] args )
  {
    testSerial( new OOSCallDefault( 1, 3.14, "test" ),
		"calldefault.data" );
    System.out.println( "Object calling defaultWriteObject()" );

    testSerial( new OOSNoCallDefault( 17, "no\ndefault", false ),
		"nocalldefault.data" );
    System.out.println( "Object not calling defaultWriteObject()" );

    testSerial( new OOSExtern( -1, "", true ), "external.data" );
    System.out.println( "Externalizable class" );

    testSerial( new HairyGraph(), "graph.data" );
    System.out.println( "Graph of objects with circular references" );
  }

}
