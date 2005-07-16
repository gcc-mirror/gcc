/*************************************************************************
/* TypeSignatureTest.java -- Tests TypeSignature class
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


import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Member;

import gnu.java.lang.reflect.TypeSignature;

public class TypeSignatureTest
{
  public static void pass()
  {
    System.out.print( "PASSED: " );
  }
  
  public static void fail()
  {
    System.out.print( "FAILED: " );
  }

  public static void testClass( Class clazz, String type_code )
  {
    if( TypeSignature.getEncodingOfClass( clazz ).equals( type_code ) )
      pass();
    else
      fail();
    
    System.out.println( "class encoding of " + clazz );
  }

  public static void testGetClass( Class clazz, String type_code )
    throws ClassNotFoundException
  {
    if( TypeSignature.getClassForEncoding( type_code ).equals( clazz ) )
      pass();
    else
      fail();
    
    System.out.println( "class from encoding " + type_code );
  }

  public static void testConstructor( Constructor c, String type_code )
  {
    if( TypeSignature.getEncodingOfConstructor( c ).equals( type_code ) )
      pass();
    else
      fail();
    
    System.out.println( "constructor encoding of " + c );
  }

  public static void testMethod( Method m, String type_code )
  {
    if( TypeSignature.getEncodingOfMethod( m ).equals( type_code ) )
      pass();
    else
      fail();
    
    System.out.println( "method encoding of " + m );
  }

  public static void testMember( Member m, String type_code )
  {
    if( TypeSignature.getEncodingOfMember( m ).equals( type_code ) )
      pass();
    else
      fail();
    
    System.out.println( "member encoding of " + m );
  }

  public static void main( String[] args )
  {
    try
    {
      // test getEncodingOfClass
      testClass( Boolean.TYPE, "Z" );
      testClass( Byte.TYPE, "B" );
      testClass( Character.TYPE, "C" );
      testClass( Double.TYPE, "D" );
      testClass( Float.TYPE, "F" );
      testClass( Integer.TYPE, "I" );
      testClass( Long.TYPE, "J" );
      testClass( Short.TYPE, "S" );
      testClass( (new int[] {}).getClass(), "[I" );
      testClass( (new float[][][] {}).getClass(), "[[[F" );
      testClass( String.class, "Ljava/lang/String;" );
      testClass( TypeSignatureTest.class, "LTypeSignatureTest;" );

      // test named inner-class
      TypeSignatureTest tst = new TypeSignatureTest();
      Inner i = tst.new Inner();
      testClass( i.getClass(),
		 "LTypeSignatureTest$Inner;" );

      // test anonymous inner-class
      Anon anon = new Anon() { public void f() {} };
      testClass( anon.getClass(), "LTypeSignatureTest$1;" );

      //test getEncodingOfConstructor
      testConstructor( String.class.getConstructor( new Class[] {} ),
		       "()V" );    
      testConstructor(
	String.class.getConstructor( new Class[]
				     { (new byte[]{}).getClass() } ),
	"([B)V" );
    
      testConstructor( 
	String.class.getConstructor( new Class[] { StringBuffer.class } ),
	"(Ljava/lang/StringBuffer;)V" );

      // test getEncodingOfMethod
      testMethod( 
	String.class.getMethod( "lastIndexOf",
				new Class[] { Integer.TYPE, Integer.TYPE } ),
	"(II)I" );
      
      testMethod(
	String.class.getMethod( "length", new Class[] {} ),
	"()I" );

      testMethod(
	TypeSignatureTest.class.getMethod( "pass", new Class[] {} ),
	"()V" );

      testMember(
	TypeSignatureTest.class.getField( "i" ),
	"I" );
      
      testMember(
	TypeSignatureTest.class.getField( "s" ),
	"Ljava/lang/String;" );

      testMember(
	TypeSignatureTest.class.getField( "o" ),
	"[[Ljava/lang/Object;" );      

      // test getClassForEncoding
      testGetClass( Boolean.TYPE, "Z" );
      testGetClass( Byte.TYPE, "B" );
      testGetClass( Character.TYPE, "C" );
      testGetClass( Double.TYPE, "D" );
      testGetClass( Float.TYPE, "F" );
      testGetClass( Integer.TYPE, "I" );
      testGetClass( Long.TYPE, "J" );
      testGetClass( Short.TYPE, "S" );
      testGetClass( (new int[] {}).getClass(), "[I" );
      testGetClass( (new float[][][] {}).getClass(), "[[[F" );
      testGetClass( String.class, "Ljava/lang/String;" );
      testGetClass( TypeSignatureTest.class, "LTypeSignatureTest;" );

      // test named inner-class
      testGetClass( i.getClass(),
		 "LTypeSignatureTest$Inner;" );

      // test anonymous inner-class
      testGetClass( anon.getClass(), "LTypeSignatureTest$1;" );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  public int i;
  public String s;
  public Object[][] o;


  class Inner
  {}
}


interface Anon
{
  public void f();
}
