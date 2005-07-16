/*************************************************************************
/* ObjectStreamClassTest.java -- Tests ObjectStreamClass class
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

import java.io.Externalizable;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.io.Serializable;
import java.util.Hashtable;
import java.util.Vector;

public class ObjectStreamClassTest
{
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

  public static void testLookup( Class cl, boolean non_null )
  {
    if( non_null == (ObjectStreamClass.lookup( cl ) != null) )
      pass();
    else
      fail();

    System.out.println( "lookup() for " + cl );
  }

  public static void testGetName( Class cl, String name )
  {
    if( ObjectStreamClass.lookup( cl ).getName().equals( name ) )
      pass();
    else
      fail();
    
    System.out.println( "getName() for " + cl );
  }

  public static void testForClass( Class cl, Class clazz )
  {
    if( ObjectStreamClass.lookup( cl ).forClass() == clazz )
      pass();
    else
      fail();
    
    System.out.println( "forClass() for " + cl );
  }

  public static void testSUID( Class cl, long suid )
  {
    testSUID( cl, suid, true );
  }

  public static void testSUID( Class cl, long suid, boolean exp_pass )
  {
    if( ObjectStreamClass.lookup( cl ).getSerialVersionUID() == suid )
      pass( exp_pass );
    else
      fail( exp_pass );
    
    System.out.println( "getSerialVersionUID() for " + cl );
  }

  public static void testHasWrite( Class cl, boolean has_write )
  {
    if( ObjectStreamClass.lookup( cl ).hasWriteMethod() == has_write )
      pass();
    else
      fail();
    
    System.out.println( "hasWriteMethod() for " + cl );
  }

  public static void testIsSerial( Class cl, boolean is_serial )
  {
    if( ObjectStreamClass.lookup( cl ).isSerializable() == is_serial )
      pass();
    else
      fail();
    
    System.out.println( "isSerializable() for " + cl );
  }

  public static void testIsExtern( Class cl, boolean is_extern )
  {
    if( ObjectStreamClass.lookup( cl ).isExternalizable() == is_extern )
      pass();
    else
      fail();
    
    System.out.println( "isExternalizable() for " + cl );
  }

  public static void main( String[] args )
  {
    try
    {
      // lookup
      testLookup( Serial.class, true );
      testLookup( NotSerial.class, false );

      // getName
      testGetName( java.lang.String.class, "java.lang.String" );
      testGetName( java.util.Hashtable.class, "java.util.Hashtable" );
      
      // forClass
      testForClass( java.lang.String.class, java.lang.String.class );
      testForClass( java.util.Vector.class, (new Vector()).getClass() );      
      
      // getSerialVersionUID
      testSUID( A.class, 1577839372146469075L );
      testSUID( B.class, -7069956958769787679L );

      // NOTE: this fails for JDK 1.1.5v5 on linux because a non-null
      // jmethodID is returned from
      // GetStaticMethodID( env, C, "<clinit>", "()V" )
      // even though class C does not have a class initializer.
      // The JDK's serialver tool does not have this problem somehow.
      // I have not tested this on other platforms.
      testSUID( C.class, 7441756018870420732L, false );

      testSUID( Defined.class, 17 );
      testSUID( DefinedNotStatic.class, 8797806279193632512L );
      testSUID( DefinedNotFinal.class, -1014973327673071657L );

      // hasWriteMethod
      testHasWrite( Serial.class, false );
      testHasWrite( HasWrite.class, true );
      testHasWrite( InherWrite.class, false );
      testHasWrite( PubWrite.class, false );
      testHasWrite( StaticWrite.class, false );
      testHasWrite( ReturnWrite.class, false );

      // isSerializable
      testIsSerial( Serial.class, true );
      testIsSerial( Extern.class, false );
      testIsSerial( InherSerial.class, true );
      testIsSerial( InherExtern.class, false );

      // isExternalizable
      testIsExtern( Serial.class, false );
      testIsExtern( Extern.class, true );
      testIsExtern( InherSerial.class, false );
      testIsExtern( InherExtern.class, true );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }
}

class NotSerial {}

class A implements Serializable
{
  int b;
  int a;

  public int f() { return 0; }
  float g() { return 3; }

  private float c;
}
  
abstract class B extends A
{
  private B( int[] ar ) {}
  public B() {}
  public static void foo() {}
  public abstract void absfoo();
  
  private static String s;
  public int[] a;

  static
  {
    s = "hello";
  }
}

class C extends B implements Cloneable, Externalizable
{
  public void absfoo() {}
  public void readExternal( ObjectInput i ) {}
  public void writeExternal( ObjectOutput o ) {}
}


class Defined implements Serializable
{
  static final long serialVersionUID = 17;
}

class DefinedNotStatic implements Serializable
{
  final long serialVersionUID = 17;
}

class DefinedNotFinal implements Serializable
{
  static long serialVersionUID = 17;
}

class HasWrite implements Serializable
{
  private void writeObject( ObjectOutputStream o ) {}
}

class InherWrite extends HasWrite {}

class PubWrite implements Serializable
{
  public void writeObject( ObjectOutputStream o ) {}
}

class StaticWrite implements Serializable
{
  private static void writeObject( ObjectOutputStream o ) {}
}

class ReturnWrite implements Serializable
{
  private int writeObject( ObjectOutputStream o ) 
  {
    return -1;
  }
}


class Serial implements Serializable {}

class Extern implements Externalizable
{
  public void readExternal( ObjectInput i )
  {}

  public void writeExternal( ObjectOutput o )
  {}
}

class InherExtern extends Extern implements Serializable {}

class InherSerial extends Serial {}
