/* DERReader.java
   Copyright (C) 1999 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.java.security.provider;

import java.math.BigInteger;
import gnu.java.security.der.DEREncodingException;

public class DERReader
{
  byte source[];
  int pos;

  static final int UNIVERSAL = 1;
  static final int APPLICATION = 2;
  static final int CONTEXT_SPECIFIC = 3;
  static final int PRIVATE = 4;


  public DERReader()
  {
    source = null;
    pos = 0;
  }

  public DERReader( byte source[] )
  {
    init( source );
  }

  public void init( String source )
  {
    init( source.getBytes() );
  }

  public void init( byte source[] )
  {
    this.source = source;
    pos = 0;
  }

  public BigInteger getBigInteger() throws DEREncodingException
  {
    return new BigInteger( getPrimitive() );
  }

  //Reads Primitive, definite-length method
  private byte[] getPrimitive() throws DEREncodingException
  {
    int tmp = pos;
	
    //Read Identifier
    byte identifier = source[tmp++];
    if( (0x20 & identifier) != 0)
      throw new DEREncodingException();
    int type = translateLeadIdentifierByte(identifier);
    //System.out.println("Type: " + type);

    //get tag
    int tag = (0x1f & identifier);
    //if( tag == 0x1f)
    //	tag = getIdentifier(tmp);
    //System.out.println("Tag: " + tag);

    //get length
    byte len = source[tmp]; //may be length of length parameter
    long length =  0x7f & len;
    int i;
    if( (0x80 & len) != 0 ) {
      //System.out.println("Extra Long Length");
      len &= 0x7f;
      //System.out.println("Length of Length: " + len);
      //get length here
      length = 0;
      for( i = 0; i < len; i++ ) {
	tmp++;
	length <<= 8;
	length += (source[tmp] < 0 ) ? 
	  (256 + source[tmp]) : 
	  source[tmp];
	//System.out.println("Length of Length: " + length);
      }
      tmp++;
    } else
      tmp++;

    /*System.out.println("Position: " + tmp);
      System.out.println("Length: " + length);
      for( i = 0; i < 10; i++)
      System.out.print(source[tmp + i] + " ");
      System.out.println();*/

    byte tmpb[] = new byte[ (int)length ];
    System.arraycopy( source, tmp, tmpb, 0, (int)length);
    pos = (int)(tmp + length);
    return tmpb;	
  }

  private int translateLeadIdentifierByte(byte b)
  {
    if( (0x3f & b ) == b)
      return UNIVERSAL;
    else if( (0x7f & b ) == b)
      return APPLICATION;
    else if( (0xbf & b ) == b)
      return CONTEXT_SPECIFIC;
    else 
      return PRIVATE;
  }

  private int getIdentifier(int tpos)
  {
    while( (0x80 & source[tpos]) != 0)
      tpos++;
    return tpos;
  }
}
