/* Charset.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package java.nio.charset;


import java.nio.*;

public class Charset
{
    public static Charset forName(String name)
    {
	return new Charset();
    }

/*
    public CharsetDecoder newDecoder()
    {	
	return new CharsetDecoder(this,2,2)
	    {
		protected CoderResult decodeLoop(ByteBuffer  in,
						 CharBuffer  out)
		{
		    while (in.hasRemaining())
			{
			    char a = (char) in.get();
			    out.put(a);
			}
		    return null;
		}
	    };
    }

    public CharsetEncoder newEncoder()
    {		
	return new CharsetEncoder(this,2,2)
	    {
		protected CoderResult encodeLoop(CharBuffer  in,
						 ByteBuffer  out)
		{
		    //System.out.println("in encode loop:"+in.hasRemaining());

		    while (in.hasRemaining())
			{
			    char a = in.get();
			    out.put((byte)a);

			    //int len = out.position();
			    //System.out.println("pos="+len + ","+a);
			}
		    return null;
		}
	    };
    }
 */
}
