/* RMIHashes.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2004  Free Software Foundation, Inc.

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


package gnu.java.rmi.server;

import gnu.java.lang.reflect.TypeSignature;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.lang.reflect.Method;
import java.security.MessageDigest;

public class RMIHashes
{
  //There're other places using DigestOutputStream to generate hash in classpath, but I think the way I used
  //here is more efficient, anyway, you can switch to DigestOutputStream by doing like "//or:" comments say.
  
  //or:add this statement: private static final NullOutputStream nullOutputStream = new NullOutputStream ();
  public static long getMethodHash(Method meth)
  {
    //Object Serialization Spec 8.3
    try
    {
        MessageDigest md = MessageDigest.getInstance ("SHA");
        //or:remove this statement: DigestOutputStream digest_out = new DigestOutputStream (nullOutputStream, md);
        ByteArrayOutputStream digest_out = new ByteArrayOutputStream();
        DataOutputStream data_out = new DataOutputStream (digest_out);
        
        StringBuffer sbuf = new StringBuffer();
        sbuf.append(meth.getName());
        sbuf.append('(');
        Class params[] = meth.getParameterTypes();
        for(int i = 0; i < params.length; i++)
            sbuf.append(TypeSignature.getEncodingOfClass(params[i]));
        sbuf.append(')');
        Class rcls = meth.getReturnType();
        if(rcls != Void.TYPE)
            sbuf.append(TypeSignature.getEncodingOfClass(rcls));
        else
            sbuf.append('V');
        
        data_out.writeUTF (sbuf.toString());
        data_out.flush();
        data_out.close ();

        md.update(digest_out.toByteArray()); //or:remove this statement
        byte[] sha = md.digest ();
        long result = 0;
        int len = sha.length < 8 ? sha.length : 8;
        for (int i=0; i < len; i++)
            result += (long)(sha[i] & 0xFF) << (8 * i);
        return result;
    }catch(Exception _){
        return -1L;
        }
  }

  public static long getInterfaceHash(Class clazz)
  {
    return clazz.hashCode ();
  }
}

