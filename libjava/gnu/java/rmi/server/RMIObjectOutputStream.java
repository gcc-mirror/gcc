/* RMIObjectOutputStream.java -
   Copyright (c) 1996, 1997, 1998, 1999, 2002, 2004
   Free Software Foundation, Inc.

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

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.rmi.Remote;
import java.rmi.server.RemoteStub;
import java.rmi.server.RMIClassLoader;

public class RMIObjectOutputStream
	extends ObjectOutputStream {

public RMIObjectOutputStream(OutputStream strm) throws IOException {
	super(strm);
	enableReplaceObject(true);
}

//Separate it for override by MarshalledObject
protected void setAnnotation(String annotation) throws IOException{
    writeObject(annotation);
}

protected void annotateClass(Class cls) throws IOException {
	setAnnotation(RMIClassLoader.getClassAnnotation(cls));
}

protected void annotateProxyClass(Class cls)
        throws IOException
{
    annotateClass(cls);
}
    
protected Object replaceObject(Object obj)
        throws IOException
{
    if((obj instanceof Remote) && !(obj instanceof RemoteStub)){
	    UnicastServerRef ref = UnicastServer.getExportedRef((Remote)obj);
	    if (ref != null)
		    return ref.getStub();
    }
    return obj;
}

protected void writeValue(Object value, Class valueClass) throws IOException{
    if(valueClass.isPrimitive()){
        if(valueClass == Boolean.TYPE)
            writeBoolean(((Boolean)value).booleanValue());
        else
        if(valueClass == Byte.TYPE)
            writeByte(((Byte)value).byteValue());
        else
        if(valueClass == Character.TYPE)
            writeChar(((Character)value).charValue());
        else
        if(valueClass == Short.TYPE)
            writeShort(((Short)value).shortValue());
        else
        if(valueClass == Integer.TYPE)
            writeInt(((Integer)value).intValue());
        else
        if(valueClass == Long.TYPE)
            writeLong(((Long)value).longValue());
        else
        if(valueClass == Float.TYPE)
            writeFloat(((Float)value).floatValue());
        else
        if(valueClass == Double.TYPE)
            writeDouble(((Double)value).doubleValue());
        else
            throw new Error("Unsupported primitive class: " + valueClass);
    } else
        writeObject(value);
}

}
