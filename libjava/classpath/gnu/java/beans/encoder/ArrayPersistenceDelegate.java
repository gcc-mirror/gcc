/* ArrayPersistenceDelegate.java - A PersistenceDelegate that handles arrays.
 Copyright (C) 2005 Free Software Foundation, Inc.

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
 Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 02110-1301 USA.

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


package gnu.java.beans.encoder;

import java.beans.Encoder;
import java.beans.Expression;
import java.beans.PersistenceDelegate;
import java.beans.Statement;

import java.lang.reflect.Array;
import java.util.HashMap;

public class ArrayPersistenceDelegate extends PersistenceDelegate
{
  private static final HashMap NULL_VALUES = new HashMap();

  static
    {
      NULL_VALUES.put(Boolean.TYPE, Boolean.FALSE);
      NULL_VALUES.put(Byte.TYPE, Byte.valueOf((byte) 0));
      NULL_VALUES.put(Short.TYPE, Short.valueOf((short) 0));
      NULL_VALUES.put(Integer.TYPE, Integer.valueOf(0));
      NULL_VALUES.put(Long.TYPE, Long.valueOf(0));
      NULL_VALUES.put(Float.TYPE, Float.valueOf(0.0f));
      NULL_VALUES.put(Double.TYPE, Double.valueOf(0.0));
    }

  protected Expression instantiate(Object oldInstance, Encoder out)
  {
    Class type = oldInstance.getClass().getComponentType();

    // oldInstance is expected to be an array, then
    // getClass().getComponentType() should lead
    // to its component type.
    assert (type != null);

    // Not handling primitive types in a special way here
    // causes that Class.forName("int") is built as an Expression
    // later which would cause an exception if executed. A special
    // handling to avoid the execution for primitive types can be
    // java.beans.Encoder.writeExpression() .
    return new Expression(
                          oldInstance,
                          Array.class,
                          "newInstance",
                          new Object[] {
                            type,
                            new Integer(Array.getLength(oldInstance)) });
  }

  protected void initialize(Class type, Object oldInstance, Object newInstance,
                            Encoder out)
  {
    int length = Array.getLength(oldInstance);

    // Compares the array value against a prototypical
    // null value of the array's component type in order to skip
    // writing the default values of an array.
    
    // Note: I have no idea why the persistence delegate for arrays writes
    // an Expression that reads the value and then writes a Statement that sets
    // the value. However it turned out that object arrays work better with the
    // get-Expression and primitive array work fine with the set-Statement.
    
    type = type.getComponentType();
    if (type.isPrimitive())
      {
        Object nullValue = NULL_VALUES.get(type);

        for (int i = 0; i < length; i++)
          {
            Object oldValue = Array.get(oldInstance, i);

            if (!oldValue.equals(nullValue))
              {
                out.writeExpression(new Expression(Array.class, "get",
                                                   new Object[] { oldInstance,
                                                                 Integer.valueOf(i),
                                                                 }));
                
                out.writeStatement(new Statement(Array.class, "set",
                                               new Object[] {
                                                 oldInstance,
                                                 Integer.valueOf(i),
                                                 oldValue
                                                 }));
              }
          }
        
      }
    else
      {

        for (int i = 0; i < length; i++)
          {
            Object oldValue = Array.get(oldInstance, i);
            
            if (oldValue != null)
              {
                out.writeExpression(new Expression(Array.class, "get",
                                                 new Object[] { oldInstance,
                                                               Integer.valueOf(i),
                                                               }));
                
                out.writeStatement(new Statement(Array.class, "set",
                                                   new Object[] {
                                                     oldInstance,
                                                     Integer.valueOf(i),
                                                     oldValue
                                                     }));
              }
          }
      }
    
  }

}
