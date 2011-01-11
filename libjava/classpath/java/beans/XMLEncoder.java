/* XMLEncoder.java
 Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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


package java.beans;

import gnu.java.beans.encoder.ScanEngine;

import java.io.OutputStream;

/**
 * This class uses the {@link PersistenceDelegate} and {@link Encoder}
 * infrastructure to generate an XML representation of the objects it
 * serializes.
 *
 * @author Robert Schuster (robertschuster@fsfe.org)
 * @since 1.4
 */
public class XMLEncoder extends Encoder
{
  Object owner;

  Exception exception;

  ScanEngine scanEngine;

  private int accessCounter = 0;

  public XMLEncoder(OutputStream os)
  {
    scanEngine = new ScanEngine(os);
  }

  public void close()
  {
    if (scanEngine != null)
      {
        scanEngine.close();
        scanEngine = null;
      }
  }

  public void flush()
  {
    scanEngine.flush();
  }

  public void writeExpression(Expression expr)
  {
    // Implementation note: Why is this method overwritten and nearly exactly
    // reimplemented as in Encoder?
    // The Encoder class can (and should be) subclassed by users outside of the
    // java.beans package. While I have doubts that this is possible from an
    // API design point of view I tried to replicate the Encoder's behavior
    // in the JDK as exactly as possible. This strictness however made it
    // extremely complicated to implement the XMLEncoder's backend. Therefore
    // I decided to copy the Encoder's implementation and make all changes
    // I needed for a succesfull operation of XMLEncoder.
    //
    // The same is true for the writeStatement method.

    //  Silently ignore out of bounds calls.
    if (accessCounter <= 0)
      return;

    scanEngine.writeExpression(expr);


    Object target = expr.getTarget();
    Object value = null;
    Object newValue = null;

    try
      {
        value = expr.getValue();
      }
    catch (Exception e)
      {
        getExceptionListener().exceptionThrown(e);
        return;
      }


    newValue = get(value);

    if (newValue == null)
      {
        Object newTarget = get(target);
        if (newTarget == null)
          {
            writeObject(target);
            newTarget = get(target);

            // May happen if exception was thrown.
            if (newTarget == null)
              {
                return;
              }
          }

        Object[] args = expr.getArguments();
        Object[] newArgs = new Object[args.length];

        for (int i = 0; i < args.length; i++)
          {
            newArgs[i] = get(args[i]);
            if (newArgs[i] == null || isImmutableType(args[i].getClass()))
              {
                writeObject(args[i]);
                newArgs[i] = get(args[i]);
              }
          }

        Expression newExpr = new Expression(newTarget, expr.getMethodName(),
                                            newArgs);

        // Fakes the result of Class.forName(<primitiveType>) to make it possible
        // to hand such a type to the encoding process.
        if (value instanceof Class && ((Class) value).isPrimitive())
          newExpr.setValue(value);

        // Instantiates the new object.
        try
          {
            newValue = newExpr.getValue();

            putCandidate(value, newValue);
          }
        catch (Exception e)
          {
            getExceptionListener().exceptionThrown(e);

            // In Statement.writeExpression we had no possibility to flags
            // an erroneous state to the ScanEngine without behaving different
            // to the JDK.
            scanEngine.revoke();

            return;
          }

        writeObject(value);

      }
    else if(value.getClass() == String.class || value.getClass() == Class.class)
      {
        writeObject(value);
      }

    scanEngine.end();
  }

  public void writeStatement(Statement stmt)
  {
    // In case of questions have a at the implementation note in
    // writeExpression.

    scanEngine.writeStatement(stmt);

    //  Silently ignore out of bounds calls.
    if (accessCounter <= 0)
      return;

    Object target = stmt.getTarget();

    Object newTarget = get(target);
    if (newTarget == null)
      {
        writeObject(target);
        newTarget = get(target);
      }

    Object[] args = stmt.getArguments();
    Object[] newArgs = new Object[args.length];

    for (int i = 0; i < args.length; i++)
      {
        // Here is the difference to the original writeStatement
        // method in Encoder. In case that the object is known or
        // not an immutable we put it directly into the ScanEngine
        // which will then generate an object reference for it.
        newArgs[i] = get(args[i]);
        if (newArgs[i] == null || isImmutableType(args[i].getClass()))
          {
            writeObject(args[i]);
            newArgs[i] = get(args[i]);
          }
        else
          scanEngine.writeObject(args[i]);
      }

    Statement newStmt = new Statement(newTarget, stmt.getMethodName(), newArgs);

    try
      {
        newStmt.execute();
      }
    catch (Exception e)
      {
        getExceptionListener().exceptionThrown(e);

        // In Statement.writeStatement we had no possibility to flags
        // an erroneous state to the ScanEngine without behaving different
        // to the JDK.
        scanEngine.revoke();
        return;
      }

    scanEngine.end();
  }

  public void writeObject(Object o)
  {
    accessCounter++;

    scanEngine.writeObject(o);

    if (get(o) == null)
      super.writeObject(o);

    accessCounter--;
  }

  public void setOwner(Object o)
  {
    owner = o;
  }

  public Object getOwner()
  {
    return owner;
  }

}
