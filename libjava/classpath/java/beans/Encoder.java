/* Encoder.java
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


package java.beans;

import gnu.java.beans.encoder.ArrayPersistenceDelegate;
import gnu.java.beans.encoder.ClassPersistenceDelegate;
import gnu.java.beans.encoder.CollectionPersistenceDelegate;
import gnu.java.beans.encoder.MapPersistenceDelegate;
import gnu.java.beans.encoder.PrimitivePersistenceDelegate;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

/**
 * @author Robert Schuster (robertschuster@fsfe.org)
 * @since 1.4
 */
public class Encoder
{

  /**
   * An internal DefaultPersistenceDelegate instance that is used for every
   * class that does not a have a special special PersistenceDelegate.
   */
  private static PersistenceDelegate defaultPersistenceDelegate;

  private static PersistenceDelegate fakePersistenceDelegate;

  /**
   * Stores the relation Class->PersistenceDelegate.
   */
  private static HashMap delegates = new HashMap();

  /**
   * Stores the relation oldInstance->newInstance
   */
  private IdentityHashMap candidates = new IdentityHashMap();

  private ExceptionListener exceptionListener;

  /**
   * A simple number that is used to restrict the access to writeExpression and
   * writeStatement. The rule is that both methods should only be used when an
   * object is written to the stream (= writeObject). Therefore accessCounter is
   * incremented just before the call to writeObject and decremented afterwards.
   * Then writeStatement and writeExpression allow execution only if
   * accessCounter is bigger than zero.
   */
  private int accessCounter = 0;

  public Encoder()
  {
    setupDefaultPersistenceDelegates();

    setExceptionListener(null);
  }

  /**
   * Sets up a bunch of {@link PersistenceDelegate} instances which are needed
   * for the basic working of a {@link Encoder}s.
   */
  private static void setupDefaultPersistenceDelegates()
  {
    synchronized (delegates)
      {
        if (defaultPersistenceDelegate != null)
          return;

        delegates.put(Class.class, new ClassPersistenceDelegate());

        PersistenceDelegate pd = new PrimitivePersistenceDelegate();
        delegates.put(Boolean.class, pd);
        delegates.put(Byte.class, pd);
        delegates.put(Short.class, pd);
        delegates.put(Integer.class, pd);
        delegates.put(Long.class, pd);
        delegates.put(Float.class, pd);
        delegates.put(Double.class, pd);

        delegates.put(Object[].class, new ArrayPersistenceDelegate());

        pd = new CollectionPersistenceDelegate();
        delegates.put(ArrayList.class, pd);
        delegates.put(LinkedList.class, pd);
        delegates.put(Vector.class, pd);
        delegates.put(HashSet.class, pd);
        delegates.put(LinkedHashSet.class, pd);
        delegates.put(TreeSet.class, pd);
        
        pd = new MapPersistenceDelegate();
        delegates.put(HashMap.class, pd);
        delegates.put(TreeMap.class, pd);
        delegates.put(java.util.Hashtable.class, pd);
        delegates.put(java.util.IdentityHashMap.class, pd);
        
        delegates.put(java.util.LinkedHashMap.class, pd);
        delegates.put(java.util.Properties.class, pd);

        delegates.put(java.awt.RenderingHints.class, pd);
        delegates.put(java.util.WeakHashMap.class, pd);
        delegates.put(javax.swing.UIDefaults.class, pd);
        
        // TODO: These classes need to be implemented first
        //delegates.put(java.security.AuthProvider.class, pd);
        //delegates.put(java.util.concurrent.ConcurrentHashMap.class, pd);
        //delegates.put(java.util.EnumMap.class, pd);
        //delegates.put(javax.management.openmbean.TabularDataSupport.class, pd);
        
        defaultPersistenceDelegate = new DefaultPersistenceDelegate();
        delegates.put(Object.class, defaultPersistenceDelegate);

        // Creates a PersistenceDelegate implementation which is
        // returned for 'null'. In practice this instance is
        // not used in any way and is just here to be compatible
        // with the reference implementation which returns a
        // similar instance when calling getPersistenceDelegate(null) .
        fakePersistenceDelegate = new PersistenceDelegate()
        {
          protected Expression instantiate(Object o, Encoder e)
          {
            return null;
          }
        };

      }
  }

  protected void writeObject(Object o)
  {
    // 'null' has no PersistenceDelegate and will not
    // create an Expression which has to be cloned.
    // However subclasses should be aware that writeObject
    // may be called with a 'null' argument and should
    // write the proper representation of it.
    if (o == null)
      return;

    PersistenceDelegate pd = getPersistenceDelegate(o.getClass());

    accessCounter++;
    pd.writeObject(o, this);
    accessCounter--;
    
  }

  /**
   * Sets the {@link ExceptionListener} instance to be used for reporting
   * recorable exceptions in the instantiation and initialization sequence. If
   * the argument is <code>null</code> a default instance will be used that
   * prints the thrown exception to <code>System.err</code>.
   */
  public void setExceptionListener(ExceptionListener listener)
  {
    exceptionListener = (listener != null) ? listener : new ExceptionListener()
    {
      public void exceptionThrown(Exception e)
      {
        System.err.println("exception thrown: " + e);
        e.printStackTrace();
      }
    };
  }

  /**
   * Returns the currently active {@link ExceptionListener} instance.
   */
  public ExceptionListener getExceptionListener()
  {
    return exceptionListener;
  }

  public PersistenceDelegate getPersistenceDelegate(Class type)
  {
    // This is not specified but the JDK behaves like this.
    if (type == null)
      return fakePersistenceDelegate;

    // Treats all array classes in the same way and assigns
    // them a shared PersistenceDelegate implementation tailored
    // for array instantation and initialization.
    if (type.isArray())
      return (PersistenceDelegate) delegates.get(Object[].class);

    PersistenceDelegate pd = (PersistenceDelegate) delegates.get(type);

    return (pd != null) ? pd : (PersistenceDelegate) defaultPersistenceDelegate;
  }

  /**
   * Sets the {@link PersistenceDelegate} instance for the given class.
   * <p>
   * Note: Throws a <code>NullPointerException</code> if the argument is
   * <code>null</code>.
   * </p>
   * <p>
   * Note: Silently ignores PersistenceDelegates for Array types and primitive
   * wrapper classes.
   * </p>
   * <p>
   * Note: Although this method is not declared <code>static</code> changes to
   * the {@link PersistenceDelegate}s affect <strong>all</strong>
   * {@link Encoder} instances. <strong>In this implementation</strong> the
   * access is thread safe.
   * </p>
   */
  public void setPersistenceDelegate(Class type, PersistenceDelegate delegate)
  {
    // If the argument is null this will cause a NullPointerException
    // which is expected behavior.

    // This makes custom PDs for array, primitive types and their wrappers
    // impossible but this is how the JDK behaves.
    if (type.isArray() || type.isPrimitive() || type == Boolean.class
        || type == Byte.class || type == Short.class || type == Integer.class
        || type == Long.class || type == Float.class || type == Double.class)
      return;

    synchronized (delegates)
      {
        delegates.put(type, delegate);
      }

  }

  public Object remove(Object oldInstance)
  {
    return candidates.remove(oldInstance);
  }

  /**
   * Returns the replacement object which has been created by the encoder during
   * the instantiation sequence or <code>null</code> if the object has not
   * been processed yet.
   * <p>
   * Note: The <code>String</code> class acts as an endpoint for the
   * inherently recursive algorithm of the {@link Encoder}. Therefore instances
   * of <code>String</code> will always be returned by this method. In other
   * words the assertion: <code>
   * assert (anyEncoder.get(anyString) == anyString)
   * </code<
   * will always hold.</p>
   *
   * <p>Note: If <code>null</code> is requested, the result will
   * always be <code>null</code>.</p>
   */
  public Object get(Object oldInstance)
  {
    // String instances are handled in a special way.
    // No one knows why this is not officially specified
    // because this is a rather important design decision.
    return (oldInstance == null) ? null : 
             (oldInstance.getClass() == String.class) ?
               oldInstance : candidates.get(oldInstance);
  }

  /**
   * <p>
   * Note: If you call this method not from within an object instantiation and
   * initialization sequence it will be silently ignored.
   * </p>
   */
  public void writeStatement(Statement stmt)
  {
    // Silently ignore out of bounds calls.
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
        newArgs[i] = get(args[i]);
        if (newArgs[i] == null || isImmutableType(args[i].getClass()))
          {
            writeObject(args[i]);
            newArgs[i] = get(args[i]);
          }
      }

    Statement newStmt = new Statement(newTarget, stmt.getMethodName(), newArgs);

    try
      {
        newStmt.execute();
      }
    catch (Exception e)
      {
        exceptionListener.exceptionThrown(e);
      }

  }

  /**
   * <p>
   * Note: If you call this method not from within an object instantiation and
   * initialization sequence it will be silently ignored.
   * </p>
   */
  public void writeExpression(Expression expr)
  {
    // Silently ignore out of bounds calls.
    if (accessCounter <= 0)
      return;

    Object target = expr.getTarget();
    Object value = null;
    Object newValue = null;

    try
      {
        value = expr.getValue();
      }
    catch (Exception e)
      {
        exceptionListener.exceptionThrown(e);
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

            candidates.put(value, newValue);
          }
        catch (Exception e)
          {
            exceptionListener.exceptionThrown(e);
            
            return;
          }
        
        writeObject(value);

      }
    else if(value.getClass() == String.class || value.getClass() == Class.class)
      {
        writeObject(value);
      }

  }

  /** Returns whether the given class is an immutable
   * type which has to be handled differently when serializing it.
   * 
   * <p>Immutable objects always have to be instantiated instead of
   * modifying an existing instance.</p>
   * 
   * @param type The class to test.
   * @return Whether the first argument is an immutable type.
   */
  boolean isImmutableType(Class type)
  {
    return type == String.class || type == Class.class
      || type == Integer.class || type == Boolean.class
      || type == Byte.class || type == Short.class
      || type == Long.class || type == Float.class
      || type == Double.class;
  }
  
  /** Sets the stream candidate for a given object.
   * 
   * @param oldObject The object given to the encoder.
   * @param newObject The object the encoder generated.
   */
  void putCandidate(Object oldObject, Object newObject)
  {
    candidates.put(oldObject, newObject);
  }
  
}
