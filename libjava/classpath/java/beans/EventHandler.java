/* java.beans.EventHandler
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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * <p>EventHandler forms a bridge between dynamically created listeners and
 * arbitrary properties and methods.</p>
 *
 * <p>You can use this class to easily create listener implementations for
 * some basic interactions between an event source and its target. Using
 * the three static methods named <code>create</code> you can create
 * these listener implementations.</p>
 *
 * <p>See the documentation of each method for usage examples.</p>
 *
 * @author Jerry Quinn (jlquinn@optonline.net)
 * @author Robert Schuster (thebohemian@gmx.net)
 * @since 1.4
 */
public class EventHandler implements InvocationHandler
{
  // The name of the method that will be implemented.  If null, any method.
  private String listenerMethod;

  // The object to call action on.
  private Object target;

  // The name of the method or property setter in target.
  private String action;

  // The property to extract from an event passed to listenerMethod.
  private String property;

  // The target objects Class.
  private Class targetClass;

  // String class doesn't already have a capitalize routine.
  private String capitalize(String s)
  {
    return s.substring(0, 1).toUpperCase() + s.substring(1);
  }

  /**
   * Creates a new <code>EventHandler</code> instance.
   *
   * <p>Typical creation is done with the create method, not by knewing an
   * EventHandler.</p>
   *
   * <p>This constructs an EventHandler that will connect the method
   * listenerMethodName to target.action, extracting eventPropertyName from
   * the first argument of listenerMethodName. and sending it to action.</p>
   *
   * <p>Throws a <code>NullPointerException</code> if the <code>target</code>
   * argument is <code>null</code>.
   *
   * @param target Object that will perform the action.
   * @param action A property or method of the target.
   * @param eventPropertyName A readable property of the inbound event.
   * @param listenerMethodName The listener method name triggering the action.
   */
  public EventHandler(Object target, String action, String eventPropertyName,
                      String listenerMethodName)
  {
    this.target = target;

    // Retrieving the class is done for two reasons:
    // 1) The class object is needed very frequently in the invoke() method.
    // 2) The constructor should throw a NullPointerException if target is null.
    targetClass = target.getClass();

    this.action = action;       // Turn this into a method or do we wait till
                // runtime
    property = eventPropertyName;
    listenerMethod = listenerMethodName;
  }

  /**
   * Returns the event property name.
   */
  public String getEventPropertyName()
  {
    return property;
  }

  /**
   * Returns the listener's method name.
   */
  public String getListenerMethodName()
  {
    return listenerMethod;
  }

  /**
   * Returns the target object.
   */
  public Object getTarget()
  {
    return target;
  }

  /**
   * Returns the action method name.
   */
  public String getAction()
  {
    return action;
  }

  // Fetch a qualified property like a.b.c from object o.  The properties can
  // be boolean isProp or object getProp properties.
  //
  // Returns a length 2 array with the first entry containing the value
  // extracted from the property, and the second entry contains the class of
  // the method return type.
  //
  // We play this game because if the method returns a native type, the return
  // value will be a wrapper.  If we then take the type of the wrapper and use
  // it to locate the action method that takes the native type, it won't match.
  private Object[] getProperty(Object o, String prop)
  {
    // Isolate the first property name from a.b.c.
    int pos;
    String rest = null;
    if ((pos = prop.indexOf('.')) != -1)
      {
        rest = prop.substring(pos + 1);
        prop = prop.substring(0, pos);
      }

    // Find a method named getProp.  It could be isProp instead.
    Method getter;
    try
      {
        // Look for boolean property getter isProperty
        getter = o.getClass().getMethod("is" + capitalize(prop));
      }
    catch (NoSuchMethodException nsme1)
      {
        try {
          // Look for regular property getter getProperty
          getter = o.getClass().getMethod("get" + capitalize(prop));
        } catch(NoSuchMethodException nsme2) {
            try {
            // Finally look for a method of the name prop
            getter = o.getClass().getMethod(prop);
            } catch(NoSuchMethodException nsme3) {
                // Ok, give up with an intelligent hint for the user.
                throw new RuntimeException("Method not called: Could not find a property or method '" + prop
                        + "' in " + o.getClass() + " while following the property argument '" + property + "'.");
            }
        }
      }
    try {
      Object val = getter.invoke(o);

      if (rest != null)
        return getProperty(val, rest);

      return new Object[] {val, getter.getReturnType()};
    } catch(InvocationTargetException ite) {
        throw new RuntimeException("Method not called: Property or method '" + prop + "' has thrown an exception.", ite);
    } catch(IllegalAccessException iae) {
        // This cannot happen because we looked up method with Class.getMethod()
        // which returns public methods only.
        throw (InternalError) new InternalError("Non-public method was invoked.").initCause(iae);
    }
  }

  /**
   * Invokes the <code>EventHandler</code>.
   *
   * <p>This method is normally called by the listener's proxy implementation.</p>
   *
   * @param proxy The listener interface that is implemented using
   * the proxy mechanism.
   * @param method The method that was called on the proxy instance.
   * @param arguments The arguments which where given to the method.
   * @throws Throwable <code>NoSuchMethodException</code> is thrown when the EventHandler's
   * action method or property cannot be found.
   */
  public Object invoke(Object proxy, Method method, Object[] arguments)
  {
      try {
      // The method instance of the target object. We have to find out which
      // one we have to invoke.
      Method actionMethod = null;

    // Listener methods that weren't specified are ignored.  If listenerMethod
    // is null, then all listener methods are processed.
    if (listenerMethod != null && !method.getName().equals(listenerMethod))
      return null;

    // If a property is defined we definitely need a valid object at
    // arguments[0] that can be used to retrieve a value to which the
    // property of the target gets set.
    if(property != null) {
      // Extracts the argument. We will let it fail with a NullPointerException
      // the caller used a listener method that has no arguments.
      Object event = arguments[0];

      // Obtains the property XXX propertyType keeps showing up null - why?
      // because the object inside getProperty changes, but the ref variable
      // can't change this way, dolt!  need a better way to get both values out
      // - need method and object to do the invoke and get return type
      Object v[] = getProperty(event, property);
      Object[] args = new Object[] { v[0] };

      // Changes the class array that controls which method signature we are going
      // to look up in the target object.
      Class[] argTypes = new Class[] { initClass((Class) v[1]) };

      // Tries to  find a setter method to which we can apply the
      while(argTypes[0] != null) {
      try
      {
        // Look for a property setter for action.
        actionMethod = targetClass.getMethod("set" + capitalize(action), argTypes);

        return actionMethod.invoke(target, args);
      }
    catch (NoSuchMethodException e)
      {
        // If action as property didn't work, try as method later.
      }

      argTypes[0] = nextClass(argTypes[0]);
      }

      // We could not find a suitable setter method. Now we try again interpreting
      // action as the method name itself.
      // Since we probably have changed the block local argTypes array
      // we need to rebuild it.
      argTypes = new Class[] { initClass((Class) v[1]) };

      // Tries to  find a setter method to which we can apply the
      while(argTypes[0] != null) {
        try
        {
          actionMethod = targetClass.getMethod(action, argTypes);

          return actionMethod.invoke(target, args);
        }
        catch (NoSuchMethodException e)
        {
        }

        argTypes[0] = nextClass(argTypes[0]);
      }

        throw new RuntimeException("Method not called: Could not find a public method named '"
                + action + "' in target " + targetClass + " which takes a '"
                + v[1] + "' argument or a property of this type.");
      }

    // If property was null we will search for a no-argument method here.
    // Note: The ordering of method lookups is important because we want to prefer no-argument
    // calls like the JDK does. This means if we have actionMethod() and actionMethod(Event) we will
    // call the first *EVEN* if we have a valid argument for the second method. This is behavior compliant
    // to the JDK.
    // If actionMethod() is not available but there is a actionMethod(Event) we take this. That makes us
    // more specification compliant than the JDK itself because this one will fail in such a case.
    try
      {
      actionMethod = targetClass.getMethod(action);
      }
    catch(NoSuchMethodException nsme)
      {
        // Note: If we want to be really strict the specification says that a no-argument method should
        // accept an EventObject (or subclass I guess). However since the official implementation is broken
        // anyways, it's more flexible without the EventObject restriction and we are compatible on everything
        // else this can stay this way.
        if(arguments != null && arguments.length >= 1/* && arguments[0] instanceof EventObject*/) {
            Class[] targetArgTypes = new Class[] { initClass(arguments[0].getClass()) };

            while(targetArgTypes[0] != null) {
                try
                {
                  // If no property exists we expect the first element of the arguments to be
                  // an EventObject which is then applied to the target method.

                  actionMethod = targetClass.getMethod(action, targetArgTypes);

                  return actionMethod.invoke(target, new Object[] { arguments[0] });
                }
                catch(NoSuchMethodException nsme2)
                {

                }

                targetArgTypes[0] = nextClass(targetArgTypes[0]);
            }

        }
      }

    // If we do not have a Method instance at this point this means that all our tries
    // failed. The JDK throws an ArrayIndexOutOfBoundsException in this case.
    if(actionMethod == null)
      throw new ArrayIndexOutOfBoundsException(0);

    // Invoke target.action(property)
    return actionMethod.invoke(target);
      } catch(InvocationTargetException ite) {
         throw new RuntimeException(ite.getCause());
      } catch(IllegalAccessException iae) {
          // Cannot happen because we always use getMethod() which returns public
          // methods only. Otherwise there is something seriously broken in
          // GNU Classpath.
          throw (InternalError) new InternalError("Non-public method was invoked.").initCause(iae);
      }
  }

  /**
   * <p>Returns the primitive type for every wrapper class or the
   * class itself if it is no wrapper class.</p>
   *
   * <p>This is needed because to be able to find both kinds of methods:
   * One that takes a wrapper class as the first argument and one that
   * accepts a primitive instead.</p>
   */
  private Class initClass(Class klass) {
   if(klass == Boolean.class) {
    return Boolean.TYPE;
   } else if(klass == Byte.class) {
    return Byte.TYPE;
   } else if(klass == Short.class) {
    return Short.TYPE;
   } else if(klass == Integer.class) {
    return Integer.TYPE;
   } else if(klass == Long.class) {
    return Long.TYPE;
   } else if(klass == Float.class) {
    return Float.TYPE;
   } else if(klass == Double.class) {
    return Double.TYPE;
   } else {
    return klass;
   }
  }

  /**
   *
   *
   * @param klass
   * @return
   */
  private Class nextClass(Class klass) {
    if(klass == Boolean.TYPE) {
    return Boolean.class;
   } else if(klass == Byte.TYPE) {
    return Byte.class;
   } else if(klass == Short.TYPE) {
    return Short.class;
   } else if(klass == Integer.TYPE) {
    return Integer.class;
   } else if(klass == Long.TYPE) {
    return Long.class;
   } else if(klass == Float.TYPE) {
    return Float.class;
   } else if(klass == Double.TYPE) {
    return Double.class;
   } else {
    return klass.getSuperclass();
   }
   }

  /**
   * <p>Constructs an implementation of <code>listenerInterface</code>
   * to dispatch events.</p>
   *
   * <p>You can use such an implementation to simply call a public
   * no-argument method of an arbitrary target object or to forward
   * the first argument of the listener method to the target method.</p>
   *
   * <p>Call this method like:</p>
   * <code>
   * button.addActionListener((ActionListener)
   *    EventHandler.create(ActionListener.class, target, "dispose"));
   * </code>
   *
   * <p>to achieve the following behavior:</p>
   * <code>
   * button.addActionListener(new ActionListener() {
   *    public void actionPerformed(ActionEvent ae) {
   *        target.dispose();
   *    }
   * });
   * </code>
   *
   * <p>That means if you need a listener implementation that simply calls a
   * a no-argument method on a given instance for <strong>each</strong>
   * method of the listener interface.</p>
   *
   * <p>Note: The <code>action</code> is interpreted as a method name. If your target object
   * has no no-argument method of the given name the EventHandler tries to find
   * a method with the same name but which can accept the first argument of the
   * listener method. Usually this will be an event object but any other object
   * will be forwarded, too. Keep in mind that using a property name instead of a
   * real method here is wrong and will throw an <code>ArrayIndexOutOfBoundsException</code>
   * whenever one of the listener methods is called.<p/>
   *
   * <p>The <code>EventHandler</code> will automatically convert primitives
   * to their wrapper class and vice versa. Furthermore it will call
   * a target method if it accepts a superclass of the type of the
   * first argument of the listener method.</p>
   *
   * <p>In case that the method of the target object throws an exception
   * it will be wrapped in a <code>RuntimeException</code> and thrown out
   * of the listener method.</p>
   *
   * <p>In case that the method of the target object cannot be found an
   * <code>ArrayIndexOutOfBoundsException</code> will be thrown when the
   * listener method is invoked.</p>
   *
   * <p>A call to this method is equivalent to:
   * <code>create(listenerInterface, target, action, null, null)</code></p>
   *
   * @param listenerInterface Listener interface to implement.
   * @param target Object to invoke action on.
   * @param action Target property or method to invoke.
   * @return A constructed proxy object.
   */
  public static <T> T create(Class<T> listenerInterface, Object target,
                             String action)
  {
    return create(listenerInterface, target, action, null, null);
  }

  /**
   * <p>Constructs an implementation of <code>listenerInterface</code>
   * to dispatch events.</p>
   *
   * <p>Use this method if you want to create an implementation that retrieves
   * a property value from the <b>first</b> argument of the listener method
   * and applies it to the target's property or method. This first argument
   * of the listener is usually an event object but any other object is
   * valid, too.</p>
   *
   * <p>You can set the value of <code>eventPropertyName</code> to "prop"
   * to denote the retrieval of a property named "prop" from the event
   * object. In case that no such property exists the <code>EventHandler</code>
   * will try to find a method with that name.</p>
   *
   * <p>If you set <code>eventPropertyName</code> to a value like this "a.b.c"
   * <code>EventHandler</code> will recursively evaluate the properties "a", "b"
   * and "c". Again if no property can be found the <code>EventHandler</code>
   * tries a method name instead. This allows mixing the names, too: "a.toString"
   * will retrieve the property "a" from the event object and will then call
   * the method "toString" on it.</p>
   *
   * <p>An exception thrown in any of these methods will provoke a
   * <code>RuntimeException</code> to be thrown which contains an
   * <code>InvocationTargetException</code> containing the triggering exception.</p>
   *
   * <p>If you set <code>eventPropertyName</code> to a non-null value the
   * <code>action</code> parameter will be interpreted as a property name
   * or a method name of the target object.</p>
   *
   * <p>Any object retrieved from the event object and applied to the
   * target will converted from primitives to their wrapper class or
   * vice versa or applied to a method that accepts a superclass
   * of the object.</p>
   *
   * <p>Examples:</p>
   * <p>The following code:</p><code>
   * button.addActionListener(
   *    new ActionListener() {
   *        public void actionPerformed(ActionEvent ae) {
   *            Object o = ae.getSource().getClass().getName();
   *            textField.setText((String) o);
   *        }
   *    });
   * </code>
   *
   * <p>Can be expressed using the <code>EventHandler</code> like this:</p>
   * <p>
   * <code>button.addActionListener((ActionListener)
   *    EventHandler.create(ActionListener.class, textField, "text", "source.class.name");
   * <code>
   * </p>
   *
   * <p>As said above you can specify the target as a method, too:</p>
   * <p>
   * <code>button.addActionListener((ActionListener)
   *    EventHandler.create(ActionListener.class, textField, "setText", "source.class.name");
   * <code>
   * </p>
   *
   * <p>Furthermore you can use method names in the property:</p>
   * <p>
   * <code>button.addActionListener((ActionListener)
   *    EventHandler.create(ActionListener.class, textField, "setText", "getSource.getClass.getName");
   * <code>
   * </p>
   *
   * <p>Finally you can mix names:</p>
   * <p>
   * <code>button.addActionListener((ActionListener)
   *    EventHandler.create(ActionListener.class, textField, "setText", "source.getClass.name");
   * <code>
   * </p>
   *
   * <p>A call to this method is equivalent to:
   * <code>create(listenerInterface, target, action, null, null)</code>
   * </p>
   *
   * @param listenerInterface Listener interface to implement.
   * @param target Object to invoke action on.
   * @param action Target property or method to invoke.
   * @param eventPropertyName Name of property to extract from event.
   * @return A constructed proxy object.
   */
  public static <T> T create(Class<T> listenerInterface, Object target,
                             String action, String eventPropertyName)
  {
    return create(listenerInterface, target, action, eventPropertyName, null);
  }

  /**
   * <p>Constructs an implementation of <code>listenerInterface</code>
   * to dispatch events.</p>
   *
   * <p>Besides the functionality described for {@link create(Class, Object, String)}
   * and {@link create(Class, Object, String, String)} this method allows you
   * to filter the listener method that should have an effect. Look at these
   * method's documentation for more information about the <code>EventHandler</code>'s
   * usage.</p>
   *
   * <p>If you want to call <code>dispose</code> on a <code>JFrame</code> instance
   * when the <code>WindowListener.windowClosing()</code> method was invoked use
   * the following code:</p>
   * <p>
   * <code>
   * EventHandler.create(WindowListener.class, jframeInstance, "dispose", null, "windowClosing");
   * </code>
   * </p>
   *
   * <p>A <code>NullPointerException</code> is thrown if the <code>listenerInterface</code>
   * or <code>target</code> argument are <code>null</code>.
   *
   * @param listenerInterface Listener interface to implement.
   * @param target Object to invoke action on.
   * @param action Target method name to invoke.
   * @param eventPropertyName Name of property to extract from event.
   * @param listenerMethodName Listener method to implement.
   * @return A constructed proxy object.
   */
  public static <T> T create(Class<T> listenerInterface, Object target,
                             String action, String eventPropertyName,
                             String listenerMethodName)
  {
    // Create EventHandler instance
    EventHandler eh = new EventHandler(target, action, eventPropertyName,
                                       listenerMethodName);

    // Create proxy object passing in the event handler
    Object proxy = Proxy.newProxyInstance(listenerInterface.getClassLoader(),
                                          new Class<?>[] {listenerInterface},
                                          eh);

    return (T) proxy;
  }
}
