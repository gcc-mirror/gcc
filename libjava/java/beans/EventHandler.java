/* java.beans.EventHandler
   Copyright (C) 2004 Free Software Foundation, Inc.

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


package java.beans;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * class EventHandler
 *
 * EventHandler forms a bridge between dynamically created listeners and
 * arbitrary properties and methods.  The idea is that a Proxy that implements
 * a listener class calls the EventHandler when a listener method is called.
 * The Proxy calls invoke(), which dispatches the event to a method, called
 * the action, in another object, called the target.
 *
 * The event passed to the listener method is used to access a prespecified
 * property, which in turn is passed to the action method.
 * 
 * Normally, call EventHandler.create(), which constructs an EventHandler and
 * a Proxy for the listener interface.  When the listenerMethod gets called on
 * the proxy, it in turn calls invoke on the attached EventHandler.  The
 * invoke call extracts the bean property from the event object and passes it
 * to the action method of target object.
 *
 * TODO: Add examples of using this thing.
 * 
 * @author Jerry Quinn (jlquinn@optonline.net)
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

  // String class doesn't already have a capitalize routine.
  private String capitalize(String s)
  {
    return s.substring(0, 1).toUpperCase() + s.substring(1);
  }

  /**
   * Creates a new <code>EventHandler</code> instance.
   *
   * Typical creation is done with the create method, not by newing an
   * EventHandler.
   *
   * This constructs an EventHandler that will connect the method
   * listenerMethodName to target.action, extracting eventPropertyName from
   * the first argument of listenerMethodName. and sending it to action.
   *
   *
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
    this.action = action;	// Turn this into a method or do we wait till
				// runtime
    property = eventPropertyName;
    listenerMethod = listenerMethodName;
  }

  /**
   * Return the event property name.
   */
  public String getEventPropertyName()
  {
    return property;
  }

  /**
   * Return the listener's method name.
   */
  public String getListenerMethodName()
  {
    return listenerMethod;
  }

  /**
   * Return the target object.
   */
  public Object getTarget()
  {
    return target;
  }

  /**
   * Return the action method name.
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
    throws NoSuchMethodException, IllegalAccessException, InvocationTargetException
  {
    // Use the event object when the property name to extract is null.
    if (prop == null)
      return new Object[] {o, o.getClass()};

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
	getter = o.getClass().getMethod("is" + capitalize(prop),
						 null);
      }
    catch (NoSuchMethodException e)
      {
	// Look for regular property getter getProperty
	getter = o.getClass().getMethod("get" + capitalize(prop),
						 null);
      }
    Object val = getter.invoke(o, null);

    if (rest != null)
      return getProperty(val, rest);

    return new Object[] {val, getter.getReturnType()};
  }


  /**
   * Invoke the event handler.
   *
   * Proxy is the object that was used, method is the method that was invoked
   * on object, and arguments is the set of arguments passed to this method.
   * We assume that the first argument is the event to extract a property
   * from.
   *
   * Assuming that method matches the listener method specified when creating
   * this EventHandler, the desired property is extracted from this argument.
   * The property is passed to target.setAction(), if possible.  Otherwise
   * target.action() is called, where action is the string fed to the
   * constructor.
   *
   * For now we punt on indexed properties.  Sun docs are not clear to me
   * about this.
   *
   * @param proxy The proxy object that had method invoked on it.
   * @param method The method that was invoked.
   * @param arguments Arguments to method.
   * @return Result of invoking target.action on the event property
   */
  public Object invoke(Object proxy, Method method, Object[] arguments)
    throws Exception
  {
    // Do we actually need the proxy?
    if (method == null)
      throw new RuntimeException("Invoking null method");

    // Listener methods that weren't specified are ignored.  If listenerMethod
    // is null, then all listener methods are processed.
    if (listenerMethod != null && !method.getName().equals(listenerMethod))
      return null;

    // Extract the first arg from arguments and do getProperty on arg
    if (arguments == null || arguments.length == 0)
      return null;
    Object event = arguments[0]; // We hope :-)

    // Obtain the property XXX propertyType keeps showing up null - why?
    // because the object inside getProperty changes, but the ref variable
    // can't change this way, dolt!  need a better way to get both values out
    // - need method and object to do the invoke and get return type
    Object v[] = getProperty(event, property);
    Object val = v[0];
    Class propertyType = (Class) v[1];

    // Find the actual method of target to invoke.  We can't do this in the
    // constructor since we don't know the type of the property we extracted
    // from the event then.
    //
    // action can be either a property or a method.  Sun's docs seem to imply
    // that action should be treated as a property first, and then a method,
    // but don't specifically say it.
    //
    // XXX check what happens with native type wrappers.  The better thing to
    // do is look at the return type of the method
    Method actionMethod;
    try
      {
	// Look for a property setter for action.
	actionMethod = 
	  target.getClass().getMethod("set" + capitalize(action),
				      new Class[] {propertyType});
      }
    catch (NoSuchMethodException e)
      {
	// If action as property didn't work, try as method.
	try
	  {
	    actionMethod = 
	      target.getClass().getMethod(action, new Class[] {propertyType});
	  }
	catch (NoSuchMethodException e1)
	  {
	    // When event property is null, we may call action with no args
	    if (property == null)
	      {
		actionMethod =
		  target.getClass().getMethod(action, null);
		return actionMethod.invoke(target, null);
	      }
	    else
	      throw e1;
	  }
      }

    // Invoke target.action(property)
    return actionMethod.invoke(target, new Object[] {val});
  }

  /**
   * Construct a new object to dispatch events.
   *
   * Equivalent to:
   * create(listenerInterface, target, action, null, null)
   *
   * I.e. all listenerInterface methods are mapped to
   * target.action(EventObject) or target.action(), if the first doesn't
   * exist.
   *
   * @param listenerInterface Listener interface to implement.
   * @param target Object to invoke action on.
   * @param action Target property or method to invoke.
   * @return A constructed proxy object.
   */
  public static Object create(Class listenerInterface, Object target, String action)
  {
    return create(listenerInterface, target, action, null, null);
  }

  /**
   * Construct a new object to dispatch events.
   *
   * Equivalent to:
   * create(listenerInterface, target, action, eventPropertyName, null)
   *
   * I.e. all listenerInterface methods are mapped to
   * target.action(event.getEventPropertyName)
   * 
   *
   * @param listenerInterface Listener interface to implement.
   * @param target Object to invoke action on.
   * @param action Target property or method to invoke.
   * @param eventPropertyName Name of property to extract from event.
   * @return A constructed proxy object.
   */
  public static Object create(Class listenerInterface, Object target,
			      String action, String eventPropertyName)
  {
    return create(listenerInterface, target, action, eventPropertyName, null);
  }


  /**
   * Construct a new object to dispatch events.
   *
   * This creates an object that acts as a proxy for the method
   * listenerMethodName in listenerInterface.  When the listener method is
   * activated, the object extracts eventPropertyName from the event.  Then it
   * passes the property to the method target.setAction, or target.action if
   * action is not a property with a setter.
   *
   * For example, EventHandler.create(MouseListener.class, test, "pushed",
   * "button", "mouseClicked") generates a proxy object that implements
   * MouseListener, at least for the method mouseClicked().  The other methods
   * of MouseListener are null operations.  When mouseClicked is invoked, the
   * generated object extracts the button property from the MouseEvent,
   * i.e. event.getButton(), and calls test.setPushed() with the result.  So under
   * the covers the following happens:
   *
   * <CODE>
   * object.mouseClicked(MouseEvent e) { test.setPushed(e.getButton()); }
   * </CODE>
   *
   * The Sun spec specifies a hierarchical property naming scheme.  Generally
   * if the property is a.b.c, this corresponds to event.getA().getB().getC()
   * or event.getA().getB().isC().  I don't see how you specify an indexed
   * property, though.  This may be a limitation of the Sun implementation as
   * well.  The spec doesn't seem to address it.
   * 
   * If eventPropertyName is null, EventHandler instead uses the event object
   * in place of a property, i.e. it calls target.action(EventObject).  If
   * there is no method named action taking an EventObject argument,
   * EventHandler looks for a method target.action() taking no arguments.
   *
   * If listenerMethodName is null, every method in listenerInterface gets
   * mapped to target.action, rather than the specified listener method.
   * 
   * @param listenerInterface Listener interface to implement.
   * @param target Object to invoke action on.
   * @param action Target method name to invoke.
   * @param eventPropertyName Name of property to extract from event.
   * @param listenerMethodName Listener method to implement.
   * @return A constructed proxy object.
   */
  public static Object create(Class listenerInterface, Object target,
			      String action, String eventPropertyName,
			      String listenerMethodName)
  {
    // Create EventHandler instance
    EventHandler eh = new EventHandler(target, action, eventPropertyName,
				       listenerMethodName);

    // Create proxy object passing in the event handler
    Object proxy = Proxy.newProxyInstance(listenerInterface.getClassLoader(),
					  new Class[] {listenerInterface},
					  eh);

    return proxy;
  }

}
