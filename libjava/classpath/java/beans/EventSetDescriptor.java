/* java.beans.EventSetDescriptor
 Copyright (C) 1998, 2006 Free Software Foundation, Inc.

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

import gnu.java.lang.ClassHelper;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Vector;

/**
 * EventSetDescriptor describes the hookup between an event source class and
 * an event listener class.
 *
 * <p>EventSets have several attributes: the listener class,
 * the events that can be fired to the listener (methods in the listener
 * class), and an add and remove listener method from the event firer's
 * class.
 * </p>
 *
 * <p>
 * The methods have these constraints on them:
 * <ul>
 * <li>event firing methods: must have <code>void</code> return value. Any
 * parameters and exceptions are allowed. May be public, protected or
 * package-protected. (Don't ask me why that is, I'm just following the spec.
 * The only place it is even mentioned is in the Java Beans white paper, and
 * there it is only implied.)</li>
 *
 * <li>add listener method: must have <code>void</code> return value. Must
 * take exactly one argument, of the listener class's type. May fire either
 * zero exceptions, or one exception of type
 * <code>java.util.TooManyListenersException</code>.
 * Must be public.</li>
 *
 * <li>remove listener method: must have <code>void</code> return value. Must
 * take exactly one argument, of the listener class's type. May not fire any
 * exceptions. Must be public.</li>
 * </ul>
 *
 * <p>
 * A final constraint is that event listener classes must extend from
 * EventListener.
 * </p>
 *
 * <p>
 * There are also various design patterns associated with some of the methods
 * of construction. Those are explained in more detail in the appropriate
 * constructors.
 * </p>
 *
 * <p>
 * <strong>Documentation Convention:</strong> for proper Internalization of
 * Beans inside an RAD tool, sometimes there are two names for a property or
 * method: a programmatic, or locale-independent name, which can be used
 * anywhere, and a localized, display name, for ease of use. In the
 * documentation I will specify different String values as either
 * <em>programmatic</em> or <em>localized</em> to make this distinction clear.
 *
 * @author John Keiser
 * @author Robert Schuster (robertschuster@fsfe.org)
 * @since 1.1
 */

public class EventSetDescriptor extends FeatureDescriptor
{
  private Method addListenerMethod;

  private Method removeListenerMethod;

  private Class listenerType;

  private MethodDescriptor[] listenerMethodDescriptors;

  private Method[] listenerMethods;

  private Method getListenerMethod;

  private boolean unicast;

  private boolean inDefaultEventSet = true;

  /**
   * Creates a new <code>EventSetDescriptor</code<.
   *
   * <p>
   * This version of the constructor enforces the rules imposed on the methods
   * described at the top of this class, as well as searching for:
   * </p>
   *
   * <ol>
   * <li>
   * The event-firing method must be non-private with signature <code>void
   * &lt;listenerMethodName&gt;(&lt;eventSetName&gt;Event)</code> (where
   * <code>&lt;eventSetName&gt;</code> has its first character capitalized
   * by the constructor and the Event is a descendant of
   * {@link java.util.EventObject}) in class <code>listenerType</code>
   * (any exceptions may be thrown). <b>Implementation note:</b> Note that
   * there could conceivably be multiple methods with this type of signature
   * (example: <code>java.util.MouseEvent</code> vs.
   * <code>my.very.own.MouseEvent</code>). In this implementation, all
   * methods fitting the description will be put into the
   * <code>EventSetDescriptor</code>, even though the spec says only one
   * should be chosen (they probably weren't thinking as pathologically as I
   * was). I don't like arbitrarily choosing things. If your class has only one
   * such signature, as most do, you'll have no problems.</li>
   *
   * <li>The add and remove methods must be public and named <code>void
   * add&lt;eventSetName&gt;Listener(&lt;listenerType&gt;)</code> and
   * <code>void remove&lt;eventSetName&gt;Listener(&lt;listenerType&gt;)</code>
   * in in class <code>eventSourceClass</code>, where
   * <code>&lt;eventSetName&gt;</code> will have its first letter capitalized.
   * Standard exception rules (see class description) apply.</li>
   * </ol>
   *
   * @param eventSourceClass
   *          the class containing the add/remove listener methods.
   * @param eventSetName
   *          the programmatic name of the event set, generally starting with a
   *          lowercase letter (i.e. fooManChu instead of FooManChu). This will
   *          be used to generate the name of the event object as well as the
   *          names of the add and remove methods.
   * @param listenerType
   *          the class containing the event firing method.
   * @param listenerMethodName
   *          the name of the event firing method.
   * @exception IntrospectionException
   *              if listenerType is not an EventListener, or if methods are not
   *              found or are invalid.
   */
  public EventSetDescriptor(Class<?> eventSourceClass, String eventSetName,
                            Class<?> listenerType, String listenerMethodName)
      throws IntrospectionException
  {
    setName(eventSetName);
    if (!java.util.EventListener.class.isAssignableFrom(listenerType))
      {
        throw new IntrospectionException(
                  "Listener type is not an EventListener.");
      }

    String[] names = new String[1];
    names[0] = listenerMethodName;

    try
      {
        eventSetName = Character.toUpperCase(eventSetName.charAt(0))
                       + eventSetName.substring(1);
      }
    catch (StringIndexOutOfBoundsException e)
      {
        eventSetName = "";
      }

    findMethods(eventSourceClass, listenerType, names,
                "add" + eventSetName + "Listener",
                "remove" + eventSetName + "Listener", eventSetName + "Event");
    this.listenerType = listenerType;
    checkAddListenerUnicast();
    if (this.removeListenerMethod.getExceptionTypes().length > 0)
      {
        throw new IntrospectionException(
                  "Listener remove method throws exceptions.");
      }
  }

  /**
   * Creates a new <code>EventSetDescriptor</code>.
   *
   * <p>This form of the constructor allows you to specify the names of the
   * methods and adds no new constraints on top of the rules already described
   * at the top of the class.
   * </p>
   *
   * @param eventSourceClass
   *          the class containing the add and remove listener methods.
   * @param eventSetName
   *          the programmatic name of the event set, generally starting with a
   *          lowercase letter (i.e. fooManChu instead of FooManChu).
   * @param listenerType
   *          the class containing the event firing methods.
   * @param listenerMethodNames
   *          the names of the even firing methods.
   * @param addListenerMethodName
   *          the name of the add listener method.
   * @param removeListenerMethodName
   *          the name of the remove listener method.
   * @exception IntrospectionException
   *              if listenerType is not an EventListener or if methods are not
   *              found or are invalid.
   */
  public EventSetDescriptor(Class<?> eventSourceClass, String eventSetName,
                            Class<?> listenerType, String[] listenerMethodNames,
                            String addListenerMethodName,
                            String removeListenerMethodName)
      throws IntrospectionException
  {
    setName(eventSetName);
    if (!java.util.EventListener.class.isAssignableFrom(listenerType))
      {
        throw new IntrospectionException(
                  "Listener type is not an EventListener.");
      }

    findMethods(eventSourceClass, listenerType, listenerMethodNames,
                addListenerMethodName, removeListenerMethodName, null);
    this.listenerType = listenerType;
    checkAddListenerUnicast();
    if (this.removeListenerMethod.getExceptionTypes().length > 0)
      {
        throw new IntrospectionException(
                  "Listener remove method throws exceptions.");
      }
  }

  /**
   * Creates a new <code>EventSetDescriptor</code>.
   *
   * <p>
   * This variant of the constructor allows you to specify the names of the
   * methods and adds no new constraints on top of the rules already described
   * at the top of the class.
   * </p>
   * <p>
   * A valid GetListener method is public, flags no exceptions and has one
   * argument which is of type <code>Class</code>
   * {@link java.awt.Component#getListeners(Class)} is such a method.
   * </p>
   * <p>
   * Note: The validity of the return value of the GetListener method is not
   * checked.
   * </p>
   *
   * @param eventSourceClass
   *          the class containing the add and remove listener methods.
   * @param eventSetName
   *          the programmatic name of the event set, generally starting with a
   *          lowercase letter (i.e. fooManChu instead of FooManChu).
   * @param listenerType
   *          the class containing the event firing methods.
   * @param listenerMethodNames
   *          the names of the even firing methods.
   * @param addListenerMethodName
   *          the name of the add listener method.
   * @param removeListenerMethodName
   *          the name of the remove listener method.
   * @param getListenerMethodName
   *          Name of a method which returns the array of listeners.
   * @exception IntrospectionException
   *              if listenerType is not an EventListener or if methods are not
   *              found or are invalid.
   * @since 1.4
   */
  public EventSetDescriptor(Class<?> eventSourceClass, String eventSetName,
                            Class<?> listenerType, String[] listenerMethodNames,
                            String addListenerMethodName,
                            String removeListenerMethodName,
                            String getListenerMethodName)
      throws IntrospectionException
  {
    this(eventSourceClass, eventSetName, listenerType, listenerMethodNames,
         addListenerMethodName, removeListenerMethodName);

    Method newGetListenerMethod = null;

    try
      {
        newGetListenerMethod
          = eventSourceClass.getMethod(getListenerMethodName,
                                       new Class[] { Class.class });
      }
    catch (NoSuchMethodException nsme)
      {
        throw (IntrospectionException)
          new IntrospectionException("No method named " + getListenerMethodName
                                      + " in class " + listenerType
                                      + " which can be used as"
                                      + " getListenerMethod.").initCause(nsme);
      }

    // Note: This does not check the return value (which
    // should be EventListener[]) but the JDK does not either.

    getListenerMethod = newGetListenerMethod;

  }

  /**
   * Creates a new <code>EventSetDescriptor.</code>
   *
   * <p>
   * This variant of the constructor allows you to specify the names of the
   * methods and adds no new constraints on top of the rules already described
   * at the top of the class.
   * </p>
   * <p>
   * A valid GetListener method is public, flags no exceptions and has one
   * argument which is of type <code>Class</code>
   * {@link java.awt.Component#getListeners(Class)} is such a method.
   * </p>
   * <p>
   * Note: The validity of the return value of the GetListener method is not
   * checked.
   * </p>
   *
   * @param eventSetName
   *          the programmatic name of the event set, generally starting with a
   *          lowercase letter (i.e. fooManChu instead of FooManChu).
   * @param listenerType
   *          the class containing the listenerMethods.
   * @param listenerMethods
   *          the event firing methods.
   * @param addListenerMethod
   *          the add listener method.
   * @param removeListenerMethod
   *          the remove listener method.
   * @param getListenerMethod
   *          The method which returns an array of the listeners.
   * @exception IntrospectionException
   *              if the listenerType is not an EventListener, or any of the
   *              methods are invalid.
   * @since 1.4
   */
  public EventSetDescriptor(String eventSetName, Class<?> listenerType,
                            Method[] listenerMethods, Method addListenerMethod,
                            Method removeListenerMethod,
                            Method getListenerMethod)
      throws IntrospectionException
  {
    this(eventSetName, listenerType, listenerMethods, addListenerMethod,
         removeListenerMethod);

    // Do no checks if the getListenerMethod is null.
    if (getListenerMethod.getParameterTypes().length != 1
        || getListenerMethod.getParameterTypes()[0] != Class.class
        || getListenerMethod.getExceptionTypes().length > 0
        || !Modifier.isPublic(getListenerMethod.getModifiers()))
      throw new IntrospectionException("GetListener method is invalid.");

    // Note: This does not check the return value (which
    // should be EventListener[]) but the JDK does not either.

    this.getListenerMethod = getListenerMethod;
  }

  /**
   * Creates a new <code>EventSetDescriptor</code>.
   *
   * <p>This form of constructor allows you to explicitly say which methods
   * do what, and no reflection is done by the <code>EventSetDescriptor</code>.
   * The methods are, however, checked to ensure that they follow the rules
   * set forth at the top of the class.
   *
   * @param eventSetName
   *          the programmatic name of the event set, generally starting with a
   *          lowercase letter (i.e. fooManChu instead of FooManChu).
   * @param listenerType
   *          the class containing the listenerMethods.
   * @param listenerMethods
   *          the event firing methods.
   * @param addListenerMethod
   *          the add listener method.
   * @param removeListenerMethod
   *          the remove listener method.
   * @exception IntrospectionException
   *              if the listenerType is not an EventListener, or any of the
   *              methods are invalid.
   */
  public EventSetDescriptor(String eventSetName, Class<?> listenerType,
                            Method[] listenerMethods, Method addListenerMethod,
                            Method removeListenerMethod)
      throws IntrospectionException
  {
    setName(eventSetName);
    if (!java.util.EventListener.class.isAssignableFrom(listenerType))
      {
        throw new IntrospectionException(
                  "Listener type is not an EventListener.");
      }

    this.listenerMethods = listenerMethods;
    this.addListenerMethod = addListenerMethod;
    this.removeListenerMethod = removeListenerMethod;
    this.listenerType = listenerType;
    checkMethods();
    checkAddListenerUnicast();
    if (this.removeListenerMethod.getExceptionTypes().length > 0)
      {
        throw new IntrospectionException(
                  "Listener remove method throws exceptions.");
      }
  }

  /** Creates a new <code>EventSetDescriptor</code>.
   *
   * <p>This form of constructor allows you to explicitly say which methods do
   * what, and no reflection is done by the <code>EventSetDescriptor</code>.
   * The methods are, however, checked to ensure that they follow the rules
   * set forth at the top of the class.
   *
   * @param eventSetName
   *          the programmatic name of the event set, generally starting with a
   *          lowercase letter (i.e. fooManChu instead of FooManChu).
   * @param listenerType
   *          the class containing the listenerMethods.
   * @param listenerMethodDescriptors
   *          the event firing methods.
   * @param addListenerMethod
   *          the add listener method.
   * @param removeListenerMethod
   *          the remove listener method.
   * @exception IntrospectionException
   *              if the listenerType is not an EventListener, or any of the
   *              methods are invalid.
   */
  public EventSetDescriptor(String eventSetName, Class<?> listenerType,
                            MethodDescriptor[] listenerMethodDescriptors,
                            Method addListenerMethod,
                            Method removeListenerMethod)
      throws IntrospectionException
  {
    setName(eventSetName);
    if (!java.util.EventListener.class.isAssignableFrom(listenerType))
      {
        throw new IntrospectionException(
                  "Listener type is not an EventListener.");
      }

    this.listenerMethodDescriptors = listenerMethodDescriptors;
    this.listenerMethods = new Method[listenerMethodDescriptors.length];
    for (int i = 0; i < this.listenerMethodDescriptors.length; i++)
      {
        this.listenerMethods[i]
           = this.listenerMethodDescriptors[i].getMethod();
      }

    this.addListenerMethod = addListenerMethod;
    this.removeListenerMethod = removeListenerMethod;
    this.listenerType = listenerType;
    checkMethods();
    checkAddListenerUnicast();
    if (this.removeListenerMethod.getExceptionTypes().length > 0)
      {
        throw new IntrospectionException(
                  "Listener remove method throws exceptions.");
      }
  }

  /** Returns the class that contains the event firing methods.
   */
  public Class<?> getListenerType()
  {
    return listenerType;
  }

  /** Returns the event firing methods.
   */
  public Method[] getListenerMethods()
  {
    return listenerMethods;
  }

  /** Returns the event firing methods as {@link MethodDescriptor}.
   */
  public MethodDescriptor[] getListenerMethodDescriptors()
  {
    if (listenerMethodDescriptors == null)
      {
        listenerMethodDescriptors
          = new MethodDescriptor[listenerMethods.length];

        for (int i = 0; i < listenerMethods.length; i++)
          {
            listenerMethodDescriptors[i]
              = new MethodDescriptor(listenerMethods[i]);
          }
      }

    return listenerMethodDescriptors;
  }

  /** Returns the add listener method.
   */
  public Method getAddListenerMethod()
  {
    return addListenerMethod;
  }

  /* Returns the remove listener method.
   */
  public Method getRemoveListenerMethod()
  {
    return removeListenerMethod;
  }

  /**
   * Returns the method that retrieves the listeners or <code>null</code> if
   * it does not exist.
   */
  public Method getGetListenerMethod()
  {
    return getListenerMethod;
  }

  /** Sets whether or not multiple listeners may be added.
   *
   * @param unicast
   *          whether or not multiple listeners may be added.
   */
  public void setUnicast(boolean unicast)
  {
    this.unicast = unicast;
  }

  /** Returns whether or not multiple listeners may be added.
   * (Defaults to false.)
   */
  public boolean isUnicast()
  {
    return unicast;
  }

  /** Sets whether or not this is in the default event set.
   *
   * @param inDefaultEventSet
   *          whether this is in the default event set.
   */
  public void setInDefaultEventSet(boolean inDefaultEventSet)
  {
    this.inDefaultEventSet = inDefaultEventSet;
  }

  /** Returns whether or not this is in the default event set.
   * (Defaults to true.)
   */
  public boolean isInDefaultEventSet()
  {
    return inDefaultEventSet;
  }

  private void checkAddListenerUnicast() throws IntrospectionException
  {
    Class[] addListenerExceptions = this.addListenerMethod.getExceptionTypes();
    if (addListenerExceptions.length > 1)
      {
        throw new IntrospectionException(
                  "Listener add method throws too many exceptions.");
      }
    else if (addListenerExceptions.length == 1
             && !java.util.TooManyListenersException.class
                .isAssignableFrom(addListenerExceptions[0]))
      {
        throw new IntrospectionException(
                  "Listener add method throws too many exceptions.");
      }
  }

  private void checkMethods() throws IntrospectionException
  {
    if (!addListenerMethod.getDeclaringClass()
        .isAssignableFrom(removeListenerMethod.getDeclaringClass())
        && !removeListenerMethod.getDeclaringClass()
        .isAssignableFrom(addListenerMethod.getDeclaringClass()))
      {
        throw new IntrospectionException(
                  "add and remove listener methods do not come from the"
                  + " same class.  This is bad.");
      }
    if (!addListenerMethod.getReturnType().equals(java.lang.Void.TYPE)
        || addListenerMethod.getParameterTypes().length != 1
        || !listenerType.equals(addListenerMethod.getParameterTypes()[0])
        || !Modifier.isPublic(addListenerMethod.getModifiers()))
      {
        throw new IntrospectionException("Add Listener Method invalid.");
      }
    if (!removeListenerMethod.getReturnType().equals(java.lang.Void.TYPE)
        || removeListenerMethod.getParameterTypes().length != 1
        || !listenerType.equals(removeListenerMethod.getParameterTypes()[0])
        || removeListenerMethod.getExceptionTypes().length > 0
        || !Modifier.isPublic(removeListenerMethod.getModifiers()))
      {
        throw new IntrospectionException("Remove Listener Method invalid.");
      }

    for (int i = 0; i < listenerMethods.length; i++)
      {
        if (!listenerMethods[i].getReturnType().equals(java.lang.Void.TYPE)
            || Modifier.isPrivate(listenerMethods[i].getModifiers()))
          {
            throw new IntrospectionException("Event Method "
                                             + listenerMethods[i].getName()
                                             + " non-void or private.");
          }
        if (!listenerMethods[i].getDeclaringClass()
            .isAssignableFrom(listenerType))
          {
            throw new IntrospectionException("Event Method "
                                             + listenerMethods[i].getName()
                                             + " not from class "
                                             + listenerType.getName());
          }
      }
  }

  private void findMethods(Class eventSourceClass, Class listenerType,
                           String listenerMethodNames[],
                           String addListenerMethodName,
                           String removeListenerMethodName,
                           String absurdEventClassCheckName)
      throws IntrospectionException
  {

    /* Find add listener method and remove listener method. */
    Class[] listenerArgList = new Class[1];
    listenerArgList[0] = listenerType;
    try
      {
        this.addListenerMethod
          = eventSourceClass.getMethod(addListenerMethodName,
                                       listenerArgList);
      }
    catch (SecurityException E)
      {
        throw new IntrospectionException(
                  "SecurityException trying to access method "
                  + addListenerMethodName + ".");
      }
    catch (NoSuchMethodException E)
      {
        throw new IntrospectionException("Could not find method "
                                         + addListenerMethodName + ".");
      }

    if (this.addListenerMethod == null
        || !this.addListenerMethod.getReturnType().equals(java.lang.Void.TYPE))
      {
        throw new IntrospectionException(
                  "Add listener method does not exist, is not public,"
                  + " or is not void.");
      }

    try
      {
        this.removeListenerMethod
          = eventSourceClass.getMethod(removeListenerMethodName,
                                       listenerArgList);
      }
    catch (SecurityException E)
      {
        throw new IntrospectionException(
                  "SecurityException trying to access method "
                  + removeListenerMethodName + ".");
      }
    catch (NoSuchMethodException E)
      {
        throw new IntrospectionException("Could not find method "
                                         + removeListenerMethodName + ".");
      }
    if (this.removeListenerMethod == null
        || !this.removeListenerMethod.getReturnType()
           .equals(java.lang.Void.TYPE))
      {
        throw new IntrospectionException(
                  "Remove listener method does not exist, is not public,"
                  + " or is not void.");
      }

    /* Find the listener methods. */
    Method[] methods;
    try
      {
        methods = ClassHelper.getAllMethods(listenerType);
      }
    catch (SecurityException E)
      {
        throw new IntrospectionException(
                  "Security: You cannot access fields in this class.");
      }

    Vector chosenMethods = new Vector();
    boolean[] listenerMethodFound = new boolean[listenerMethodNames.length];
    for (int i = 0; i < methods.length; i++)
      {
        if (Modifier.isPrivate(methods[i].getModifiers()))
          {
            continue;
          }
        Method currentMethod = methods[i];
        Class retval = currentMethod.getReturnType();
        if (retval.equals(java.lang.Void.TYPE))
          {
            for (int j = 0; j < listenerMethodNames.length; j++)
              {
                if (currentMethod.getName().equals(listenerMethodNames[j])
                    && (absurdEventClassCheckName == null
                    || (currentMethod.getParameterTypes().length == 1
                    && ((currentMethod.getParameterTypes()[0])
                        .getName().equals(absurdEventClassCheckName)
                    || (currentMethod.getParameterTypes()[0])
                       .getName().endsWith("." + absurdEventClassCheckName)))))
                  {
                    chosenMethods.addElement(currentMethod);
                    listenerMethodFound[j] = true;
                  }
              }
          }
      }

    /* Make sure we found all the methods we were looking for. */
    for (int i = 0; i < listenerMethodFound.length; i++)
      {
        if (!listenerMethodFound[i])
          {
            throw new IntrospectionException("Could not find event method "
                                             + listenerMethodNames[i]);
          }
      }

    /* Now that we've chosen the listener methods we want, store them. */
    this.listenerMethods = new Method[chosenMethods.size()];
    for (int i = 0; i < chosenMethods.size(); i++)
      {
        this.listenerMethods[i] = (Method) chosenMethods.elementAt(i);
      }
  }

}
