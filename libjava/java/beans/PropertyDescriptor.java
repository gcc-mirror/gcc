/* java.beans.PropertyDescriptor
   Copyright (C) 1998, 2001 Free Software Foundation, Inc.

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

import java.util.*;
import java.lang.reflect.*;


/**
 ** PropertyDescriptor describes information about a JavaBean property,
 ** by which we mean a property that has been exposed via a pair of
 ** get and set methods.  (There may be no get method, which means
 ** the property is write-only, or no set method, which means the
 ** the property is read-only.)<P>
 **
 ** The constraints put on get and set methods are:<P>
 ** <OL>
 ** <LI>A get method must have signature
 **     <CODE>&lt;propertyType&gt; &lt;getMethodName&gt;()</CODE></LI>
 ** <LI>A set method must have signature
 **     <CODE>void &lt;setMethodName&gt;(&lt;propertyType&gt;)</CODE></LI>
 ** <LI>Either method type may throw any exception.</LI>
 ** <LI>Both methods must be public.</LI>
 ** </OL>
 **
 ** @author John Keiser
 ** @since JDK1.1
 ** @version 1.1.0, 26 Jul 1998
 **/

public class PropertyDescriptor extends FeatureDescriptor {
  Class propertyType;
  Method getMethod;
  Method setMethod;
  
  Class propertyEditorClass;
  boolean bound;
  boolean constrained;
  
  PropertyDescriptor(String name) {
    setName(name);
  }
  
  /** Create a new PropertyDescriptor by introspection.
   ** This form of constructor creates the PropertyDescriptor by
   ** looking for a getter method named <CODE>get&lt;name&gt;()</CODE>
   ** (or, optionally, if the property is boolean,
   ** <CODE>is&lt;name&gt;()</CODE>) and
   ** <CODE>set&lt;name&gt;()</CODE> in class
   ** <CODE>&lt;beanClass&gt;</CODE>, where &lt;name&gt; has its
   ** first letter capitalized by the constructor.<P>
   **
   ** <B>Implementation note:</B> If there is both are both isXXX and
   ** getXXX methods, the former is used in preference to the latter.
   ** We do not check that an isXXX method returns a boolean. In both
   ** cases, this matches the behaviour of JDK 1.4<P>
   **
   ** @param name the programmatic name of the property, usually
   **             starting with a lowercase letter (e.g. fooManChu
   **             instead of FooManChu).
   ** @param beanClass the class the get and set methods live in.
   ** @exception IntrospectionException if the methods are not found 
   **            or invalid.
   **/
  public PropertyDescriptor(String name, Class beanClass) 
    throws IntrospectionException 
  {
    setName(name);
    if (name.length() == 0) {
      throw new IntrospectionException("empty property name");
    }
    String caps = Character.toUpperCase(name.charAt(0)) + name.substring(1);
    findMethods(beanClass, "is" + caps, "get" + caps, "set" + caps);
    if (getMethod == null) {
      throw new IntrospectionException("Cannot find an is" + caps + 
				       " or get" + caps + " method");
    }
    if (setMethod == null) {
      throw new IntrospectionException("Cannot find a " + caps + " method");
    }
    checkMethods();
  }
  
  /** Create a new PropertyDescriptor by introspection.
   ** This form of constructor allows you to specify the
   ** names of the get and set methods to search for.<P>
   **
   ** <B>Implementation note:</B> If there is a get method (or
   ** boolean isXXX() method), then the return type of that method
   ** is used to find the set method.  If there is no get method,
   ** then the set method is searched for exhaustively.<P>
   **
   ** <B>Spec note:</B>
   ** If there is no get method and multiple set methods with
   ** the same name and a single parameter (different type of course),
   ** then an IntrospectionException is thrown.  While Sun's spec
   ** does not state this, it can make Bean behavior different on
   ** different systems (since method order is not guaranteed) and as
   ** such, can be treated as a bug in the spec.  I am not aware of
   ** whether Sun's implementation catches this.
   **
   ** @param name the programmatic name of the property, usually
   **             starting with a lowercase letter (e.g. fooManChu
   **             instead of FooManChu).
   ** @param beanClass the class the get and set methods live in.
   ** @param getMethodName the name of the get method.
   ** @param setMethodName the name of the set method.
   ** @exception IntrospectionException if the methods are not found 
   **            or invalid.
   **/
  public PropertyDescriptor(String name, Class beanClass, 
			    String getMethodName, String setMethodName) 
    throws IntrospectionException 
  {
    setName(name);
    findMethods(beanClass, getMethodName, null, setMethodName);
    if (getMethod == null && getMethodName != null) {
      throw new IntrospectionException("Cannot find a getter method called " + 
				       getMethodName);
    }
    if (setMethod == null && setMethodName != null) {
      throw new IntrospectionException("Cannot find a setter method called " + 
				       setMethodName);
    }
    checkMethods();
  }
  
  /** Create a new PropertyDescriptor using explicit Methods.
   ** Note that the methods will be checked for conformance to standard
   ** Property method rules, as described above at the top of this class.
   ** 
   ** @param name the programmatic name of the property, usually
   **             starting with a lowercase letter (e.g. fooManChu
   **             instead of FooManChu).
   ** @param getMethod the get method.
   ** @param setMethod the set method.
   ** @exception IntrospectionException if the methods are not found 
   **            or invalid.
   **/
  public PropertyDescriptor(String name, Method getMethod, Method setMethod)
    throws IntrospectionException 
  {
    setName(name);
    this.getMethod = getMethod;
    this.setMethod = setMethod;
    if (getMethod != null) {
      this.propertyType = getMethod.getReturnType();
    } 
    else if (setMethod != null) {
      this.propertyType = setMethod.getParameterTypes()[0];
    }
    checkMethods();
  }
  
  /** Get the property type.
   ** This is the type the get method returns and the set method
   ** takes in.
   **/
  public Class getPropertyType() {
    return propertyType;
  }
  
  /** Get the get method.  Why they call it readMethod here and
   ** get everywhere else is beyond me.
   **/
  public Method getReadMethod() {
    return getMethod;
  }
  
  /** Get the set method.  Why they call it writeMethod here and
   ** set everywhere else is beyond me.
   **/
  public Method getWriteMethod() {
    return setMethod;
  }
  
  /** Get whether the property is bound.  Defaults to false. **/
  public boolean isBound() {
    return bound;
  }
  
  /** Set whether the property is bound.
   ** As long as the the bean implements addPropertyChangeListener() and
   ** removePropertyChangeListener(), setBound(true) may safely be called.<P>
   ** If these things are not true, then the behavior of the system
   ** will be undefined.<P>
   **
   ** When a property is bound, its set method is required to fire the
   ** <CODE>PropertyChangeListener.propertyChange())</CODE> event
   ** after the value has changed.
   ** @param bound whether the property is bound or not.
   **/
  public void setBound(boolean bound) {
    this.bound = bound;
  }
  
  /** Get whether the property is constrained.  Defaults to false. **/
  public boolean isConstrained() {
    return constrained;
  }
  
  /** Set whether the property is constrained.
   ** If the set method throws <CODE>java.beans.PropertyVetoException</CODE>
   ** (or subclass thereof) and the bean implements addVetoableChangeListener()
   ** and removeVetoableChangeListener(), then setConstrained(true) may safely
   ** be called.  Otherwise, the system behavior is undefined.
   ** <B>Spec note:</B> given those strict parameters, it would be nice if it
   ** got set automatically by detection, but oh well.<P>
   ** When a property is constrained, its set method is required to:<P>
   ** <OL>
   ** <LI>Fire the <CODE>VetoableChangeListener.vetoableChange()</CODE>
   **     event notifying others of the change and allowing them a chance to
   **     say it is a bad thing.</LI>
   ** <LI>If any of the listeners throws a PropertyVetoException, then
   **     it must fire another vetoableChange() event notifying the others
   **     of a reversion to the old value (though, of course, the change
   **     was never made).  Then it rethrows the PropertyVetoException and
   **     exits.</LI>
   ** <LI>If all has gone well to this point, the value may be changed.</LI>
   ** </OL>
   ** @param constrained whether the property is constrained or not.
   **/
  public void setConstrained(boolean constrained) {
    this.constrained = constrained;
  }
  
  /** Get the PropertyEditor class.  Defaults to null. **/
  public Class getPropertyEditorClass() {
    return propertyEditorClass;
  }
  
  /** Set the PropertyEditor class.  If the class does not implement
   ** the PropertyEditor interface, you will likely get an exception
   ** late in the game.
   ** @param propertyEditorClass the PropertyEditor class for this 
   **        class to use.
   **/
  public void setPropertyEditorClass(Class propertyEditorClass) {
    this.propertyEditorClass = propertyEditorClass;
  }
  
  private void findMethods(Class beanClass, String getMethodName1, 
			   String getMethodName2, String setMethodName) 
    throws IntrospectionException 
  {
    try {
      // Try the first get method name
      if (getMethodName1 != null) {
	try {
	  getMethod = beanClass.getMethod(getMethodName1, new Class[0]);
	} 
	catch (NoSuchMethodException e) {
	}
      }

      // Fall back to the second get method name
      if (getMethod == null && getMethodName2 != null) {
	try {
	  getMethod = beanClass.getMethod(getMethodName2, new Class[0]);
	} 
	catch (NoSuchMethodException e) {
	}
      }

      // Try the set method name
      if (setMethodName != null) {
	if (getMethod != null) {
	  // If there is a get method, use its return type to help
	  // select the corresponding set method.
	  Class propertyType = getMethod.getReturnType();
	  if (propertyType == Void.TYPE) {
	    String msg = "The property's read method has return type 'void'";
	    throw new IntrospectionException(msg);
	  }
	  
	  Class[] setArgs = new Class[]{propertyType};
	  try {
	    setMethod = beanClass.getMethod(setMethodName, setArgs);
	  } 
	  catch (NoSuchMethodException e) {
	  }
	}
	else if (getMethodName1 == null && getMethodName2 == null) {
	  // If this is a write-only property, choose the first set method
	  // with the required name, one parameter and return type 'void'
	  Method[] methods = beanClass.getMethods();
	  for (int i = 0; i < methods.length; i++) {
	    if (methods[i].getName().equals(setMethodName) &&
		methods[i].getParameterTypes().length == 1 &&
		methods[i].getReturnType() == Void.TYPE) {
	      setMethod = methods[i];
	      break;
	    }
	  }
	}
      }
    } 
    catch (SecurityException e) {
      // FIXME -- shouldn't we just allow SecurityException to propagate?
      String msg = "SecurityException thrown on attempt to access methods.";
      throw new IntrospectionException(msg);
    }
  }

  private void checkMethods() 
    throws IntrospectionException
  {
    if (getMethod != null) {
      if (getMethod.getParameterTypes().length > 0) {
	throw new IntrospectionException("get method has parameters");
      }
      this.propertyType = getMethod.getReturnType();
      if (propertyType == Void.TYPE) {
	throw new IntrospectionException("get method has void return type");
      }
    }
    if (setMethod != null) {
      if (setMethod.getParameterTypes().length != 1) {
	String msg = "set method does not have exactly one parameter"; 
	throw new IntrospectionException(msg);
      }
      if (getMethod == null) {
	propertyType = setMethod.getParameterTypes()[0];
      }
      else {
	if (!propertyType.equals(setMethod.getParameterTypes()[0])) {
	  String msg = "set and get methods do not share the same type";
	  throw new IntrospectionException(msg);
	}
	if ((!getMethod.getDeclaringClass().
	     isAssignableFrom(setMethod.getDeclaringClass())) &&
	    (!setMethod.getDeclaringClass().
	     isAssignableFrom(getMethod.getDeclaringClass()))) {
	  String msg = "set and get methods are not in the same class.";
	  throw new IntrospectionException(msg);
	}
      }
    }
  }
}
