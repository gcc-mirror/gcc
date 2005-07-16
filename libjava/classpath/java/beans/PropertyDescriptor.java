/* java.beans.PropertyDescriptor
   Copyright (C) 1998, 2001, 2004, 2005  Free Software Foundation, Inc.

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

import java.lang.reflect.Method;

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
 ** @author Robert Schuster (thebohemian@gmx.net)
 ** @since 1.1
 ** @status updated to 1.4
 **/

public class PropertyDescriptor extends FeatureDescriptor
{
    Class propertyType;
    Method getMethod;
    Method setMethod;

    Class propertyEditorClass;
    boolean bound;
    boolean constrained;

    PropertyDescriptor(String name)
    {
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
     ** Note that using this constructor the given property must be read- <strong>and</strong>
     ** writeable. If the implementation does not both, a read and a write method, an
     ** <code>IntrospectionException</code> is thrown.
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
        if (name.length() == 0)
        {
            throw new IntrospectionException("empty property name");
        }
        String caps = Character.toUpperCase(name.charAt(0)) + name.substring(1);
        findMethods(beanClass, "is" + caps, "get" + caps, "set" + caps);

        if (getMethod == null)
        {
            throw new IntrospectionException(
                "Cannot find a is" + caps + " or get" + caps + " method");
        }

        if (setMethod == null)
        {
            throw new IntrospectionException(
                "Cannot find a " + caps + " method");
        }

        // finally check the methods compatibility        
        propertyType = checkMethods(getMethod, setMethod);
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
     ** @param getMethodName the name of the get method or <code>null</code> if the property is write-only.
     ** @param setMethodName the name of the set method or <code>null</code> if the property is read-only.
     ** @exception IntrospectionException if the methods are not found 
     **            or invalid.
     **/
    public PropertyDescriptor(
        String name,
        Class beanClass,
        String getMethodName,
        String setMethodName)
        throws IntrospectionException
    {
        setName(name);
        findMethods(beanClass, getMethodName, null, setMethodName);

        if (getMethod == null && getMethodName != null)
        {
            throw new IntrospectionException(
                "Cannot find a getter method called " + getMethodName);
        }

        if (setMethod == null && setMethodName != null)
        {
            throw new IntrospectionException(
                "Cannot find a setter method called " + setMethodName);
        }

        propertyType = checkMethods(getMethod, setMethod);
    }

    /** Create a new PropertyDescriptor using explicit Methods.
     ** Note that the methods will be checked for conformance to standard
     ** Property method rules, as described above at the top of this class.
     **<br>
     ** It is possible to call this method with both <code>Method</code> arguments
     ** being <code>null</code>. In such a case the property type is <code>null</code>.
     ** 
     ** @param name the programmatic name of the property, usually
     **             starting with a lowercase letter (e.g. fooManChu
     **             instead of FooManChu).
     ** @param readMethod the read method or <code>null</code> if the property is write-only.
     ** @param writeMethod the write method or <code>null</code> if the property is read-only.
     ** @exception IntrospectionException if the methods are not found 
     **            or invalid.
     **/
    public PropertyDescriptor(
        String name,
        Method readMethod,
        Method writeMethod)
        throws IntrospectionException
    {
        setName(name);
        getMethod = readMethod;
        setMethod = writeMethod;
        propertyType = checkMethods(getMethod, setMethod);
    }

    /** Get the property type.
     ** This is the type the get method returns and the set method
     ** takes in.
     **/
    public Class getPropertyType()
    {
        return propertyType;
    }

    /** Get the get method.  Why they call it readMethod here and
     ** get everywhere else is beyond me.
     **/
    public Method getReadMethod()
    {
        return getMethod;
    }

    /** Sets the read method.<br/>
     * The read method is used to retrieve the value of a property. A legal
     * read method must have no arguments. Its return type must not be
     * <code>void</code>. If this methods succeeds the property type
     * is adjusted to the return type of the read method.<br/>
     * <br/>
     * It is legal to set the read and the write method to <code>null</code>
     * or provide method which have been declared in distinct classes.
     * 
     * @param readMethod The new method to be used or <code>null</code>.
     * @throws IntrospectionException If the given method is invalid.
     * @since 1.2
     */
    public void setReadMethod(Method readMethod) throws IntrospectionException
    {
        propertyType = checkMethods(readMethod, setMethod);

        getMethod = readMethod;
    }

    /** Get the set method.  Why they call it writeMethod here and
     ** set everywhere else is beyond me.
     **/
    public Method getWriteMethod()
    {
        return setMethod;
    }

    /** Sets the write method.<br/>
     * The write method is used to set the value of a property. A legal write method
     * must have a single argument which can be assigned to the property. If no
     * read method exists the property type changes to the argument type of the
     * write method.<br/>
     * <br/>
     * It is legal to set the read and the write method to <code>null</code>
     * or provide method which have been declared in distinct classes.
     * 
     * @param writeMethod The new method to be used or <code>null</code>.
     * @throws IntrospectionException If the given method is invalid.
     * @since 1.2
     */
    public void setWriteMethod(Method writeMethod)
        throws IntrospectionException
    {
        propertyType = checkMethods(getMethod, writeMethod);

        setMethod = writeMethod;
    }

    /** Get whether the property is bound.  Defaults to false. **/
    public boolean isBound()
    {
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
    public void setBound(boolean bound)
    {
        this.bound = bound;
    }

    /** Get whether the property is constrained.  Defaults to false. **/
    public boolean isConstrained()
    {
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
    public void setConstrained(boolean constrained)
    {
        this.constrained = constrained;
    }

    /** Get the PropertyEditor class.  Defaults to null. **/
    public Class getPropertyEditorClass()
    {
        return propertyEditorClass;
    }

    /** Set the PropertyEditor class.  If the class does not implement
     ** the PropertyEditor interface, you will likely get an exception
     ** late in the game.
     ** @param propertyEditorClass the PropertyEditor class for this 
     **        class to use.
     **/
    public void setPropertyEditorClass(Class propertyEditorClass)
    {
        this.propertyEditorClass = propertyEditorClass;
    }

    private void findMethods(
        Class beanClass,
        String getMethodName1,
        String getMethodName2,
        String setMethodName)
        throws IntrospectionException
    {
        try
        {
            // Try the first get method name
            if (getMethodName1 != null)
            {
                try
                {
                    getMethod =
                        beanClass.getMethod(getMethodName1, new Class[0]);
                }
                catch (NoSuchMethodException e)
                {}
            }

            // Fall back to the second get method name
            if (getMethod == null && getMethodName2 != null)
            {
                try
                {
                    getMethod =
                        beanClass.getMethod(getMethodName2, new Class[0]);
                }
                catch (NoSuchMethodException e)
                {}
            }

            // Try the set method name
            if (setMethodName != null)
            {
                if (getMethod != null)
                {
                    // If there is a get method, use its return type to help
                    // select the corresponding set method.
                    Class propertyType = getMethod.getReturnType();
                    if (propertyType == Void.TYPE)
                    {
                        String msg =
                            "The property's read method has return type 'void'";
                        throw new IntrospectionException(msg);
                    }

                    Class[] setArgs = new Class[] { propertyType };
                    try
                    {
                        setMethod = beanClass.getMethod(setMethodName, setArgs);
                    }
                    catch (NoSuchMethodException e)
                    {}
                }
                else if (getMethodName1 == null && getMethodName2 == null)
                {
                    // If this is a write-only property, choose the first set method
                    // with the required name, one parameter and return type 'void'
                    Method[] methods = beanClass.getMethods();
                    for (int i = 0; i < methods.length; i++)
                    {
                        if (methods[i].getName().equals(setMethodName)
                            && methods[i].getParameterTypes().length == 1
                            && methods[i].getReturnType() == Void.TYPE)
                        {
                            setMethod = methods[i];
                            break;
                        }
                    }
                }
            }
        }
        catch (SecurityException e)
        {
            // FIXME -- shouldn't we just allow SecurityException to propagate?
            String msg =
                "SecurityException thrown on attempt to access methods.";
            throw new IntrospectionException(msg);
        }
    }

    /** Checks whether the given <code>Method</code> instances are legal read and
     * write methods. The following requirements must be met:<br/>
     * <ul>
     * <li>the read method must not have an argument</li>
     * <li>the read method must have a non void return type</li>
     * <li>the read method may not exist</li>
     * <li>the write method must have a single argument</li>
     * <li>the property type and the read method's return type must be assignable from the
     * write method's argument type</li>
     * <li>the write method may not exist</li>
     * </ul>
     * While checking the methods a common new property type is calculated. If the method
     * succeeds this property type is returned.<br/>
     * <br/>
     * For compatibility this has to be noted:<br/>
     * The two methods are allowed to be defined in two distinct classes and may both be null.
     * 
     * @param readMethod The new read method to check.
     * @param writeMethod The new write method to check.
     * @return The common property type of the two method.
     * @throws IntrospectionException If any of the above requirements are not met.
     */
    private Class checkMethods(Method readMethod, Method writeMethod)
        throws IntrospectionException
    {
        Class newPropertyType = propertyType;

        // a valid read method has zero arguments and a non-void return type.
        if (readMethod != null)
        {
            if (readMethod.getParameterTypes().length > 0)
            {
                throw new IntrospectionException("read method has unexpected parameters");
            }

            newPropertyType = readMethod.getReturnType();

            if (newPropertyType == Void.TYPE)
            {
                throw new IntrospectionException("read method return type is void");
            }
        }

        // a valid write method has one argument which can be assigned to the property
        if (writeMethod != null)
        {
            if (writeMethod.getParameterTypes().length != 1)
            {
                String msg = "write method does not have exactly one parameter";
                throw new IntrospectionException(msg);
            }

            if (readMethod == null)
            {
                // changes the property type if there is no read method
                newPropertyType = writeMethod.getParameterTypes()[0];
            }
            else
            {
                // checks whether the write method can be assigned to the return type of the read
                // method (if this is not the case, the methods are not compatible)
                // note: newPropertyType may be null if no methods or method names have been
                // delivered in the constructor.
                if (newPropertyType != null
                    && !newPropertyType.isAssignableFrom(
                        writeMethod.getParameterTypes()[0]))
                {
                    // note: newPropertyType is the same as readMethod.getReturnType() at this point
                    throw new IntrospectionException("read and write method are not compatible");
                }

                /* note: the check whether both method are defined in related classes makes sense but is not
                 * done in the JDK. 
                 * I leave this code here in case someone at Sun decides to add that functionality in later versions (rschuster)
                if ((!readMethod
                    .getDeclaringClass()
                    .isAssignableFrom(writeMethod.getDeclaringClass()))
                    && (!writeMethod
                        .getDeclaringClass()
                        .isAssignableFrom(readMethod.getDeclaringClass())))
                {
                    String msg =
                        "set and get methods are not in the same class.";
                    throw new IntrospectionException(msg);
                }
                */

            }
        }

        return newPropertyType;
    }

    /** Compares this <code>PropertyDescriptor</code> against the
     * given object.
     * Two PropertyDescriptors are equals if
     * <ul>
     * <li>the read methods are equal</li>
     * <li>the write methods are equal</li>
     * <li>the property types are equals</li>
     * <li>the property editor classes are equal</li>
     * <li>the flags (constrained and bound) are equal</li>
     * </ul>
     * @return Whether both objects are equal according to the rules given above.
     * @since 1.4
    */
    public boolean equals(Object o)
    {
        if (o instanceof PropertyDescriptor)
        {
            PropertyDescriptor that = (PropertyDescriptor) o;

            // compares the property types and checks the case where both are null
            boolean samePropertyType =
                (propertyType == null)
                    ? that.propertyType == null
                    : propertyType.equals(that.propertyType);

            // compares the property editor classes and checks the case where both are null
            boolean samePropertyEditorClass =
                (propertyEditorClass == null)
                    ? that.propertyEditorClass == null
                    : propertyEditorClass.equals(that.propertyEditorClass);

            // compares the flags for equality
            boolean sameFlags =
                bound == that.bound && constrained == that.constrained;

            // compares the read methods and checks the case where both are null
            boolean sameReadMethod =
                (getMethod == null)
                    ? that.getMethod == null
                    : getMethod.equals(that.getMethod);

            boolean sameWriteMethod =
                (setMethod == null)
                    ? that.setMethod == null
                    : setMethod.equals(that.setMethod);

            return samePropertyType
                && sameFlags
                && sameReadMethod
                && sameWriteMethod
                && samePropertyEditorClass;
        }
        else
        {
            return false;
        }
        
    }

}
