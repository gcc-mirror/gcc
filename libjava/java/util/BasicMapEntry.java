/* BasicMapEntry.java -- a class providing a plain-vanilla implementation of
   the Map.Entry interface; could be used anywhere in java.util
   Copyright (C) 1998 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.util;

/**
 * a class which implements Map.Entry
 *
 * @author      Jon Zeppieri
 * @version     $Revision: 1.3 $
 * @modified    $Id: BasicMapEntry.java,v 1.3 2000/03/15 21:59:07 rao Exp $
 */
class BasicMapEntry implements Map.Entry
{
    /** the key */
    Object key;
    /** the value */
    Object value;
    
    /**
     * construct a new BasicMapEntry with the given key and value
     *
     * @param     newKey       the key of this Entry
     * @param     newValue     the value of this Entry
     */
    BasicMapEntry(Object newKey, Object newValue)
    {
	key = newKey;
	value = newValue;
    }
    
    /**
     * returns true if <pre>o</pre> is a Map.Entry and 
     * <pre> 
     * (((o.getKey == null) ? (key == null) : 
     * o.getKey().equals(key)) && 
     * ((o.getValue() == null) ? (value == null) : 
     * o.getValue().equals(value)))
     * </pre>
     *
     * NOTE: the calls to getKey() and getValue() in this implementation
     * are <i>NOT</i> superfluous and should not be removed.  They insure 
     * that subclasses such as HashMapEntry work correctly
     *
     * @param      o        the Object being tested for equality
     */
    public boolean equals(Object o)
    {
	Map.Entry tester;
	Object oTestingKey, oTestingValue;
	Object oKey, oValue;
	if (o instanceof Map.Entry)
	    {
		tester = (Map.Entry) o;
		oKey = getKey();
		oValue = getValue();
		oTestingKey = tester.getKey();
		oTestingValue = tester.getValue();
		return (((oTestingKey == null) ? (oKey == null) : 
			 oTestingKey.equals(oKey)) && 
			((oTestingValue == null) ? (oValue == null) : 
			 oTestingValue.equals(oValue)));
	    }
	    return false;
    }

    /** returns the key */
    public Object getKey()
	{
	    return key;
	}
    
    /** returns the value */
    public Object getValue()
    {
	return value;
    }
    
    /** the hashCode() for a Map.Entry is 
     * <pre> 
     * ((getKey() == null) ? 0 : getKey().hashCode()) ^ 
     * ((getValue() == null) ? 0 : getValue().hashCode());
     * </pre>
     *
     * NOTE: the calls to getKey() and getValue() in this implementation
     * are <i>NOT</i> superfluous and should not be removed.  They insure 
     * that subclasses such as HashMapEntry work correctly
     */
    public int hashCode()
    {
	Object oKey = getKey();
	Object oValue = getValue();
	return ((oKey == null) ? 0 : oKey.hashCode()) ^ 
	    ((oValue == null) ? 0 : oValue.hashCode());
    }
    
    /** 
     * sets the value of this Map.Entry 
     *
     * @param     newValue         the new value of this Map.Entry
     */
    public Object setValue(Object newValue)
	throws java.lang.UnsupportedOperationException, ClassCastException, 
	       IllegalArgumentException, NullPointerException
    {
	Object oVal = value;
	value = newValue;
	return oVal;
    }
}
