/* JdwpIdFactory.java -- factory for generating type and object IDs
   Copyright (C) 2005 Free Software Foundation

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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp.id;

import java.util.HashMap;

/**
 * This factory generates ids for objects and types that may
 * be sent to a debugger.
 *
 * @author Keith Seitz  (keiths@redhat.com)
 */
public class JdwpIdFactory
{
  // ID of last object / referencetype
  private static Object _idLock = new Object ();
  private static Object _ridLock = new Object ();
  private static long _lastId = 0;
  private static long _lastRid = 0;

  // A list of all ID types
  private static HashMap _idList = new HashMap ();

  // Initialize the id list with known types
  static
  {
    // ObjectId and ArrayId are special cases. See newId.
    _idList.put (ClassLoaderId.typeClass, ClassLoaderId.class);
    _idList.put (ClassObjectId.typeClass, ClassObjectId.class);
    //_idList.put (FieldId.typeClass, FieldId.class);
    //_idList.put (FrameId.typeClass, FrameId.class);
    //_idList.put (MethodId.typeClass, MethodId.class);
    _idList.put (StringId.typeClass, StringId.class);
    _idList.put (ThreadId.typeClass, ThreadId.class);
    _idList.put (ThreadGroupId.typeClass, ThreadGroupId.class);
  }

  /**
   * Returns a new id for the given object
   *
   * @param object  the object for which an id is desired
   * @returns a suitable object id
   */
  public static JdwpId newId (Object object)
  {
    JdwpId id = null;
    
    // Special case: arrays
    if (object.getClass ().isArray ())
	id = new ArrayId ();
    else
      {
	// Loop through all classes until we hit baseclass
	Class myClass;
	for (myClass = object.getClass (); myClass != null;
	     myClass = myClass.getSuperclass ())
	  {
	    Class clz = (Class) _idList.get (myClass);
	    if (clz != null)
	      {
		try
		  {
		    id = (JdwpId) clz.newInstance ();
		    synchronized (_idLock)
		      {
			id.setId (++_lastId);
		      }
		    return id;
		  }
		catch (InstantiationException ie)
		  {
		    // This really should not happen
		    throw new RuntimeException ("cannot create new ID", ie);
		  }
		catch (IllegalAccessException iae)
		  {
		    // This really should not happen
		    throw new RuntimeException ("illegal access of ID", iae);
		  }
	      }
	  }

	/* getSuperclass returned null and no matching ID type found.
	   So it must derive from Object. */
	id = new ObjectId ();
      }

    synchronized (_idLock)
      {
	id.setId (++_lastId);
      }

    return id;
  }

  /**
   * Returns a new reference type id for the given class
   *
   * @param clazz  the <code>Class</code> for which an id is desired
   * @returns a suitable reference type id or <code>null</code>
   */
  public static ReferenceTypeId newReferenceTypeId (Class clazz)
  {
    ReferenceTypeId id = null;
    try
      {
	if (clazz.isArray ())
	  id = new ArrayReferenceTypeId ();
	else if (clazz.isInterface ())
	  id = new InterfaceReferenceTypeId ();
	else
	  id = new ClassReferenceTypeId ();
	synchronized (_ridLock)
	  {
	    id.setId (++_lastRid);
	  }
	return id;
      }
    catch (InstantiationException ie)
      {
	return null;
      }
    catch (IllegalAccessException iae)
      {
	return null;
      }
  }
}
