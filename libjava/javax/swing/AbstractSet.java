/* AbstractSet.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

package javax.swing;

import java.util.AbstractCollection;
import java.util.Collection;
import java.util.Set;

/**
 * Empty
 *
 * @author Ronald Veldema (rveldema@cs.vu.nl)
 */
public abstract class AbstractSet extends AbstractCollection implements Set
{
	boolean contained(Object []a1, Object b)
	{
		for (int i=0;i<a1.length;i++)
		{
			if (a1[i] == b)
				return true;
		}
		return false;
	}

	public boolean equals(Object o)
	{
		if (! (o instanceof AbstractSet))
			return false;
		AbstractSet s = (AbstractSet) o;

		if (s == this)
			return true;

		if (s.size() != size())
			return false;

		Object[] a1 = s.toArray();
		Object[] a2 = toArray();

		for (int i=0;i<a1.length;i++)
		{
			if (! contained(a2, a1[i]))
				return false;
		}
		return true;
	}

	public int hashCode()
	{
		int hash = 0;
		Object[] a1 = toArray();

		if (a1 == null)
			return 0;

		for (int i=0; i<a1.length; i++)
		{
			hash += a1[i].hashCode();
		}
		return hash;
	}

	public boolean removeAll(Collection c)
	{
		return false;
	}
}
