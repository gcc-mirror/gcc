/* LayoutManager.java -- Layout containers in a Window
   Copyright (C) 1999 Free Software Foundation, Inc.

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


package java.awt;

/**
  * This interface is for laying out containers.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface LayoutManager
{

/**
  * Adds the specified component to the layout group.
  *
  * @param name The name of the component to add.
  * @param component The component to add.
  */
public abstract void
addLayoutComponent(String name, Component component);

/*************************************************************************/

/**
  * Removes the specified component from the layout group.
  *
  * @param component The component to remove.
  */
public abstract void
removeLayoutComponent(Component component);

/*************************************************************************/

/**
  * Calculates the preferred size for this container, taking into account
  * the components in the specified parent container.
  *
  * @param parent The parent container.
  *
  * @return The preferred dimensions of this container.
  */
public abstract Dimension
preferredLayoutSize(Container parent);

/*************************************************************************/

/**
  * Calculates the minimum size for this container, taking into account
  * the components in the specified parent container.
  *
  * @param parent The parent container.
  *
  * @return The minimum dimensions of this container.
  */
public abstract Dimension
minimumLayoutSize(Container parent);

/*************************************************************************/

/**
  * Lays out the components in this container on the specified parent
  * container.
  *
  * @param parent The parent container.
  */
public abstract void
layoutContainer(Container parent);

} // interface LayoutManager

