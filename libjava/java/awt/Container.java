/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/* A very incomplete placeholder. */

public abstract class Container extends Component
{
  int componentCount;
  Component[] components;

  public Component[] getComponents()
  {
    Component[] result = new Component[componentCount];
    if (componentCount > 0)
      System.arraycopy(components, 0, result, 0, componentCount);
    return result;
  }

  public Component getComponent (int n)
  {
    if (n < 0 || n >= componentCount)
      throw new ArrayIndexOutOfBoundsException("no such component");
    return components[n];
  }

  public boolean isAncestorOf (Component comp)
  {
    for (;;)
      {
	if (comp == null)
	  return false;
	if (comp == this)
	  return true;
	comp = comp.getParent();
      }
  }

  public Component add (String name, Component comp)
  {
    /* FIXME */
    return comp;
  }

  public void addNotify ()
  {
    for (int i = componentCount;  --i >= 0; )
      components[i].addNotify();
  }

  public void setLayout (LayoutManager layout)
  { /* FIXME */ }
}
