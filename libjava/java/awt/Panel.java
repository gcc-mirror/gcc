/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

import java.awt.peer.ComponentPeer;

/* This class is complete to 1.2.  */

public class Panel extends Container
{
  public Panel()
  { 
    this (new FlowLayout ());
  }

  public Panel(LayoutManager layout)
  {
    super();
    setLayout (layout);    
  }

  //public AccessibleContext getAccessibleContext()

  public void addNotify()
  {
    if (peer == null)
      peer = getToolkit().createPanel(this);
    super.addNotify();
  }
}
