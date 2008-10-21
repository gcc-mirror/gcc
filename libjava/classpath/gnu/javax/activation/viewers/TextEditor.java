/* TextEditor.java -- Simple text editor component.
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

package gnu.javax.activation.viewers;

import java.awt.Dimension;
import java.awt.TextArea;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import javax.activation.CommandObject;
import javax.activation.DataHandler;

/**
 * Simple text editor component.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 * @version 1.0.2
 */
public class TextEditor extends TextArea
    implements CommandObject, ActionListener
{

    private transient DataHandler dh;

    public TextEditor()
    {
        super("", 24, 80, 1);
    }

    public Dimension getPreferredSize()
    {
        return getMinimumSize(24, 80);
    }

    public void setCommandContext(String verb, DataHandler dh)
        throws IOException
    {
        this.dh = dh;
        InputStream in = dh.getInputStream();
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        byte[] buf = new byte[4096];
        for (int len = in.read(buf); len != -1; len = in.read(buf))
            bytes.write(buf, 0, len);
        in.close();
        setText(bytes.toString());
    }

    public void actionPerformed(ActionEvent event)
    {
        if ("save".equals(event.getActionCommand()) && dh != null)
        {
            OutputStream out = null;
            try
            {
                out = dh.getOutputStream();
                if (out != null)
                    out.write(getText().getBytes());
            }
            catch (IOException e)
            {
                e.printStackTrace(System.err);
            }
            finally
            {
                if (out != null)
                {
                    try
                    {
                        
                        out.close();
                    }
                    catch (IOException e)
                    {
                        e.printStackTrace(System.err);
                    }
                }
            }
        }
    }
    
}
