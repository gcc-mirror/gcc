/* NodeWriter - Writes and exports preferences nodes to files
   Copyright (C) 2001 Free Software Foundation, Inc.

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

package gnu.java.util.prefs;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;

import java.util.StringTokenizer;

import java.util.prefs.*;

/**
 * Writes and exports preferences nodes to files
 *
 * @author Mark Wielaard (mark@klomp.org)
 */
public class NodeWriter {

    /** The Preferences node to write. */
    private final Preferences prefs;

    /** The bufferedWriter to write the node to. */
    private final BufferedWriter bw;

    /**
     * True if the complete sub tree should be written,
     * false if only the node should be written.
     */
    private boolean subtree;

    /**
     * Creates a new NodeWriter for the given preferences node and writer.
     */
    public NodeWriter(Preferences prefs, Writer w) {
        this.prefs = prefs;
        if (w instanceof BufferedWriter) {
            this.bw = (BufferedWriter) w;
        } else {
            this.bw = new BufferedWriter(w);
        }
    }

    /**
     * Creates a new NodeWriter for the given preferences node and
     * outputstream. Creates a new OutputStreamWriter.
     */
    public NodeWriter(Preferences prefs, OutputStream os) {
        this(prefs, new OutputStreamWriter(os));
    }

    /**
     * Writes the preference node plus the complete subtree.
     */
    public void writePrefsTree() throws BackingStoreException, IOException {
        subtree = true;
        writeHeader();
        writePreferences();
        bw.flush();
    }

    /**
     * Writes only the preference node.
     */
    public void writePrefs() throws BackingStoreException, IOException {
        subtree = false;
        writeHeader();
        writePreferences();
        bw.flush();
    }

    /**
     * Writes the standard header.
     */
    private void writeHeader() throws BackingStoreException, IOException {
        bw.write("<?xml version=\"1.0\"?>");
        bw.newLine();
        bw.newLine();
        bw.write("<!-- GNU Classpath java.util.prefs Preferences ");

        if (prefs.isUserNode()) {
            bw.write("user");
        } else {
            bw.write("system");
        }

        // root node?
        if (prefs.parent() == null) {
            bw.write(" root");
        }

        if (subtree) {
            bw.write(" tree");
        } else {
            bw.write(" node");
        }

        // no root?
        if (prefs.parent() != null) {
            bw.newLine();
            bw.write("     '");
            bw.write(prefs.absolutePath());
            bw.write('\'');
            bw.newLine();
        }
        bw.write(" -->");
        bw.newLine();
        bw.newLine();
    }

    /**
     * Write the preferences tag and the root.
     */
    private void writePreferences() throws BackingStoreException, IOException {
        bw.write("<preferences>");
        bw.newLine();
        writeRoot();
        bw.write("</preferences>");
        bw.newLine();
    }

    private void writeRoot() throws BackingStoreException, IOException {
        bw.write("  <root type=\"");
        if (prefs.isUserNode()) {
            bw.write("user");
        } else {
            bw.write("system");
        }
        bw.write("\"/>");

        writeRootMap();
        writeNode();

        bw.write("  </root>");
        bw.newLine();
    }

    private void writeRootMap() throws BackingStoreException, IOException {
        // Is it a root node?
        if(prefs.parent() == null && prefs.keys().length > 0) {
            bw.newLine();
            writeMap(prefs, 2);
        } else {
            bw.write("<map/>");
            bw.newLine();
        }
    }

    /**
     * Writes all the parents of the preferences node without any entries.
     * Returns the number of parents written, which has to be used as
     * argument to <code>writeCloseParents()</code> after writing the node
     * itself.
     */
    private int writeParents() throws IOException {
        int parents;
        String path = prefs.absolutePath();
        int lastslash = path.lastIndexOf("/");
        if (lastslash > 0) {
            path = path.substring(1, lastslash);
            StringTokenizer st = new StringTokenizer(path);
            parents = st.countTokens();

            for (int i=0; i<parents; i++) {
                String name = st.nextToken();
                indent(i+2);
                bw.write("<node name=\"" + name + "\">");
                bw.write("<map/>");
                bw.write("</node>");
                bw.newLine();
            }
        } else {
            parents = 0;
        }

        return parents;
    }

    private void writeCloseParents(int parents) throws IOException {
        while(parents > 0) {
            indent(parents+1);
            bw.write("</node>");
            bw.newLine();
            parents--;
        }
    }

    private void writeNode() throws BackingStoreException, IOException {
        int parents = writeParents();
        // root?
        int indent;
        if (prefs.parent() == null) {
            indent = parents+1;
        } else {
            indent = parents+2;
        }
        writeNode(prefs, indent);
        writeCloseParents(parents);
    }

    private void writeNode(Preferences node, int indent)
                                    throws BackingStoreException, IOException
    {
        // not root?
        if (node.parent() != null) {
            indent(indent);
            bw.write("<node name=\"" + node.name() + "\">");
            if (node.keys().length > 0) {
                bw.newLine();
            }
            writeMap(node, indent+1);
        }

        if (subtree) {
            String[] children = node.childrenNames();
            for (int i=0; i<children.length; i++) {
                Preferences child = node.node(children[i]);
                writeNode(child, indent+1);
            }
        }

        // not root?
        if (node.parent() != null) {
            indent(indent);
            bw.write("</node>");
            bw.newLine();
        }
    }

    private void writeMap(Preferences node, int indent) 
                                    throws BackingStoreException, IOException
    {
        // construct String used for indentation
        StringBuffer indentBuffer = new StringBuffer(2*indent);
        for (int i=0; i < indent; i++)
            indentBuffer.append("  ");
        String indentString = indentBuffer.toString();

        if (node.keys().length > 0) {
            bw.write(indentString);
            bw.write("<map>");
            bw.newLine();
            writeEntries(node, indentString + "  ");
            bw.write(indentString);
            bw.write("</map>");
        } else {
            bw.write("<map/>");
        }
        bw.newLine();
    }

    private void writeEntries(Preferences node, String indent)
                                    throws BackingStoreException, IOException
    {
        String[] keys = node.keys();
        for(int i = 0; i < keys.length; i++) {
            String value = node.get(keys[i], null);
            if (value == null) {
                throw new BackingStoreException("null value for key '"
                                                + keys[i] + "'");
            }

            bw.write(indent);
            bw.write("<entry key=\"" + keys[i] + "\""
                    + " value=\"" + value + "\"/>");
            bw.newLine();
        }
    }

    private void indent(int x) throws IOException {
        for (int i=0; i<x; i++) {
            bw.write("  ");
        }
    }
}
