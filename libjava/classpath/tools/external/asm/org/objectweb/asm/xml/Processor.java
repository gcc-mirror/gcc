/***
 * ASM XML Adapter
 * Copyright (c) 2004, Eugene Kuleshov
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.objectweb.asm.xml;

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.sax.SAXTransformerFactory;
import javax.xml.transform.sax.TransformerHandler;
import javax.xml.transform.stream.StreamSource;

import org.objectweb.asm.ClassReader;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.helpers.AttributesImpl;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * Processor is a command line tool that can be used for bytecode waving
 * directed by XSL transformation. <p> In order to use a concrete XSLT engine,
 * system property <tt>javax.xml.transform.TransformerFactory</tt> must be set
 * to one of the following values.
 * 
 * <blockquote> <table border="1" cellspacing="0" cellpadding="3"> <tr> <td>jd.xslt</td>
 * <td>jd.xml.xslt.trax.TransformerFactoryImpl</td> </tr>
 * 
 * <tr> <td>Saxon</td> <td>net.sf.saxon.TransformerFactoryImpl</td> </tr>
 * 
 * <tr> <td>Caucho</td> <td>com.caucho.xsl.Xsl</td> </tr>
 * 
 * <tr> <td>Xalan interpeter</td> <td>org.apache.xalan.processor.TransformerFactory</td>
 * </tr>
 * 
 * <tr> <td>Xalan xsltc</td> <td>org.apache.xalan.xsltc.trax.TransformerFactoryImpl</td>
 * </tr> </table> </blockquote>
 * 
 * @author Eugene Kuleshov
 */
public class Processor {

    public static final int BYTECODE = 1;

    public static final int MULTI_XML = 2;

    public static final int SINGLE_XML = 3;

    private static final String SINGLE_XML_NAME = "classes.xml";

    private int inRepresentation;

    private int outRepresentation;

    private InputStream input = null;

    private OutputStream output = null;

    private Source xslt = null;

    private boolean computeMax;

    private int n = 0;

    public Processor(
        int inRepresenation,
        int outRepresentation,
        InputStream input,
        OutputStream output,
        Source xslt)
    {
        this.inRepresentation = inRepresenation;
        this.outRepresentation = outRepresentation;
        this.input = input;
        this.output = output;
        this.xslt = xslt;
        this.computeMax = true;
    }

    public int process() throws TransformerException, IOException, SAXException
    {
        ZipInputStream zis = new ZipInputStream(input);
        final ZipOutputStream zos = new ZipOutputStream(output);
        final OutputStreamWriter osw = new OutputStreamWriter(zos);

        Thread.currentThread()
                .setContextClassLoader(getClass().getClassLoader());

        TransformerFactory tf = TransformerFactory.newInstance();
        if (!tf.getFeature(SAXSource.FEATURE)
                || !tf.getFeature(SAXResult.FEATURE))
            return 0;

        SAXTransformerFactory saxtf = (SAXTransformerFactory) tf;
        Templates templates = null;
        if (xslt != null) {
            templates = saxtf.newTemplates(xslt);
        }

        // configuring outHandlerFactory
        // ///////////////////////////////////////////////////////

        EntryElement entryElement = getEntryElement(zos);

        ContentHandler outDocHandler = null;
        switch (outRepresentation) {
            case BYTECODE:
                outDocHandler = new OutputSlicingHandler(new ASMContentHandlerFactory(zos,
                        computeMax),
                        entryElement,
                        false);
                break;

            case MULTI_XML:
                outDocHandler = new OutputSlicingHandler(new SAXWriterFactory(osw,
                        true),
                        entryElement,
                        true);
                break;

            case SINGLE_XML:
                ZipEntry outputEntry = new ZipEntry(SINGLE_XML_NAME);
                zos.putNextEntry(outputEntry);
                outDocHandler = new SAXWriter(osw, false);
                break;

        }

        // configuring inputDocHandlerFactory
        // /////////////////////////////////////////////////
        ContentHandler inDocHandler = null;
        if (templates == null) {
            inDocHandler = outDocHandler;
        } else {
            inDocHandler = new InputSlicingHandler("class",
                    outDocHandler,
                    new TransformerHandlerFactory(saxtf,
                            templates,
                            outDocHandler));
        }
        ContentHandlerFactory inDocHandlerFactory = new SubdocumentHandlerFactory(inDocHandler);

        if (inDocHandler != null && inRepresentation != SINGLE_XML) {
            inDocHandler.startDocument();
            inDocHandler.startElement("",
                    "classes",
                    "classes",
                    new AttributesImpl());
        }

        int i = 0;
        ZipEntry ze = null;
        while ((ze = zis.getNextEntry()) != null) {
            update(ze.getName(), n++);
            if (isClassEntry(ze)) {
                processEntry(zis, ze, inDocHandlerFactory);
            } else {
                OutputStream os = entryElement.openEntry(getName(ze));
                copyEntry(zis, os);
                entryElement.closeEntry();
            }

            i++;
        }

        if (inDocHandler != null && inRepresentation != SINGLE_XML) {
            inDocHandler.endElement("", "classes", "classes");
            inDocHandler.endDocument();
        }

        if (outRepresentation == SINGLE_XML) {
            zos.closeEntry();
        }
        zos.flush();
        zos.close();

        return i;
    }

    private void copyEntry(InputStream is, OutputStream os) throws IOException {
        if (outRepresentation == SINGLE_XML)
            return;

        byte[] buff = new byte[2048];
        int i;
        while ((i = is.read(buff)) != -1) {
            os.write(buff, 0, i);
        }
    }

    private boolean isClassEntry(ZipEntry ze) {
        String name = ze.getName();
        return inRepresentation == SINGLE_XML && name.equals(SINGLE_XML_NAME)
                || name.endsWith(".class") || name.endsWith(".class.xml");
    }

    private void processEntry(
        final ZipInputStream zis,
        ZipEntry ze,
        ContentHandlerFactory handlerFactory)
    {
        ContentHandler handler = handlerFactory.createContentHandler();
        try {

            // if (CODE2ASM.equals(command)) { // read bytecode and process it
            // // with TraceClassVisitor
            // ClassReader cr = new ClassReader(readEntry(zis, ze));
            // cr.accept(new TraceClassVisitor(null, new PrintWriter(os)),
            // false);
            // }

            boolean singleInputDocument = inRepresentation == SINGLE_XML;
            if (inRepresentation == BYTECODE) { // read bytecode and process it
                // with handler
                ClassReader cr = new ClassReader(readEntry(zis, ze));
                cr.accept(new SAXClassAdapter(handler, singleInputDocument),
                        false);

            } else { // read XML and process it with handler
                XMLReader reader = XMLReaderFactory.createXMLReader();
                reader.setContentHandler(handler);
                reader.parse(new InputSource(singleInputDocument
                        ? (InputStream) new ProtectedInputStream(zis)
                        : new ByteArrayInputStream(readEntry(zis, ze))));

            }
        } catch (Exception ex) {
            update(ze.getName(), 0);
            update(ex, 0);
        }
    }

    private EntryElement getEntryElement(ZipOutputStream zos) {
        if (outRepresentation == SINGLE_XML) {
            return new SingleDocElement(zos);
        }
        return new ZipEntryElement(zos);
    }

    // private ContentHandlerFactory getHandlerFactory(
    // OutputStream os,
    // SAXTransformerFactory saxtf,
    // Templates templates)
    // {
    // ContentHandlerFactory factory = null;
    // if (templates == null) {
    // if (outputRepresentation == BYTECODE) { // factory used to write
    // // bytecode
    // factory = new ASMContentHandlerFactory(os, computeMax);
    // } else { // factory used to write XML
    // factory = new SAXWriterFactory(os, true);
    // }
    // } else {
    // if (outputRepresentation == BYTECODE) { // factory used to transform
    // // and then write bytecode
    // factory = new ASMTransformerHandlerFactory(saxtf,
    // templates,
    // os,
    // computeMax);
    // } else { // factory used to transformand then write XML
    // factory = new TransformerHandlerFactory(saxtf,
    // templates,
    // os,
    // outputRepresentation == SINGLE_XML);
    // }
    // }
    // return factory;
    // }

    private String getName(ZipEntry ze) {
        String name = ze.getName();
        if (isClassEntry(ze)) {
            if (inRepresentation != BYTECODE && outRepresentation == BYTECODE) {
                name = name.substring(0, name.length() - 4); // .class.xml to
                // .class
            } else if (inRepresentation == BYTECODE
                    && outRepresentation != BYTECODE)
            {
                name = name.concat(".xml"); // .class to .class.xml
            }
            // } else if( CODE2ASM.equals( command)) {
            // name = name.substring( 0, name.length()-6).concat( ".asm");
        }
        return name;
    }

    private byte[] readEntry(ZipInputStream zis, ZipEntry ze)
            throws IOException
    {
        long size = ze.getSize();
        if (size > -1) {
            byte[] buff = new byte[(int) size];
            int k = 0;
            int n;
            while(( n = zis.read(buff, k, buff.length-k)) > 0) {
              k += n;
            }
            return buff;
        }

        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        byte[] buff = new byte[4096];
        int i;
        while ((i = zis.read(buff)) != -1) {
            bos.write(buff, 0, i);
        }
        return bos.toByteArray();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
     */
    protected void update(Object arg, int n) {
        if (arg instanceof Throwable) {
            ((Throwable) arg).printStackTrace();
        } else {
            if ((n % 100) == 0) {
                System.err.println(n + " " + arg);
            }
        }
    }

    public static void main(String[] args) throws Exception {
        if (args.length < 2) {
            showUsage();
            return;
        }

        int inRepresentation = getRepresentation(args[0]);
        int outRepresentation = getRepresentation(args[1]);

        InputStream is = System.in;
        OutputStream os = new BufferedOutputStream(System.out);

        Source xslt = null;
        // boolean computeMax = true;

        for (int i = 2; i < args.length; i++) {
            if ("-in".equals(args[i])) {
                is = new FileInputStream(args[++i]);

            } else if ("-out".equals(args[i])) {
                os = new BufferedOutputStream(new FileOutputStream(args[++i]));

            } else if ("-xslt".equals(args[i])) {
                xslt = new StreamSource(new FileInputStream(args[++i]));

                // } else if( "-computemax".equals( args[ i].toLowerCase())) {
                // computeMax = true;

            } else {
                showUsage();
                return;

            }
        }

        if (inRepresentation == 0 || outRepresentation == 0) {
            showUsage();
            return;
        }

        Processor m = new Processor(inRepresentation,
                outRepresentation,
                is,
                os,
                xslt);

        long l1 = System.currentTimeMillis();
        int n = m.process();
        long l2 = System.currentTimeMillis();
        System.err.println(n);
        System.err.println("" + (l2 - l1) + "ms  " + (1000f * n / (l2 - l1))
                + " resources/sec");
    }

    private static int getRepresentation(String s) {
        if ("code".equals(s)) {
            return BYTECODE;
        } else if ("xml".equals(s)) {
            return MULTI_XML;
        } else if ("singlexml".equals(s)) {
            return SINGLE_XML;
        }
        return 0;
    }

    private static void showUsage() {
        System.err.println("Usage: Main <in format> <out format> [-in <input jar>] [-out <output jar>] [-xslt <xslt file>]");
        System.err.println("  when -in or -out is omitted sysin and sysout would be used");
        System.err.println("  <in format> and <out format> - code | xml | singlexml");
    }

    /**
     * IputStream wrapper class used to protect input streams from being closed
     * by some stupid XML parsers.
     */
    private static final class ProtectedInputStream extends InputStream {
        private final InputStream is;

        private ProtectedInputStream(InputStream is) {
            super();
            this.is = is;
        }

        public final void close() throws IOException {
        }

        public final int read() throws IOException {
            return is.read();
        }

        public final int read(byte[] b, int off, int len) throws IOException {
            return is.read(b, off, len);
        }

        public final int available() throws IOException {
            return is.available();
        }
    }

    /**
     * A {@link ContentHandlerFactory ContentHandlerFactory} is used to create
     * {@link org.xml.sax.ContentHandler ContentHandler} instances for concrete
     * context.
     */
    private static interface ContentHandlerFactory {

        /**
         * Creates an instance of the content handler.
         * 
         * @return content handler
         */
        ContentHandler createContentHandler();

    }

    /**
     * SAXWriterFactory
     */
    private static final class SAXWriterFactory implements
            ContentHandlerFactory
    {
        private Writer w;

        private boolean optimizeEmptyElements;

        public SAXWriterFactory(Writer w, boolean optimizeEmptyElements) {
            this.w = w;
            this.optimizeEmptyElements = optimizeEmptyElements;
        }

        public final ContentHandler createContentHandler() {
            return new SAXWriter(w, optimizeEmptyElements);
        }

    }

    /**
     * ASMContentHandlerFactory
     */
    private static final class ASMContentHandlerFactory implements
            ContentHandlerFactory
    {
        private OutputStream os;

        private boolean computeMax;

        public ASMContentHandlerFactory(OutputStream os, boolean computeMax) {
            this.os = os;
            this.computeMax = computeMax;
        }

        public final ContentHandler createContentHandler() {
            return new ASMContentHandler(os, computeMax);
        }

    }

    /**
     * TransformerHandlerFactory
     */
    private static final class TransformerHandlerFactory implements
            ContentHandlerFactory
    {
        private SAXTransformerFactory saxtf;

        private Templates templates;

        private ContentHandler outputHandler;

        public TransformerHandlerFactory(
            SAXTransformerFactory saxtf,
            Templates templates,
            ContentHandler outputHandler)
        {
            this.saxtf = saxtf;
            this.templates = templates;
            this.outputHandler = outputHandler;
        }

        public final ContentHandler createContentHandler() {
            try {
                TransformerHandler handler = saxtf.newTransformerHandler(templates);
                handler.setResult(new SAXResult(outputHandler));
                return handler;
            } catch (TransformerConfigurationException ex) {
                throw new RuntimeException(ex.toString());
            }
        }
    }

    /**
     * SubdocumentHandlerFactory
     */
    private final static class SubdocumentHandlerFactory implements
            ContentHandlerFactory
    {
        private ContentHandler subdocumentHandler;

        public SubdocumentHandlerFactory(ContentHandler subdocumentHandler) {
            this.subdocumentHandler = subdocumentHandler;
        }

        public final ContentHandler createContentHandler() {
            return subdocumentHandler;
        }

    }

    /**
     * A {@link org.xml.sax.ContentHandler ContentHandler} and
     * {@link org.xml.sax.ext.LexicalHandler LexicalHandler} that serializes XML
     * from SAX 2.0 events into {@link java.io.Writer Writer}.
     * 
     * <i><blockquote> This implementation does not support namespaces, entity
     * definitions (uncluding DTD), CDATA and text elements. </blockquote></i>
     */
    private final static class SAXWriter extends DefaultHandler implements
            LexicalHandler
    {
        private static final char[] OFF = "                                                                                                        ".toCharArray();

        private Writer w;

        private boolean optimizeEmptyElements;

        private boolean openElement = false;

        private int ident = 0;

        /**
         * Creates <code>SAXWriter</code>.
         * 
         * @param w writer
         * @param optimizeEmptyElements if set to <code>true</code>, short
         *        XML syntax will be used for empty elements
         */
        public SAXWriter(Writer w, boolean optimizeEmptyElements) {
            this.w = w;
            this.optimizeEmptyElements = optimizeEmptyElements;
        }

        public final void startElement(
            String ns,
            String localName,
            String qName,
            Attributes atts) throws SAXException
        {
            try {
                closeElement();

                writeIdent();
                w.write("<".concat(qName));
                if (atts != null && atts.getLength() > 0)
                    writeAttributes(atts);

                if (!optimizeEmptyElements) {
                    w.write(">\n");
                } else {
                    openElement = true;
                }
                ident += 2;

            } catch (IOException ex) {
                throw new SAXException(ex);

            }
        }

        public final void endElement(String ns, String localName, String qName)
                throws SAXException
        {
            ident -= 2;
            try {
                if (openElement) {
                    w.write("/>\n");
                    openElement = false;
                } else {
                    writeIdent();
                    w.write("</" + qName + ">\n");
                }

            } catch (IOException ex) {
                throw new SAXException(ex);

            }
        }

        public final void endDocument() throws SAXException {
            try {
                w.flush();

            } catch (IOException ex) {
                throw new SAXException(ex);

            }
        }

        public final void comment(char[] ch, int off, int len)
                throws SAXException
        {
            try {
                closeElement();

                writeIdent();
                w.write("<!-- ");
                w.write(ch, off, len);
                w.write(" -->\n");

            } catch (IOException ex) {
                throw new SAXException(ex);

            }
        }

        public final void startDTD(String arg0, String arg1, String arg2)
                throws SAXException
        {
        }

        public final void endDTD() throws SAXException {
        }

        public final void startEntity(String arg0) throws SAXException {
        }

        public final void endEntity(String arg0) throws SAXException {
        }

        public final void startCDATA() throws SAXException {
        }

        public final void endCDATA() throws SAXException {
        }

        private final void writeAttributes(Attributes atts) throws IOException {
            StringBuffer sb = new StringBuffer();
            int len = atts.getLength();
            for (int i = 0; i < len; i++) {
                sb.append(" ")
                        .append(atts.getLocalName(i))
                        .append("=\"")
                        .append(esc(atts.getValue(i)))
                        .append("\"");
            }
            w.write(sb.toString());
        }

        /**
         * Encode string with escaping.
         * 
         * @param str string to encode.
         * @return encoded string
         */
        private final String esc(String str) {
            StringBuffer sb = new StringBuffer(str.length());
            for (int i = 0; i < str.length(); i++) {
                char ch = str.charAt(i);
                switch (ch) {
                    case '&':
                        sb.append("&amp;");
                        break;

                    case '<':
                        sb.append("&lt;");
                        break;

                    case '>':
                        sb.append("&gt;");
                        break;

                    case '\"':
                        sb.append("&quot;");
                        break;

                    default:
                        if (ch > 0x7f) {
                            sb.append("&#")
                                    .append(Integer.toString(ch))
                                    .append(';');
                        } else {
                            sb.append(ch);
                        }

                }
            }
            return sb.toString();
        }

        private final void writeIdent() throws IOException {
            int n = ident;
            while (n > 0) {
                if (n > OFF.length) {
                    w.write(OFF);
                    n -= OFF.length;
                } else {
                    w.write(OFF, 0, n);
                    n = 0;
                }
            }
        }

        private final void closeElement() throws IOException {
            if (openElement) {
                w.write(">\n");
            }
            openElement = false;
        }

    }

    /**
     * A {@link org.xml.sax.ContentHandler ContentHandler} that splits XML
     * documents into smaller chunks. Each chunk is processed by the nested
     * {@link org.xml.sax.ContentHandler ContentHandler} obtained from
     * {@link java.net.ContentHandlerFactory ContentHandlerFactory}. This is
     * useful for running XSLT engine against large XML document that will
     * hardly fit into the memory all together. <p> TODO use complete path for
     * subdocumentRoot
     */
    private final static class InputSlicingHandler extends DefaultHandler {
        private String subdocumentRoot;

        private ContentHandler rootHandler;

        private ContentHandlerFactory subdocumentHandlerFactory;

        private boolean subdocument = false;

        private ContentHandler subdocumentHandler;

        /**
         * Constructs a new {@link InputSlicingHandler SubdocumentHandler}
         * object.
         * 
         * @param subdocumentRoot name/path to the root element of the
         *        subdocument
         * @param rootHandler content handler for the entire document
         *        (subdocument envelope).
         * @param subdocumentHandlerFactory a
         *        {@link ContentHandlerFactory ContentHandlerFactory} used to
         *        create {@link ContentHandler ContentHandler} instances for
         *        subdocuments.
         */
        public InputSlicingHandler(
            String subdocumentRoot,
            ContentHandler rootHandler,
            ContentHandlerFactory subdocumentHandlerFactory)
        {
            this.subdocumentRoot = subdocumentRoot;
            this.rootHandler = rootHandler;
            this.subdocumentHandlerFactory = subdocumentHandlerFactory;
        }

        public final void startElement(
            String namespaceURI,
            String localName,
            String qName,
            Attributes list) throws SAXException
        {
            if (subdocument) {
                subdocumentHandler.startElement(namespaceURI,
                        localName,
                        qName,
                        list);
            } else if (localName.equals(subdocumentRoot)) {
                subdocumentHandler = subdocumentHandlerFactory.createContentHandler();
                subdocumentHandler.startDocument();
                subdocumentHandler.startElement(namespaceURI,
                        localName,
                        qName,
                        list);
                subdocument = true;
            } else if (rootHandler != null) {
                rootHandler.startElement(namespaceURI, localName, qName, list);
            }
        }

        public final void endElement(
            String namespaceURI,
            String localName,
            String qName) throws SAXException
        {
            if (subdocument) {
                subdocumentHandler.endElement(namespaceURI, localName, qName);
                if (localName.equals(subdocumentRoot)) {
                    subdocumentHandler.endDocument();
                    subdocument = false;
                }
            } else if (rootHandler != null) {
                rootHandler.endElement(namespaceURI, localName, qName);
            }
        }

        public final void startDocument() throws SAXException {
            if (rootHandler != null) {
                rootHandler.startDocument();
            }
        }

        public final void endDocument() throws SAXException {
            if (rootHandler != null) {
                rootHandler.endDocument();

            }
        }

        public final void characters(char[] buff, int offset, int size)
                throws SAXException
        {
            if (subdocument) {
                subdocumentHandler.characters(buff, offset, size);
            } else if (rootHandler != null) {
                rootHandler.characters(buff, offset, size);
            }
        }

    }

    /**
     * A {@link org.xml.sax.ContentHandler ContentHandler} that splits XML
     * documents into smaller chunks. Each chunk is processed by the nested
     * {@link org.xml.sax.ContentHandler ContentHandler} obtained from
     * {@link java.net.ContentHandlerFactory ContentHandlerFactory}. This is
     * useful for running XSLT engine against large XML document that will
     * hardly fit into the memory all together. <p> TODO use complete path for
     * subdocumentRoot
     */
    private static final class OutputSlicingHandler extends DefaultHandler {
        private String subdocumentRoot;

        private ContentHandlerFactory subdocumentHandlerFactory;

        private EntryElement entryElement;

        private boolean isXml;

        private boolean subdocument = false;

        private ContentHandler subdocumentHandler;

        /**
         * Constructs a new {@link OutputSlicingHandler SubdocumentHandler}
         * object.
         * 
         * @param subdocumentHandlerFactory a
         *        {@link ContentHandlerFactory ContentHandlerFactory} used to
         *        create {@link ContentHandler ContentHandler} instances for
         *        subdocuments.
         * @param entryElement TODO.
         * @param isXml TODO.
         */
        public OutputSlicingHandler(
            ContentHandlerFactory subdocumentHandlerFactory,
            EntryElement entryElement,
            boolean isXml)
        {
            this.subdocumentRoot = "class";
            this.subdocumentHandlerFactory = subdocumentHandlerFactory;
            this.entryElement = entryElement;
            this.isXml = isXml;
        }

        public final void startElement(
            String namespaceURI,
            String localName,
            String qName,
            Attributes list) throws SAXException
        {
            if (subdocument) {
                subdocumentHandler.startElement(namespaceURI,
                        localName,
                        qName,
                        list);
            } else if (localName.equals(subdocumentRoot)) {
                String name = list.getValue("name");
                if (name == null || name.length() == 0)
                    throw new SAXException("Class element without name attribute.");
                try {
                    entryElement.openEntry(isXml
                            ? name.concat(".class.xml")
                            : name.concat(".class"));
                } catch (IOException ex) {
                    throw new SAXException(ex.toString(), ex);
                }
                subdocumentHandler = subdocumentHandlerFactory.createContentHandler();
                subdocumentHandler.startDocument();
                subdocumentHandler.startElement(namespaceURI,
                        localName,
                        qName,
                        list);
                subdocument = true;
            }
        }

        public final void endElement(
            String namespaceURI,
            String localName,
            String qName) throws SAXException
        {
            if (subdocument) {
                subdocumentHandler.endElement(namespaceURI, localName, qName);
                if (localName.equals(subdocumentRoot)) {
                    subdocumentHandler.endDocument();
                    subdocument = false;
                    try {
                        entryElement.closeEntry();
                    } catch (IOException ex) {
                        throw new SAXException(ex.toString(), ex);
                    }
                }
            }
        }

        public final void startDocument() throws SAXException {
        }

        public final void endDocument() throws SAXException {
        }

        public final void characters(char[] buff, int offset, int size)
                throws SAXException
        {
            if (subdocument) {
                subdocumentHandler.characters(buff, offset, size);
            }
        }

    }

    private static interface EntryElement {

        OutputStream openEntry(String name) throws IOException;

        void closeEntry() throws IOException;

    }

    private static final class SingleDocElement implements EntryElement {
        private OutputStream os;

        public SingleDocElement(OutputStream os) {
            this.os = os;
        }

        public OutputStream openEntry(String name) throws IOException {
            return os;
        }

        public void closeEntry() throws IOException {
            os.flush();
        }

    }

    private static final class ZipEntryElement implements EntryElement {
        private ZipOutputStream zos;

        public ZipEntryElement(ZipOutputStream zos) {
            this.zos = zos;
        }

        public OutputStream openEntry(String name) throws IOException {
            ZipEntry entry = new ZipEntry(name);
            zos.putNextEntry(entry);
            return zos;
        }

        public void closeEntry() throws IOException {
            zos.flush();
            zos.closeEntry();
        }

    }

}
