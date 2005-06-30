/* CallFilter.java -- 
   Copyright (C) 1999,2000,2001 Free Software Foundation, Inc.

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

package gnu.xml.pipeline;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.net.URLConnection;
import java.io.Writer;

import org.xml.sax.DTDHandler;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

import gnu.xml.util.Resolver;
import gnu.xml.util.XMLWriter;


/**
 * Input is sent as an XML request to given URI, and the output of this
 * filter is the parsed response to that request.
 * A connection is opened to the remote URI when the startDocument call is
 * issued through this filter, and the request is finished when the
 * endDocument call is issued.  Events should be written quickly enough to
 * prevent the remote HTTP server from aborting the connection due to
 * inactivity; you may want to buffer text in an earlier pipeline stage.
 * If your application requires validity checking of such
 * outputs, have the output pipeline include a validation stage.
 *
 * <p>In effect, this makes a remote procedure call to the URI, with the
 * request and response document syntax as chosen by the application.
 * <em>Note that all the input events must be seen, and sent to the URI,
 * before the first output event can be seen. </em>  Clients are delayed
 * at least by waiting for the server to respond, constraining concurrency.
 * Services can thus be used to synchronize concurrent activities, and
 * even to prioritize service among different clients.
 *
 * <p> You are advised to avoid restricting yourself to an "RPC" model
 * for distributed computation.  With a World Wide Web, network latencies
 * and failures (e.g. non-availability)
 * are significant; adopting a "procedure" model, rather than a workflow
 * model where bulk requests are sent and worked on asynchronously, is not
 * generally an optimal system-wide architecture.  When the messages may
 * need authentication, such as with an OpenPGP signature, or when server
 * loads don't argue in favor of immediate responses, non-RPC models can
 * be advantageous.  (So-called "peer to peer" computing models are one
 * additional type of model, though too often that term is applied to
 * systems that still have a centralized control structure.)
 *
 * <p> <em>Be strict in what you send, liberal in what you accept,</em> as
 * the Internet tradition goes.  Strictly conformant data should never cause
 * problems to its receiver; make your request pipeline be very strict, and
 * don't compromise on that.  Make your response pipeline strict as well,
 * but be ready to tolerate specific mild, temporary, and well-documented
 * variations from specific communications peers.
 *
 * @see XmlServlet
 *
 * @author David Brownell
 */
final public class CallFilter implements EventConsumer
{
    private Requestor			req;
    private EventConsumer		next;
    private URL				target;
    private URLConnection		conn;
    private ErrorHandler		errHandler;


    /**
     * Initializes a call filter so that its inputs are sent to the
     * specified URI, and its outputs are sent to the next consumer
     * provided.
     *
     * @exception IOException if the URI isn't accepted as a URL
     */
	// constructor used by PipelineFactory
    public CallFilter (String uri, EventConsumer next)
    throws IOException
    {
	this.next = next;
	req = new Requestor ();
	setCallTarget (uri);
    }

    /**
     * Assigns the URI of the call target to be used.
     * Does not affect calls currently being made.
     */
    final public void setCallTarget (String uri)
    throws IOException
    {
	target = new URL (uri);
    }

    /**
     * Assigns the error handler to be used to present most fatal
     * errors.
     */
    public void setErrorHandler (ErrorHandler handler)
    {
	req.setErrorHandler (handler);
    }


    /**
     * Returns the call target's URI.
     */
    final public String getCallTarget ()
    {
	return target.toString ();
    }

    /** Returns the content handler currently in use. */
    final public org.xml.sax.ContentHandler getContentHandler ()
    {
	return req;
    }

    /** Returns the DTD handler currently in use. */
    final public DTDHandler getDTDHandler ()
    {
	return req;
    }


    /**
     * Returns the declaration or lexical handler currently in
     * use, or throws an exception for other properties.
     */
    final public Object getProperty (String id)
    throws SAXNotRecognizedException
    {
	if (EventFilter.DECL_HANDLER.equals (id))
	    return req;
	if (EventFilter.LEXICAL_HANDLER.equals (id))
	    return req;
	throw new SAXNotRecognizedException (id);
    }


    // JDK 1.1 seems to need it to be done this way, sigh
    ErrorHandler getErrorHandler () { return errHandler; }

    //
    // Takes input and echoes to server as POST input.
    // Then sends the POST reply to the next pipeline element.
    //
    final class Requestor extends XMLWriter
    {
	Requestor ()
	{
	    super ((Writer)null);
	}

	public synchronized void startDocument () throws SAXException
	{
	    // Connect to remote object and set up to send it XML text
	    try {
		if (conn != null)
		    throw new IllegalStateException ("call is being made");

		conn = target.openConnection ();
		conn.setDoOutput (true);
		conn.setRequestProperty ("Content-Type",
			    "application/xml;charset=UTF-8");

		setWriter (new OutputStreamWriter (
			conn.getOutputStream (),
			"UTF8"), "UTF-8");

	    } catch (IOException e) {
		fatal ("can't write (POST) to URI: " + target, e);
	    }

	    // NOW base class can safely write that text!
	    super.startDocument ();
	}

	public void endDocument () throws SAXException
	{
	    //
	    // Finish writing the request (for HTTP, a POST);
	    // this closes the output stream.
	    //
	    super.endDocument ();

	    //
	    // Receive the response.
	    // Produce events for the next stage.
	    //
	    InputSource	source;
	    XMLReader	producer;
	    String	encoding;

	    try {

		source = new InputSource (conn.getInputStream ());

// FIXME if status is anything but success, report it!!  It'd be good to
// save the request data just in case we need to deal with a forward.

		encoding = Resolver.getEncoding (conn.getContentType ());
		if (encoding != null)
		    source.setEncoding (encoding);

		producer = XMLReaderFactory.createXMLReader ();
		producer.setErrorHandler (getErrorHandler ());
		EventFilter.bind (producer, next);
		producer.parse (source);
		conn = null;

	    } catch (IOException e) {
		fatal ("I/O Exception reading response, " + e.getMessage (), e);
	    }
	}
    }
}
