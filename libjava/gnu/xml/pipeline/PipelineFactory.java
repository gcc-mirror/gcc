/* PipelineFactory.java -- 
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

package gnu.xml.pipeline;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.lang.reflect.Constructor;
import java.util.StringTokenizer;

import org.xml.sax.*;
import org.xml.sax.ext.*;


/**
 * This provides static factory methods for creating simple event pipelines.
 * These pipelines are specified by strings, suitable for passing on
 * command lines or embedding in element attributes.  For example, one way
 * to write a pipeline that restores namespace syntax, validates (stopping
 * the pipeline on validity errors) and then writes valid data to standard
 * output is this: <pre>
 *      nsfix | validate | write ( stdout )</pre>
 *
 * <p> In this syntax, the tokens are always separated by whitespace, and each
 * stage of the pipeline may optionally have a parameter (which can be a
 * pipeline) in parentheses.  Interior stages are called filters, and the
 * rightmost end of a pipeline is called a terminus.
 *
 * <p> Stages are usually implemented by a single class, which may not be
 * able to act as both a filter and a terminus; but any terminus can be
 * automatically turned into a filter, through use of a {@link TeeConsumer}.
 * The stage identifiers are either class names, or are one of the following
 * short identifiers built into this class.  (Most of these identifiers are
 * no more than aliases for classes.)  The built-in identifiers include:</p>
 
 <table border="1" cellpadding="3" cellspacing="0">
    <tr bgcolor="#ccccff" class="TableHeadingColor">
	<th align="center" width="5%">Stage</th>
	<th align="center" width="9%">Parameter</th>
	<th align="center" width="1%">Terminus</th>
	<th align="center">Description</th>
    </tr>

    <tr valign="top" align="center">
	<td><a href="../dom/Consumer.html">dom</a></td>
	<td><em>none</em></td>
	<td> yes </td>
	<td align="left"> Applications code can access a DOM Document built
	from the input event stream.  When used as a filter, this buffers
	data up to an <em>endDocument</em> call, and then uses a DOM parser
	to report everything that has been recorded (which can easily be
	less than what was reported to it).  </td>
    </tr>
    <tr valign="top" align="center">
	<td><a href="NSFilter.html">nsfix</a></td>
	<td><em>none</em></td>
	<td>no</td>
	<td align="left">This stage ensures that the XML element and attribute
	names in its output use namespace prefixes and declarations correctly.
	That is, so that they match the "Namespace plus LocalName" naming data
	with which each XML element and attribute is already associated.  </td>
    </tr>
    <tr valign="top" align="center">
	<td><a href="EventFilter.html">null</a></td>
	<td><em>none</em></td>
	<td>yes</td>
	<td align="left">This stage ignores all input event data.</td>
    </tr>
    <tr valign="top" align="center">
	<td><a href="CallFilter.html">server</a></td>
	<td><em>required</em><br> server URL </td>
	<td>no</td>
	<td align="left">Sends its input as XML request to a remote server,
	normally a web application server using the HTTP or HTTPS protocols.
	The output of this stage is the parsed response from that server.</td>
    </tr>
    <tr valign="top" align="center">
	<td><a href="TeeConsumer.html">tee</a></td>
	<td><em>required</em><br> first pipeline</td>
	<td>no</td>
	<td align="left">This sends its events down two paths; its parameter
	is a pipeline descriptor for the first path, and the second path
	is the output of this stage.</td>
    </tr>

    <tr valign="top" align="center">
	<td><a href="ValidationConsumer.html">validate</a></td>
	<td><em>none</em></td>
	<td>yes</td>
	<td align="left">This checks for validity errors, and reports them
	through its error handler.  The input must include declaration events
	and some lexical events.  </td>
    </tr>
    <tr valign="top" align="center">
	<td><a href="WellFormednessFilter.html">wf</a></td>
	<td><em>none</em></td>
	<td>yes</td>
	<td align="left"> This class provides some basic "well formedness"
	tests on the input event stream, and reports a fatal error if any
	of them fail.  One example: start/end calls for elements must match.
	No SAX parser is permitted to produce malformed output, but other
	components can easily do so.</td>
    </tr>
    <tr valign="top" align="center">
	<td>write</td>
	<td><em>required</em><br> "stdout", "stderr", or filename</td>
	<td>yes</td>
	<td align="left"> Writes its input to the specified output, as pretty
	printed XML text encoded using UTF-8.  Input events must be well
	formed and "namespace fixed", else the output won't be XML (or possibly
	namespace) conformant.  The symbolic names represent
	<em>System.out</em> and <em>System.err</em> respectively; names must
	correspond to files which don't yet exist.</td>
    </tr>
    <tr valign="top" align="center">
	<td>xhtml</td>
	<td><em>required</em><br> "stdout", "stderr", or filename</td>
	<td>yes</td>
	<td align="left"> Like <em>write</em> (above), except that XHTML rules
	are followed.  The XHTML 1.0 Transitional document type is declared,
	and only ASCII characters are written (for interoperability).  Other
	characters are written as entity or character references; the text is
	pretty printed.</td>
    </tr>
    <tr valign="top" align="center">
	<td><a href="XIncludeFilter.html">xinclude</a></td>
	<td><em>none</em></td>
	<td>no</td>
	<td align="left">This stage handles XInclude processing.
	This is like entity inclusion, except that the included content
	is declared in-line rather than in the DTD at the beginning of
	a document.
	</td>
    </tr>
    <tr valign="top" align="center">
	<td><a href="XsltFilter.html">xslt</a></td>
	<td><em>required</em><br> XSLT stylesheet URI</td>
	<td>no</td>
	<td align="left">This stage handles XSLT transformation
	according to a stylesheet.
	The implementation of the transformation may not actually
	stream data, although if such an XSLT engine is in use
	then that can happen.
	</td>
    </tr>

 </table>
 
 * <p> Note that {@link EventFilter#bind} can automatically eliminate
 * some filters by setting SAX2 parser features appropriately.  This means
 * that you can routinely put filters like "nsfix", "validate", or "wf" at the
 * front of a pipeline (for components that need inputs conditioned to match
 * that level of correctness), and know that it won't actually be used unless
 * it's absolutely necessary.
 *
 * @author David Brownell
 */
public class PipelineFactory
{
    /**
     * Creates a simple pipeline according to the description string passed in.
     */
    public static EventConsumer createPipeline (String description)
    throws IOException
    {
	return createPipeline (description, null);
    }

    /**
     * Extends an existing pipeline by prepending the filter pipeline to the
     * specified consumer.  Some pipelines need more customization than can
     * be done through this simplified syntax.  When they are set up with
     * direct API calls, use this method to merge more complex pipeline
     * segments with easily configured ones.
     */
    public static EventConsumer createPipeline (
	String		description,
	EventConsumer	next
    ) throws IOException
    {
	// tokens are (for now) what's separated by whitespace;
	// very easy to parse, but IDs never have spaces.

	StringTokenizer		tokenizer;
	String			tokens [];

	tokenizer = new StringTokenizer (description);
	tokens = new String [tokenizer.countTokens ()];
	for (int i = 0; i < tokens.length; i++)
	    tokens [i] = tokenizer.nextToken ();

	PipelineFactory		factory = new PipelineFactory ();
	Pipeline		pipeline = factory.parsePipeline (tokens, next);

	return pipeline.createPipeline ();
    }


    private PipelineFactory () { /* NYET */ }


    /**
     * Extends an existing pipeline by prepending a pre-tokenized filter
     * pipeline to the specified consumer.  Tokens are class names (or the
     * predefined aliases) left and right parenthesis, and the vertical bar.
     */
    public static EventConsumer createPipeline (
	String		tokens [],
	EventConsumer	next
    ) throws IOException
    {
	PipelineFactory		factory = new PipelineFactory ();
	Pipeline		pipeline = factory.parsePipeline (tokens, next);

	return pipeline.createPipeline ();
    }


    private String		tokens [];
    private int			index;

    private Pipeline parsePipeline (String toks [], EventConsumer next)
    {
	tokens = toks;
	index = 0;
	
	Pipeline retval = parsePipeline (next);

	if (index != toks.length)
	    throw new ArrayIndexOutOfBoundsException (
		    "extra token: " + tokens [index]);
	return retval;
    }

    // pipeline  ::= stage | stage '|' pipeline
    private Pipeline parsePipeline (EventConsumer next)
    {
	Pipeline	retval = new Pipeline (parseStage ());

	// minimal pipelines:  "stage" and "... | id"
	if (index > (tokens.length - 2)
		|| !"|".equals (tokens [index])
		) {
	    retval.next = next;
	    return retval;
	}
	index++;
	retval.rest = parsePipeline (next);
	return retval;
    }

    // stage     ::= id    | id '(' pipeline ')'
    private Stage parseStage ()
    {
	Stage		retval = new Stage (tokens [index++]);

	// minimal stages:  "id" and "id ( id )"
	if (index > (tokens.length - 2)
		|| !"(".equals (tokens [index]) /*)*/
		)
	    return retval;
	
	index++;
	retval.param = parsePipeline (null);
	if (index >= tokens.length)
	    throw new ArrayIndexOutOfBoundsException (
		    "missing right paren");
	if (/*(*/ !")".equals (tokens [index++]))
	    throw new ArrayIndexOutOfBoundsException (
		    "required right paren, not: " + tokens [index - 1]);
	return retval;
    }


    //
    // these classes obey the conventions for constructors, so they're
    // only built in to this table of shortnames
    //
    //	- filter (one or two types of arglist)
    //	   * last constructor is 'next' element
    //	   * optional (first) string parameter
    //
    //	- terminus (one or types of arglist)
    //	   * optional (only) string parameter
    //
    // terminus stages are transformed into filters if needed, by
    // creating a "tee".  filter stages aren't turned to terminus
    // stages though; either eliminate such stages, or add some
    // terminus explicitly.
    //
    private static final String builtinStages [][] = {
	{ "dom",	"gnu.xml.dom.Consumer" },
	{ "nsfix",	"gnu.xml.pipeline.NSFilter" },
	{ "null",	"gnu.xml.pipeline.EventFilter" },
	{ "server",	"gnu.xml.pipeline.CallFilter" },
	{ "tee",	"gnu.xml.pipeline.TeeConsumer" },
	{ "validate",	"gnu.xml.pipeline.ValidationConsumer" },
	{ "wf",		"gnu.xml.pipeline.WellFormednessFilter" },
	{ "xinclude",	"gnu.xml.pipeline.XIncludeFilter" },
	{ "xslt",	"gnu.xml.pipeline.XsltFilter" },

// XXX want:  option for validate, to preload external part of a DTD

	    //	xhtml, write ... nyet generic-ready
    };

    private static class Stage
    {
	String		id;
	Pipeline	param;

	Stage (String name)
	    {  id = name; }

	public String toString ()
	{
	    if (param == null)
		return id;
	    return id + " ( " + param + " )";
	}

	private void fail (String message)
	throws IOException
	{
	    throw new IOException ("in '" + id
		    + "' stage of pipeline, " + message);
	}

	EventConsumer createStage (EventConsumer next)
	throws IOException
	{
	    String	 name = id;

	    // most builtins are just class aliases
	    for (int i = 0; i < builtinStages.length; i++) {
		if (id.equals (builtinStages [i][0])) {
		    name = builtinStages [i][1];
		    break;
		}
	    }

	    // Save output as XML or XHTML text
	    if ("write".equals (name) || "xhtml".equals (name)) {
		String		filename;
		boolean		isXhtml = "xhtml".equals (name);
		OutputStream	out = null;
		TextConsumer	consumer;

		if (param == null)
		    fail ("parameter is required");

		filename = param.toString ();
		if ("stdout".equals (filename))
		    out = System.out;
		else if ("stderr".equals (filename))
		    out = System.err;
		else {
		    File f = new File (filename);

/*
		    if (!f.isAbsolute ())
			fail ("require absolute file paths");
 */
		    if (f.exists ())
			fail ("file already exists: " + f.getName ());

// XXX this races against the existence test
		    out = new FileOutputStream (f);
		}
		
		if (!isXhtml)
		    consumer = new TextConsumer (out);
		else
		    consumer = new TextConsumer (
			new OutputStreamWriter (out, "8859_1"),
			true);
		
		consumer.setPrettyPrinting (true);
		if (next == null)
		    return consumer;
		return new TeeConsumer (consumer, next);

	    } else {
		//
		// Here go all the builtins that are just aliases for
		// classes, and all stage IDs that started out as such
		// class names.  The following logic relies on several
		// documented conventions for constructor invocation.
		//
		String		msg = null;

		try {
		    Class	klass = Class.forName (name);
		    Class	argTypes [] = null;
		    Constructor	constructor = null;
		    boolean	filter = false;
		    Object	params [] = null;
		    Object	obj = null;

		    // do we need a filter stage?
		    if (next != null) {
			// "next" consumer is always passed, with
			// or without the optional string param
			if (param == null) {
			    argTypes = new Class [1];
			    argTypes [0] = EventConsumer.class;

			    params = new Object [1];
			    params [0] = next;

			    msg = "no-param filter";
			} else {
			    argTypes = new Class [2];
			    argTypes [0] = String.class;
			    argTypes [1] = EventConsumer.class;

			    params = new Object [2];
			    params [0] = param.toString ();
			    params [1] = next;

			    msg = "one-param filter";
			}


			try {
			    constructor = klass.getConstructor (argTypes);
			} catch (NoSuchMethodException e) {
			    // try creating a filter from a
			    // terminus and a tee
			    filter = true;
			    msg += " built from ";
			}
		    }

		    // build from a terminus stage, with or
		    // without the optional string param
		    if (constructor == null) {
			String	tmp;

			if (param == null) {
			    argTypes = new Class [0];
			    params = new Object [0];

			    tmp = "no-param terminus";
			} else {
			    argTypes = new Class [1];
			    argTypes [0] = String.class;

			    params = new Object [1];
			    params [0] = param.toString ();

			    tmp = "one-param terminus";
			}
			if (msg == null)
			    msg = tmp;
			else
			    msg += tmp;
			constructor = klass.getConstructor (argTypes);
			    // NOT creating terminus by dead-ending
			    // filters ... users should think about
			    // that one, something's likely wrong
		    }
		    
		    obj = constructor.newInstance (params);

		    // return EventConsumers directly, perhaps after
		    // turning them into a filter
		    if (obj instanceof EventConsumer) {
			if (filter)
			    return new TeeConsumer ((EventConsumer) obj, next);
			return (EventConsumer) obj;
		    }
		    
		    // if it's not a handler, it's an error
		    // we can wrap handlers in a filter
		    EventFilter		retval = new EventFilter ();
		    boolean		updated = false;

		    if (obj instanceof ContentHandler) {
			retval.setContentHandler ((ContentHandler) obj);
			updated = true;
		    }
		    if (obj instanceof DTDHandler) {
			retval.setDTDHandler ((DTDHandler) obj);
			updated = true;
		    }
		    if (obj instanceof LexicalHandler) {
			retval.setProperty (
			    EventFilter.PROPERTY_URI + "lexical-handler",
			    obj);
			updated = true;
		    }
		    if (obj instanceof DeclHandler) {
			retval.setProperty (
			    EventFilter.PROPERTY_URI + "declaration-handler",
			    obj);
			updated = true;
		    }

		    if (!updated)
			fail ("class is neither Consumer nor Handler");
		    
		    if (filter)
			return new TeeConsumer (retval, next);
		    return retval;

		} catch (IOException e) {
		    throw e;

		} catch (NoSuchMethodException e) {
		    fail (name + " constructor missing -- " + msg);

		} catch (ClassNotFoundException e) {
		    fail (name + " class not found");

		} catch (Exception e) {
		    // e.printStackTrace ();
		    fail ("stage not available: " + e.getMessage ());
		}
	    }
	    // NOTREACHED
	    return null;
	}
    }

    private static class Pipeline
    {
	Stage		stage;

	// rest may be null
	Pipeline	rest;
	EventConsumer	next;

	Pipeline (Stage s)
	    { stage = s; }

	public String toString ()
	{
	    if (rest == null && next == null)
		return stage.toString ();
	    if (rest != null)
		return stage + " | " + rest;
	    throw new IllegalArgumentException ("next");
	}

	EventConsumer createPipeline ()
	throws IOException
	{
	    if (next == null) {
		if (rest == null)
		    next = stage.createStage (null);
		else
		    next = stage.createStage (rest.createPipeline ());
	    }
	    return next;
	}
    }

/*
    public static void main (String argv [])
    {
	try {
	    // three basic terminus cases
	    createPipeline ("null");
	    createPipeline ("validate");
	    createPipeline ("write ( stdout )");

	    // four basic filters
	    createPipeline ("nsfix | write ( stderr )");
	    createPipeline ("wf | null");
	    createPipeline ("null | null");
	    createPipeline (
"call ( http://www.example.com/services/xml-1a ) | xhtml ( stdout )");

	    // tee junctions
	    createPipeline ("tee ( validate ) | write ( stdout )");
	    createPipeline ("tee ( nsfix | write ( stdout ) ) | validate");

	    // longer pipeline
	    createPipeline ("nsfix | tee ( validate ) | write ( stdout )");
	    createPipeline (
		"null | wf | nsfix | tee ( validate ) | write ( stdout )");

	    // try some parsing error cases
	    try {
		createPipeline ("null (");		// extra token '('
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }

	    try {
		createPipeline ("nsfix |");		// extra token '|'
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }

	    try {
		createPipeline ("xhtml ( foo");		// missing right paren
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }

	    try {
		createPipeline ("xhtml ( foo bar");	// required right paren
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }

	    try {
		createPipeline ("tee ( nsfix | validate");// missing right paren
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }

	    // try some construction error cases

	    try {
		createPipeline ("call");		// missing param
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }
	    try {
		createPipeline ("call ( foobar )");	// broken param
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }
	    try {
		createPipeline ("nsfix ( foobar )");	// illegal param
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }
	    try {
		createPipeline ("null ( foobar )");	// illegal param
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }
	    try {
		createPipeline ("wf ( foobar )");	// illegal param
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }
	    try {
		createPipeline ("xhtml ( foobar.html )");
		new File ("foobar.html").delete ();
		// now supported
	    } catch (Exception e) {
		System.err.println ("** err: " + e.getMessage ()); }
	    try {
		createPipeline ("xhtml");		// missing param
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }
	    try {
		createPipeline ("write ( stdout ) | null");	// nonterminal
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }
	    try {
		createPipeline ("validate | null");
		// now supported
	    } catch (Exception e) {
		System.err.println ("** err: " + e.getMessage ()); }
	    try {
		createPipeline ("validate ( foo )");	// illegal param
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }
	    try {
		createPipeline ("tee");			// missing param
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }
	    try {
		    // only builtins so far
		createPipeline ("com.example.xml.FilterClass");
		System.err.println ("** didn't report error");
	    } catch (Exception e) {
		System.err.println ("== err: " + e.getMessage ()); }

	} catch (Exception e) {
	    e.printStackTrace ();
	}
    }
/**/

}
