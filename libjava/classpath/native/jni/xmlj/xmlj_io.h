/* xmlj_io.h -
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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

#ifndef XMLJ_IO_H
#define XMLJ_IO_H

#include <jni.h>
#include <libxml/xmlIO.h>
#include "xmlj_error.h"

typedef struct _SAXParseContext
{

  JNIEnv *env; /* Current JNI environment */
  jobject obj; /* The gnu.xml.libxmlj.sax.GnomeXmlReader instance */
  xmlParserCtxtPtr ctx; /* libxml2 parser context */
  xmlSAXLocatorPtr loc; /* libxml2 SAX locator */
  xmlSAXHandlerPtr sax; /* pristine SAX handler */
  jstring publicId;
  jstring systemId;

  jmethodID startDTD;
  jmethodID externalEntityDecl;
  jmethodID internalEntityDecl;
  jmethodID resolveEntity;
  jmethodID notationDecl;
  jmethodID attributeDecl;
  jmethodID elementDecl;
  jmethodID unparsedEntityDecl;
  jmethodID setDocumentLocator;
  jmethodID startDocument;
  jmethodID endDocument;
  jmethodID startElement;
  jmethodID endElement;
  jmethodID characters;
  jmethodID ignorableWhitespace;
  jmethodID processingInstruction;
  jmethodID comment;
  jmethodID cdataBlock;
  jmethodID warning;
  jmethodID error;
  jmethodID fatalError;

  jmethodID resolveURIAndOpen; /* JavaProxy */
  jclass stringClass; 
}
SAXParseContext;

SAXParseContext *
xmljNewSAXParseContext (JNIEnv * env, jobject obj, xmlParserCtxtPtr ctx,
                        jstring publicId, jstring systemId);

void
xmljFreeSAXParseContext (SAXParseContext * saxCtx);

xmlParserCtxtPtr
xmljNewParserContext (JNIEnv * env,
                      jobject inputStream,
                      jbyteArray detectBuffer,
                      jstring publicId,
                      jstring systemId,
                      jstring base,
                      jboolean validate,
                      jboolean coalesce,
                      jboolean expandEntities,
                      jboolean loadEntities);

void
xmljFreeParserContext (xmlParserCtxtPtr parserContext);

xmlDocPtr
xmljParseDocument (JNIEnv * env,
                   jobject self,
                   jobject in,
                   jbyteArray detectBuffer,
                   jstring publicId,
                   jstring systemId,
                   jstring base,
                   jboolean validate,
                   jboolean coalesce,
                   jboolean expandEntities,
                   jboolean contentHandler,
                   jboolean dtdHandler,
                   jboolean entityResolver,
                   jboolean errorHandler,
                   jboolean declarationHandler,
                   jboolean lexicalHandler,
                   int saxMode);

xmlDocPtr
xmljParseDocument2 (JNIEnv * env,
                    xmlParserCtxtPtr ctx,
                    SAXParseContext *saxCtx,
                    xmlSAXHandlerPtr sax,
                    int saxMode);

xmlParserInputPtr
xmljNewParserInput (JNIEnv * env,
		    jobject inputStream,
                    jbyteArray detectBuffer,
                    xmlParserCtxtPtr parserContext);

xmlParserInputBufferPtr
xmljNewParserInputBuffer (JNIEnv * env,
			  jobject inputStream,
                          xmlCharEncoding encoding);

void
xmljSaveFileToJavaOutputStream (JNIEnv * env, jobject outputStream,
                                xmlDocPtr tree,
                                const char *outputEncoding);

/*
xmlParserInputPtr
xmljLoadExternalEntity (const char *URL, const char *ID,
					  xmlParserCtxtPtr ctxt);

jobject
xmljResolveURI (SaxErrorContext * saxErrorContext, const char *URL,
			const char *ID);
*/
xmlDocPtr
xmljResolveURIAndOpen (SAXParseContext *saxContext,
		       const char *URL, const char *ID);


void
xmljSetThreadContext (SAXParseContext * ctxt);

SAXParseContext *
xmljGetThreadContext (void);

void
xmljClearThreadContext (void);

#endif /* !defined XMLJ_IO_H */
