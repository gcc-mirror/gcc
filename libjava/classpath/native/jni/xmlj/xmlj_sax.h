/* xmlj_sax.h -
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

#ifndef XMLJ_SAX_H
#define XMLJ_SAX_H

#include "gnu_xml_libxmlj_sax_GnomeLocator.h"
#include "gnu_xml_libxmlj_sax_GnomeXMLReader.h"

#include <libxml/SAX.h>
#include <libxml/parser.h>

xmlSAXHandlerPtr
xmljNewSAXHandler (jboolean contentHandler,
                   jboolean dtdHandler,
                   jboolean entityResolver,
                   jboolean errorHandler,
                   jboolean declarationHandler,
                   jboolean lexicalHandler);

xmlParserInputPtr
xmljExternalEntityLoader (const char *systemId, const char *publicId,
                          xmlParserCtxtPtr context);

/* -- Function declarations for callback functions -- */

void xmljSAXInternalSubset(void *ctx,
                           const xmlChar *name,
                           const xmlChar *publicId,
                           const xmlChar *systemId);

xmlParserInputPtr xmljSAXResolveEntity(void *ctx,
                                       const xmlChar *publicId,
                                       const xmlChar *systemId);

xmlEntityPtr xmljSAXGetEntity(void *ctx,
                              const xmlChar *name);

void xmljSAXEntityDecl(void *ctx,
                       const xmlChar *name,
                       int type,
                       const xmlChar *publicId,
                       const xmlChar *systemId,
                       xmlChar *content);

void xmljSAXNotationDecl(void *ctx,
                         const xmlChar *name,
                         const xmlChar *publicId,
                         const xmlChar *systemId);

void xmljSAXAttributeDecl(void *ctx,
                          const xmlChar *elem,
                          const xmlChar *fullName,
                          int type,
                          int def,
                          const xmlChar *defaultValue,
                          xmlEnumerationPtr tree);

void xmljSAXElementDecl(void *ctx,
                        const xmlChar *name,
                        int type,
                        xmlElementContentPtr content);

void xmljSAXUnparsedEntityDecl(void *ctx,
                               const xmlChar *name,
                               const xmlChar *publicId, 
                               const xmlChar *systemId,
                               const xmlChar *notationName);

void xmljSAXSetDocumentLocator(void *ctx,
                               xmlSAXLocatorPtr loc);

void xmljSAXStartDocument(void *ctx);

void xmljSAXEndDocument(void *ctx);

void xmljSAXStartElement(void *ctx,
                         const xmlChar *name,
                         const xmlChar **atts);

void xmljSAXEndElement(void *ctx,
                       const xmlChar *name);

void xmljSAXReference(void *ctx,
                      const xmlChar *name);

void xmljSAXCharacters(void *ctx,
                       const xmlChar *ch,
                       int len);

void xmljSAXIgnorableWhitespace(void *ctx,
                                const xmlChar *ch,
                                int len);

void xmljSAXProcessingInstruction(void *ctx,
                                  const xmlChar *target,
                                  const xmlChar *data);

void xmljSAXComment(void *ctx,
                    const xmlChar *value);

void xmljSAXCDataBlock(void *ctx,
                       const xmlChar *ch,
                       int len);

void xmljSAXWarning(void *ctx,
                    const char *msg,
                    ...);

void xmljSAXError(void *ctx,
                  const char *msg,
                  ...);

void xmljSAXFatalError(void *ctx,
                       const char *msg,
                       ...);

void xmljCheckWellFormed(xmlParserCtxtPtr ctx);

jstring xmljAttributeTypeName (JNIEnv *env,
                               int type);

jstring xmljAttributeModeName (JNIEnv *env,
                               int type);

#endif /* !defined XMLJ_SAX_H */
