<?xml version="1.0" encoding="utf-8"?>

<!-- packageclasses.xsl
     Copyright (C) 2003 Free Software Foundation, Inc.
     
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
     -->

<!-- Creates the package descriptor files for HTML documentation. 
     -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gjdoc="http://www.gnu.org/software/cp-tools/gjdocxml"
  xmlns:html="http://www.w3.org/TR/REC-html40"
  xmlns="http://www.w3.org/TR/REC-html40">

  <xsl:include href="html_common.xsl"/>

  <xsl:output method="xml"
    encoding="utf-8"
    indent="no"/>

  <xsl:strip-space elements="*"/>

  <xsl:template name="add_link_to_class">
    <a href="{concat(@name, '.html')}" target="content" class="menu packageclasses">
      <xsl:choose>
        <xsl:when test="gjdoc:isInterface">
          <i><xsl:value-of select="@name"/></i>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@name"/>
        </xsl:otherwise>
      </xsl:choose>
    </a><br/>
  </xsl:template>

  <xsl:template match="/">

    <html>
      <head>
        <title>
          <xsl:value-of select="concat(/gjdoc:rootdoc/gjdoc:title, ' - ', 'Classes in ', $gjdoc.outputfile.info)"/>
        </title>
        <xsl:call-template name="include_common"/>
      </head>
      <body onload="secondaryPageLoaded('classes');" class="menu packageclasses">
        <h3 class="menu-title"><a href="package-summary.html" target="content"><xsl:value-of select="$gjdoc.outputfile.info"/></a></h3>
        <xsl:for-each select="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:containingPackage/@name=$gjdoc.outputfile.info and not(child::gjdoc:isError or child::gjdoc:isException)]">
          <xsl:sort select="@name" order="ascending"/>
          <xsl:call-template name="add_link_to_class"/>
        </xsl:for-each>

        <xsl:if test="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:containingPackage/@name=$gjdoc.outputfile.info and child::gjdoc:isException]">
          <h3><xsl:value-of select="$gjdoc.outputfile.info"/> Exceptions</h3>
          <xsl:for-each select="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:containingPackage/@name=$gjdoc.outputfile.info and child::gjdoc:isException]">
            <xsl:sort select="@name" order="ascending"/>
            <xsl:call-template name="add_link_to_class"/>
          </xsl:for-each>
        </xsl:if>

        <xsl:if test="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:containingPackage/@name=$gjdoc.outputfile.info and child::gjdoc:isError]">
          <h3><xsl:value-of select="$gjdoc.outputfile.info"/> Errors</h3>
          <xsl:for-each select="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:containingPackage/@name=$gjdoc.outputfile.info and child::gjdoc:isError]">
            <xsl:sort select="@name" order="ascending"/>
            <xsl:call-template name="add_link_to_class"/>
          </xsl:for-each>
        </xsl:if>
      </body>
    </html>
    
  </xsl:template>
  
</xsl:stylesheet>
