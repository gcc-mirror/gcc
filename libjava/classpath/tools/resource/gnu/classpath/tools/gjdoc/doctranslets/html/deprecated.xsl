<?xml version="1.0" encoding="utf-8"?>

<!-- deprecated.xsl
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

<!-- Creates the deprecation information page for HTML documentation.
     -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gjdoc="http://www.gnu.org/software/cp-tools/gjdocxml"
  xmlns:html="http://www.w3.org/TR/REC-html40"
  xmlns="http://www.w3.org/TR/REC-html40">

  <xsl:include href="html_common.xsl"/>

  <xsl:output method="xml"
    encoding="utf-8"
    doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
    doctype-system="http://www.w3.org/TR/html4/loose.dtd"
    indent="no"/>

  <xsl:strip-space elements="*"/>

  <xsl:template match="gjdoc:tag[attribute::kind='@see'][attribute::name='@link']">
    <xsl:call-template name="output_link_tag">
      <xsl:with-param name="p_contexturl"
        select="concat(translate(ancestor::gjdoc:classdoc/gjdoc:containingPackage/@name, '.', '/'), '/', ancestor::gjdoc:classdoc/@name,'.html')"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="/">
    <html>
      <head>
        <xsl:call-template name="output_title">
          <xsl:with-param name="p_pagetitle" select="'Deprecated List'"/>
        </xsl:call-template>
        <xsl:call-template name="include_common"/>
      </head>
      <body class="classdoc" onload="if(parent.contentPageLoaded)parent.contentPageLoaded(document.title)">
        
        <!-- Top Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_frames" select="1"/>
          <xsl:with-param name="p_show_noframes" select="1"/>
          <xsl:with-param name="p_show_package" select="0"/>
          <xsl:with-param name="p_show_package_tree" select="0"/>
          <xsl:with-param name="p_show_full_tree" select="1"/>
          <xsl:with-param name="p_show_index" select="1"/>
          <xsl:with-param name="p_show_help" select="1"/>
          <xsl:with-param name="p_show_deprecated" select="0"/>
          <xsl:with-param name="p_curr_deprecated" select="1"/>
          <xsl:with-param name="p_top" select="1"/> 
        </xsl:call-template>

        <div class="pagebody">

        <h1 class="classdoc-title">Deprecated API</h1>

        <xsl:choose>
          <xsl:when test="gjdoc:rootdoc/gjdoc:hasDeprecatedInterfaces or gjdoc:rootdoc/gjdoc:hasDeprecatedClasses or gjdoc:rootdoc/gjdoc:hasDeprecatedExceptions or gjdoc:rootdoc/gjdoc:hasDeprecatedErrors or gjdoc:rootdoc/gjdoc:rootdoc/gjdoc:hasDeprecatedMethods or gjdoc:rootdoc/gjdoc:hasDeprecatedFields">
            <div class="classdoc-tag-section-header">Contents:</div>
            <dl class="classdoc-list">
              <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedInterfaces">
                <dt><a href="#interfaces">Deprecated Interfaces</a></dt>
              </xsl:if>
              <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedClasses">
                <dt><a href="#classes">Deprecated Classes</a></dt>
              </xsl:if>
              <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedExceptions">
                <dt><a href="#exceptions">Deprecated Exceptions</a></dt>
              </xsl:if>
              <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedErrors">
                <dt><a href="#errors">Deprecated Errors</a></dt>
              </xsl:if>
              <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedFields">
                <dt><a href="#fields">Deprecated Fields</a></dt>
              </xsl:if>
              <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedMethods">
                <dt><a href="#methods">Deprecated Methods</a></dt>
              </xsl:if>
              <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedConstructors">
                <dt><a href="#constructors">Deprecated Constructors</a></dt>
              </xsl:if>
            </dl>
          </xsl:when>
          <xsl:otherwise>
            <p>No parts of this API are deprecated.</p>
          </xsl:otherwise>
        </xsl:choose>

        <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedInterfaces">
          <a name="interfaces"/>
          <h1 class="classdoc-header">Deprecated Interfaces</h1>

          <xsl:for-each select="gjdoc:rootdoc/gjdoc:classdoc/@qualifiedtypename">
            <xsl:variable name="v_currentclass" select="."/>
            <xsl:variable name="v_sub_xml_filename" select="concat(.,'.xml')"/>
            <xsl:for-each select="document($v_sub_xml_filename,/gjdoc:rootdoc)//gjdoc:classdoc[@qualifiedtypename=$v_currentclass]/@qualifiedtypename/..">
              <xsl:if test="gjdoc:isInterface">
                <xsl:call-template name="output-deprecated-class"/>
              </xsl:if>
            </xsl:for-each>
          </xsl:for-each>
        </xsl:if>

        <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedClasses">
          <a name="classes"/>
          <h1 class="classdoc-header">Deprecated Classes</h1>

          <xsl:for-each select="gjdoc:rootdoc/gjdoc:classdoc/@qualifiedtypename">
            <xsl:variable name="v_currentclass" select="."/>
            <xsl:variable name="v_sub_xml_filename" select="concat(.,'.xml')"/>
            <xsl:for-each select="document($v_sub_xml_filename,/gjdoc:rootdoc)//gjdoc:classdoc[@qualifiedtypename=$v_currentclass]/@qualifiedtypename/..">
              <xsl:if test="gjdoc:isOrdinaryClass">
                <xsl:call-template name="output-deprecated-class"/>
              </xsl:if>
            </xsl:for-each>
          </xsl:for-each>
        </xsl:if>

        <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedExceptions">
          <a name="exceptions"/>
          <h1 class="classdoc-header">Deprecated Exceptions</h1>
          
          <xsl:for-each select="gjdoc:rootdoc/gjdoc:classdoc/@qualifiedtypename">
            <xsl:variable name="v_currentclass" select="."/>
            <xsl:variable name="v_sub_xml_filename" select="concat(.,'.xml')"/>
            <xsl:for-each select="document($v_sub_xml_filename,/gjdoc:rootdoc)//gjdoc:classdoc[@qualifiedtypename=$v_currentclass]/.">
              <xsl:if test="gjdoc:isException">
                <xsl:call-template name="output-deprecated-class"/>
              </xsl:if>
            </xsl:for-each>
          </xsl:for-each>
        </xsl:if>

        <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedErrors">
          <a name="errors"/>
          <h1 class="classdoc-header">Deprecated Errors</h1>
          
          <xsl:for-each select="gjdoc:rootdoc/gjdoc:classdoc/@qualifiedtypename">
            <xsl:variable name="v_currentclass" select="."/>
            <xsl:variable name="v_sub_xml_filename" select="concat(.,'.xml')"/>
            <xsl:for-each select="document($v_sub_xml_filename,/gjdoc:rootdoc)//gjdoc:classdoc[@qualifiedtypename=$v_currentclass]/.">
              <xsl:if test="gjdoc:isError">
                <xsl:call-template name="output-deprecated-class"/>
              </xsl:if>
            </xsl:for-each>
          </xsl:for-each>
        </xsl:if>

        <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedFields">
          <a name="fields"/>
          <h1 class="classdoc-header">Deprecated Fields</h1>

          <xsl:for-each select="gjdoc:rootdoc/gjdoc:classdoc/@qualifiedtypename">
            <xsl:variable name="v_currentclass" select="."/>
            <xsl:variable name="v_sub_xml_filename" select="concat(.,'.xml')"/>
            <xsl:for-each select="document($v_sub_xml_filename,/gjdoc:rootdoc)//gjdoc:classdoc[@qualifiedtypename=$v_currentclass]/gjdoc:fielddoc/@name">
              <xsl:call-template name="output-deprecated-member"/>
            </xsl:for-each>
          </xsl:for-each>
        </xsl:if>

        <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedMethods">
          <a name="methods"/>
          <h1 class="classdoc-header">Deprecated Methods</h1>

          <xsl:for-each select="gjdoc:rootdoc/gjdoc:classdoc/@qualifiedtypename">
            <xsl:variable name="v_currentclass" select="."/>
            <xsl:variable name="v_sub_xml_filename" select="concat(.,'.xml')"/>
            <xsl:for-each select="document($v_sub_xml_filename,/gjdoc:rootdoc)//gjdoc:classdoc[@qualifiedtypename=$v_currentclass]/gjdoc:methoddoc/@name">
              <xsl:call-template name="output-deprecated-member"/>
            </xsl:for-each>
          </xsl:for-each>
        </xsl:if>

        <xsl:if test="gjdoc:rootdoc/gjdoc:hasDeprecatedConstructors">
          <a name="constructors"/>
          <h1 class="classdoc-header">Deprecated Constructors</h1>
          
          <xsl:for-each select="gjdoc:rootdoc/gjdoc:classdoc/@qualifiedtypename">
            <xsl:variable name="v_currentclass" select="."/>
            <xsl:variable name="v_sub_xml_filename" select="concat(.,'.xml')"/>
            <xsl:for-each select="document($v_sub_xml_filename,/gjdoc:rootdoc)//gjdoc:classdoc[@qualifiedtypename=$v_currentclass]/gjdoc:constructordoc/@name">
              <xsl:call-template name="output-deprecated-member"/>
            </xsl:for-each>
          </xsl:for-each>
        </xsl:if>
        <!-- Bottom Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_frames" select="1"/>
          <xsl:with-param name="p_show_noframes" select="1"/>
          <xsl:with-param name="p_show_package" select="0"/>
          <xsl:with-param name="p_show_package_tree" select="0"/>
          <xsl:with-param name="p_show_full_tree" select="1"/>
          <xsl:with-param name="p_show_index" select="1"/>
          <xsl:with-param name="p_show_help" select="1"/>
          <xsl:with-param name="p_show_deprecated" select="0"/>
          <xsl:with-param name="p_curr_deprecated" select="1"/>
          <xsl:with-param name="p_top" select="0"/> 
        </xsl:call-template>

        </div>
      </body>
    </html>
  </xsl:template>

  <xsl:template name="output-deprecated-member">

    <xsl:if test="../gjdoc:tags/gjdoc:tag[@kind='@deprecated']">
      <dl>
        <dt>
          <a href="{concat($gjdoc.pathtoroot, translate(ancestor::gjdoc:classdoc/gjdoc:containingPackage/@name, '.', '/'), '/', ancestor::gjdoc:classdoc/@name, '.html#', ., ../gjdoc:signature/@full)}">
            <xsl:value-of select="concat(ancestor::gjdoc:classdoc/@qualifiedtypename, '.', ., ../gjdoc:signature/@flat)"/>
          </a>
        </dt>
        <dd>
          <xsl:for-each select="../gjdoc:tags/gjdoc:tag[@kind='@deprecated']/gjdoc:firstSentenceTags/node()">
            <xsl:apply-templates select="."/>
          </xsl:for-each>
        </dd>
      </dl>
    </xsl:if>
    
  </xsl:template>

  <xsl:template name="output-deprecated-class">
    <xsl:if test="gjdoc:tags/gjdoc:tag[@kind='@deprecated']">
      <dl>
        <dt>
          <a href="{concat($gjdoc.pathtoroot, translate(gjdoc:containingPackage/@name, '.', '/'), '/', @name, '.html')}"><xsl:value-of select="@qualifiedtypename"/></a>
        </dt>
        <dd>
          <xsl:for-each select="gjdoc:tags/gjdoc:tag[@kind='@deprecated']/gjdoc:firstSentenceTags/node()">
            <xsl:apply-templates select="."/>
          </xsl:for-each>
        </dd>
      </dl>
    </xsl:if>    
  </xsl:template>

</xsl:stylesheet>
