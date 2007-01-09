<?xml version='1.0'?>
<!-- XSL stylesheet to convert checkstyle XML to HTML -->
<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

  <!-- This tells the XSLT processor to emit HTML -->
  <xsl:output method='html'
    doctype-public='-//W3C//DTD HTML 4.01//EN'
    doctype-system='http://www.w3.org/TR/html4/strict.dtd'
    omit-xml-declaration='yes'/>

  <!-- Match the checkstyle root element -->
  <xsl:template match='checkstyle'>
    <html>
      <head>
        <title>Checkstyle results</title>
        <link rel='stylesheet' type='text/css' href='checkstyle.css' />
      </head>
      <body>
        <h1>Checkstyle results</h1>
        <div>The following document contains the results of
        <a href='http://checkstyle.sourceforge.net/'>Checkstyle</a>.</div>
        
        <h2>Summary</h2>
        <table summary='Summary'>
          <tr>
            <th>Files</th><th>Infos</th><th>Warnings</th><th>Errors</th>
          </tr>
          <tr>
            <td><xsl:value-of select='count(file)' /></td>
            <td><xsl:value-of select='count(file/error[@severity="info"])' /></td>
            <td><xsl:value-of select='count(file/error[@severity="warning"])' /></td>
            <td><xsl:value-of select='count(file/error[@severity="error"])' /></td>
          </tr>
        </table>

        <h2>Files</h2>
        <table summary='Files'>
          <tr>
            <th>File</th><th>I</th><th>W</th><th>E</th>
          </tr>
          <!-- Process file elements in file mode -->
          <xsl:apply-templates select='file' mode='file'>
	    <xsl:sort select="@name"/>
	  </xsl:apply-templates>
        </table>

        <!-- Process file elements in detail mode -->
        <xsl:apply-templates select='file' mode='detail'>
	  <xsl:sort select="@name"/>
	</xsl:apply-templates>
      </body>
    </html>
  </xsl:template>

  <!-- Match a file element in file mode -->
  <xsl:template match='file' mode='file'>
    <xsl:if test='count(error) &gt; 0'>
      <tr>
        <td>
          <xsl:element name='a'>
            <xsl:attribute name='href'>
              #<xsl:value-of select='translate(string(@name),"/","__")' />
            </xsl:attribute>
            <xsl:value-of select='@name' />
          </xsl:element>
        </td>
        <td><xsl:value-of select='count(error[@severity="info"])' /></td>
        <td><xsl:value-of select='count(error[@severity="warning"])' /></td>
        <td><xsl:value-of select='count(error[@severity="error"])' /></td>
      </tr>
    </xsl:if>
  </xsl:template>

  <!-- Match a file element in detail mode-->
  <xsl:template match='file' mode='detail'>
    <xsl:if test='count(error) &gt; 0'>
      <h3>
        <xsl:element name='a'>
          <xsl:attribute name='name'>
            <xsl:value-of select='translate(string(@name),"/","__")' />
          </xsl:attribute>
          <xsl:value-of select='@name' />
        </xsl:element>
      </h3>
      <table summary='Errors'>
        <tr>
          <th>Error</th><th width="100px">Line</th>
        </tr>
        <xsl:apply-templates select='error' />
      </table>
    </xsl:if>
  </xsl:template>

  <!-- Match an error element -->
  <xsl:template match='error'>
    <tr>
      <td><xsl:value-of select='@message'/></td>
      <td><xsl:value-of select='@line' /></td>
    </tr>
  </xsl:template>

</xsl:stylesheet>
