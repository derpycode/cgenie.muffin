<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:xalan="org.apache.xalan.xslt.extensions.Redirect"
xmlns:common="http://exslt.org/common"
  extension-element-prefixes="common xalan">

<xsl:output method="text"/>

<xsl:param name="UP" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'"/>
<xsl:param name="LO" select="'abcdefghijklmnopqrstuvwxyz'"/>

<!--
The two params below may be set from the command line.
NML_DIR = destination of namelists; defalult is nml
DIR_SEP = directory separator; default is /
-->
<xsl:param name="NML_DIR" select="'nml'"/>
<xsl:param name="DIR_SEP" select="'/'"/>
<xsl:param name="OutputParamArrayElementWise" select="'true'"/>


<xsl:template match="node()">
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="file">
 <xsl:choose>
  <xsl:when test="element-available('common:document')">
   <common:document href="{concat($NML_DIR,$DIR_SEP,@name)}" method="text">
    <xsl:apply-templates select="*"/>
   </common:document>
  </xsl:when>
  <xsl:when test="element-available('xalan:write')">
	  <xalan:open file="{concat($NML_DIR,$DIR_SEP,@name)}" append="true"/>
   <xalan:write file="{concat($NML_DIR,$DIR_SEP,@name)}" append="true">
    <xsl:apply-templates select="*"/>
</xalan:write>
<xalan:close file="{concat($NML_DIR,$DIR_SEP,@name)}"/>
  </xsl:when>
  <xsl:otherwise>
   <xsl:message terminate="yes">
    <xsl:text>No file output support found</xsl:text>
   </xsl:message>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="namelist">
 <xsl:text>&amp;</xsl:text><xsl:value-of select="translate(@name,$LO,$UP)"/>
 <xsl:apply-templates select="*"/><xsl:text>
&amp;END
</xsl:text>
</xsl:template>

<xsl:template match="param">
 <xsl:text>
 </xsl:text><xsl:value-of select="@name"/><xsl:text>=</xsl:text><xsl:apply-templates select="value"/>
 <xsl:if test="position()!=last()">
  <xsl:text>,</xsl:text>
 </xsl:if>
</xsl:template>

<xsl:template match="value">
 <xsl:if test="@datatype='string'">
  <xsl:text>"</xsl:text>
 </xsl:if>
 <xsl:for-each select="node()">
  <xsl:choose>
   <xsl:when test="local-name()='varref'">
    <xsl:choose>
     <xsl:when test="/job/vars/var[@name=normalize-space(current()/text())]">
      <xsl:value-of select="/job/vars/var[@name=normalize-space(current()/text())]/text()"/>
     </xsl:when>
     <xsl:when test="/job/testing/var[@name=normalize-space(current()/text())]">
      <xsl:call-template name="outputMixed">
       <xsl:with-param name="newVal" select="/job/testing/var[@name=normalize-space(current()/text())]"/>
      </xsl:call-template>
     </xsl:when>
     <xsl:otherwise>
      <xsl:message terminate="yes">
       <xsl:text>Error: toNml.xsl, cannot find the value of the variable </xsl:text><xsl:value-of select="."/>
      </xsl:message>
     </xsl:otherwise>
    </xsl:choose>
   </xsl:when>
   <xsl:when test="local-name()='sep'">
    <xsl:value-of select="$DIR_SEP"/>
   </xsl:when>
   <xsl:otherwise>
    <xsl:value-of select="normalize-space(.)"/>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:for-each>
 <xsl:if test="@datatype='string'">
  <xsl:text>"</xsl:text>
 </xsl:if>
</xsl:template>

<xsl:template match="paramArray">
 <xsl:choose>
  <xsl:when test="$OutputParamArrayElementWise = 'false'">
  <xsl:text>
 </xsl:text>
   <xsl:value-of select="@name"/><xsl:text>=</xsl:text><xsl:apply-templates select="param" mode="element"/><xsl:text>,</xsl:text>
  </xsl:when>
  <xsl:otherwise>
   <xsl:apply-templates select="param"/><xsl:text>,</xsl:text>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="param" mode="element">
 <xsl:apply-templates select="value"/>
 <xsl:if test="position()!=last()">
  <xsl:text>    </xsl:text>
 </xsl:if>
</xsl:template>

<xsl:template name="outputMixed">
 <xsl:param name="newVal"/>
    <xsl:for-each select="$newVal">
     <xsl:choose>
      <xsl:when test="local-name()='varref'">
       <xsl:apply-templates select="."/>
      </xsl:when>
      <xsl:when test="local-name()='sep'">
       <xsl:apply-templates select="."/>
      </xsl:when>
      <xsl:otherwise>
       <xsl:value-of select="."/>
      </xsl:otherwise>
     </xsl:choose>
    </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
