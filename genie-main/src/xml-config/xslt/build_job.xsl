<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!--
    CWA
    merge.xsl: merge a config doc into the definition doc and output the result.
    The approach is to iterate through the defintiion document and check to 
    see if the user has overridden an option in the config file. If so, output
    the value given in the config file rather than the definition default. If not,
    just copy the definition default into the output.

    SAM - 20100721
    made some additions to the functionality of the original
    'merge.xsl' stylesheet (history and model-state features) and
    separated it into two functional parts which are now two
    separate stylesheets ('apply_job_configuration.xsl' and
    'build_job.xsl' (this stylesheet))

    'apply_job_configuration.xsl' (this stylesheet) updates values in
    a "definition" or "pre-configured definition" document
    ('genie-main/src/xml-config/xml/definition.xml' is the default
    "definition" document) with values specified in a "job
    configuration" document and outputs a "pre-configured definition"
    document, which can be used to replace the previous "definition"
    or "pre-configured definition" document in subsequent applications
    of 'merge_config.xsl'.

    'build_job.xsl' converts the "pre-configured definition" into a "job"
    document.

-->

<xsl:param name="DATE" select="unknown"/>
<xsl:param name="CODEDIR" select="'~/genie'"/>
<xsl:param name="OUTROOT" select="'~/genie_output'"/>
<xsl:param name="RUNTIME_ROOT" select="'../..'"/>
<xsl:param name="RUNTIME_OUTDIR" select="'.'"/>

<xsl:output method="xml" indent="yes"/>

<!-- 
     Head template. This matches on the root of the definition document, tests whether the config
     file has been specified (exits if not) and then processes each child of the <definition> root in turn.
 -->
<xsl:template match="/" >
  <job creator="build_job.xsl" date="{$DATE}">
    <history>
      <xsl:copy>
	<xsl:copy-of select="definition/history/@*" />
      </xsl:copy>
      <entry action="build job" date="{$DATE}"/>
      <xsl:apply-templates select="definition/history/*"/>
    </history>
    <xsl:apply-templates select="definition/*"/>
  </job>
</xsl:template>

<!-- Match on <history> elements: Empty template deletes old history.
All of its children and attributes are used to build the updated
history element elsewhere -->
<xsl:template match="history"/>

<!-- Match on <job> elements: Empty template deletes old job element. -->
<xsl:template match="job"/>

<!--
     Match on <var> elements. <var> may appear in two places in the definition document: 
     as a child of <vars>, defining variables, or as a child of <testing>. Each occurrence 
     is handled separately. <vars><var> elements are handled in the first 'when' block
     and <testing><var> elements are handled in the second.
     <var>s here are mixed content, so call the 'outputMixed' template.
-->
<xsl:template match="var">
  <!-- Don't copy <vars><var> elements with the names 'OUTROOT',
       'CODEDIR', 'RUNTIME_ROOT', 'RUNTIME_ROOT' -->
  <xsl:if test="not(local-name(..)='vars' and (@name='OUTROOT' or @name='CODEDIR' or @name='RUNTIME_ROOT' or @name='RUNTIME_OUTDIR'))">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:call-template name="outputMixed">
	<xsl:with-param name="newVal" select="node()"/>
      </xsl:call-template>
    </xsl:copy>
  </xsl:if>
</xsl:template>

<xsl:template match="vars">
 <vars>
  <var name="OUTROOT"><xsl:value-of select="$OUTROOT"/></var>
  <var name="CODEDIR"><xsl:value-of select="$CODEDIR"/></var>
  <var name="RUNTIME_ROOT"><xsl:value-of select="$RUNTIME_ROOT"/></var>
  <var name="RUNTIME_OUTDIR"><xsl:value-of select="$RUNTIME_OUTDIR"/></var>
  <xsl:apply-templates select="var"/>
 </vars>
</xsl:template>

<!--
    This template matches all <model> elements but only outputs 
    if the model is listed in the <config> element of the config file.
    Applies to both <config><model> and <parameters><model> elements.
-->
<xsl:template match="model">
  <xsl:variable name="modelName" select="@name"/>
  <xsl:if test="/definition/config/model[@name=$modelName]/@state='on'">
    <model name="{@name}" type="{@type}" flagname="{@flagname}">
      <xsl:apply-templates select="*"/>
    </model>
  </xsl:if>
</xsl:template>

<!--
    Match namelists. Add model activation flags if this is the GENIE_CONTROL_NML namelist,
    otherwise, just copy the param and paramArray children.
-->
<xsl:template match="namelist">
  <xsl:copy>
    <xsl:copy-of select="@*" />
    <xsl:if test="@name='GENIE_CONTROL_NML'"><!--this is where the flags get added to the control namelist-->
      <xsl:for-each select="/definition/config/model">
	<param name="{@flagname}">
	  <xsl:choose>
	    <xsl:when test="@state='on'">
	      <value datatype="boolean">.true.</value>
	    </xsl:when>
	    <xsl:otherwise>
	      <value datatype="boolean">.false.</value>
	    </xsl:otherwise>
	  </xsl:choose>
	</param>
      </xsl:for-each>
    </xsl:if>
    <xsl:apply-templates select="*"/>
  </xsl:copy>
</xsl:template>

<!--
    If a new value has been passed down the call tree, output that new value,
    otherwise just output the value given in the definition file.
-->
<xsl:template match="value">
 <xsl:param name="newVal"/>
 <xsl:copy>
  <xsl:copy-of select="@*" />
  <xsl:choose>
   <xsl:when test="$newVal">
    <xsl:call-template name="outputMixed">
     <xsl:with-param name="newVal" select="$newVal"/>
    </xsl:call-template>
   </xsl:when>
   <xsl:otherwise>
    <xsl:for-each select="node()">
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
   </xsl:otherwise>
  </xsl:choose>
 </xsl:copy>
</xsl:template>

<!--
    This callable template outputs a path, which consist of mixed 
    content (text, <varref> and <sep> nodes). A path is passed through
    the 'newVal' parameter.
-->
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

<!--
    Catch-all template. Any nodes that are not specially handled are simply output
    in the form they are presented.
-->
<xsl:template match="node()">
   <xsl:copy>
    <xsl:copy-of select="@*" />
    <xsl:copy-of select="normalize-space(text())" />
    <xsl:apply-templates select="*"/>
   </xsl:copy>
</xsl:template>

</xsl:stylesheet>
