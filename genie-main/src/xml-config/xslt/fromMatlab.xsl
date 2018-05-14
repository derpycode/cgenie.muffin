<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

	<xsl:param name="mixedcontent" select="false()"/>

	<xsl:template match="node()">
		<xsl:apply-templates/>
	</xsl:template>

	<xsl:template match="/job">
		<xsl:element name="job">
			<xsl:apply-templates/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="/definition">
		<xsl:element name="definition">
			<xsl:apply-templates/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="/config">
		<xsl:element name="config">
			<xsl:apply-templates/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="author">
		<xsl:attribute name="author">
			<xsl:value-of select="normalize-space(.)"/>
		</xsl:attribute>
		<xsl:apply-templates/>
	</xsl:template>

	<xsl:template match="vars">
		<xsl:element name="vars">
			<xsl:for-each select="*">
				<xsl:call-template name="var">
					<xsl:with-param name="varname">
						<xsl:value-of select="local-name()"/>
					</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<xsl:template name="var">
		<xsl:param name="varname"/>
		<xsl:element name="var">
			<xsl:attribute name="name">
				<xsl:value-of select="$varname"/>
			</xsl:attribute>
			<xsl:choose>
				<xsl:when test="$mixedcontent='true'">
					<xsl:value-of disable-output-escaping="yes" select="normalize-space(.)"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="normalize-space(.)"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:element>
	</xsl:template>

	<xsl:template match="config">
		<xsl:element name="config">
			<xsl:for-each select="*">
				<xsl:call-template name="configmodel">
					<xsl:with-param name ="modelname">
						<xsl:value-of select="local-name()"/>
					</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<xsl:template name="configmodel">
		<xsl:param name="modelname"/>
		<xsl:element name="model">
			<xsl:attribute name="name">
				<xsl:value-of select="$modelname"/>
			</xsl:attribute>
			<xsl:for-each select="*">
				<xsl:variable name="nodename">
					<xsl:value-of select="local-name()"/>
				</xsl:variable>
				<xsl:variable name="nodevalue">
					<xsl:value-of select="normalize-space(.)"/>
				</xsl:variable>
				<xsl:attribute name="{$nodename}">
					<xsl:value-of select="$nodevalue"/>
				</xsl:attribute>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<xsl:template match="build">
		<xsl:element name="build">
			<xsl:for-each select="*">
				<xsl:choose>
					<xsl:when test="local-name()='make_arg'">
						<xsl:for-each select="*">
							<xsl:element name="make-arg">
								<xsl:attribute name="name">
									<xsl:value-of select="local-name()"/>
								</xsl:attribute>
								<xsl:value-of select="normalize-space(.)"/>
							</xsl:element>
						</xsl:for-each>
					</xsl:when>
					<xsl:when test="local-name()='macro'">
						<xsl:for-each select="*">
							<xsl:element name="macro">
								<xsl:attribute name="handle">
									<xsl:value-of select="local-name()"/>
								</xsl:attribute>
								<xsl:for-each select="*">
									<xsl:choose>
										<xsl:when test="local-name()='status'">
											<xsl:attribute name="status">
												<xsl:value-of select="normalize-space(.)"/>
											</xsl:attribute>
										</xsl:when>
										<xsl:otherwise>
											<xsl:variable name="elementname" select="local-name()"/>
											<xsl:element name="{$elementname}">
												<xsl:value-of select="normalize-space(.)"/>
											</xsl:element>
										</xsl:otherwise>
									</xsl:choose>
								</xsl:for-each>
							</xsl:element>
						</xsl:for-each>
					</xsl:when>
					<xsl:otherwise>
						<xsl:message>
							<xsl:text>Unrecognised element in the build structure</xsl:text>
						</xsl:message>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<xsl:template match="testing">
		<xsl:element name="testing">
			<xsl:for-each select="*">
				<xsl:call-template name="var">
					<xsl:with-param name="varname">
						<xsl:value-of select="local-name()"/>
					</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<xsl:template match="parameters">
		<xsl:element name="parameters">
			<xsl:for-each select="*">
				<xsl:choose>
					<xsl:when test="local-name()='control'">
						<xsl:call-template name="control"/>
					</xsl:when>
					<xsl:otherwise>
						<xsl:call-template name="model">
							<xsl:with-param name="modelname">
								<xsl:value-of select="local-name()"/>
							</xsl:with-param>
						</xsl:call-template>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<xsl:template name="control">
		<xsl:element name="control">
			<xsl:for-each select="*">
				<xsl:call-template name="file">
					<xsl:with-param name="filename">
						<xsl:value-of select="local-name()"/>
					</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<xsl:template name="model">
		<xsl:param name="modelname"/>
		<xsl:element name="model">
			<xsl:attribute name="name">
				<xsl:value-of select="$modelname"/>
			</xsl:attribute>
			<xsl:for-each select="*">
				<xsl:call-template name="file">
					<xsl:with-param name="filename">
						<xsl:value-of select="local-name()"/>
					</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<xsl:template name="file">
		<xsl:param name="filename"/>
		<xsl:element name="file">
			<xsl:attribute name="name">
				<xsl:value-of select="$filename"/>
			</xsl:attribute>
			<xsl:for-each select="*">
				<xsl:call-template name="namelist">
					<xsl:with-param name="namelistname">
						<xsl:value-of select="local-name()"/>
					</xsl:with-param>
				</xsl:call-template>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<!--	<xsl:template name="namelist">
		<xsl:param name="namelistname"/>
		<xsl:element name="namelist">
			<xsl:attribute name="name"><xsl:value-of select="$namelistname"/></xsl:attribute>
			<xsl:for-each select="*">
				<xsl:choose>
					<xsl:when test="not(@size='1 1') and not(@type='char')">
						<xsl:call-template name="paramArray">
							<xsl:with-param name="paramArrayname">
								<xsl:value-of select="local-name()"/>
							</xsl:with-param>
							<xsl:with-param name="paramArraytype">
								<xsl:value-of select="@type"/>
							</xsl:with-param>
							<xsl:with-param name="paramArraysize">
								<xsl:value-of select="substring-after(@size,' ')"/>
							</xsl:with-param>
						</xsl:call-template>
					</xsl:when>
					<xsl:otherwise>
						<xsl:call-template name="param">
							<xsl:with-param name="paramname">
								<xsl:value-of select="local-name()"/>
							</xsl:with-param>
							<xsl:with-param name="paramtype">
								<xsl:value-of select="@type"/>
							</xsl:with-param>
						</xsl:call-template>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:for-each>
		</xsl:element>
	</xsl:template> -->

	<xsl:template name="namelist">
		<xsl:param name="namelistname"/>
		<xsl:element name="namelist">
			<xsl:attribute name="name">
				<xsl:value-of select="$namelistname"/>
			</xsl:attribute>
			<xsl:for-each select="*">
				<xsl:choose>
					<xsl:when test="@type='struct'">
						<xsl:call-template name="paramArray">
							<xsl:with-param name="paramArrayName">
								<xsl:value-of select="local-name()"/>
							</xsl:with-param>
						</xsl:call-template>
					</xsl:when>
					<xsl:otherwise>
						<xsl:call-template name="param">
							<xsl:with-param name="paramname">
								<xsl:value-of select="local-name()"/>
							</xsl:with-param>
							<xsl:with-param name="paramtype">
								<xsl:value-of select="@type"/>
							</xsl:with-param>
						</xsl:call-template>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<xsl:template name="param">
		<xsl:param name="paramname"/>
		<xsl:param name="paramtype"/>
		<xsl:param name="index"/>
		<xsl:element name="param">
			<xsl:attribute name="name">
				<xsl:value-of select="$paramname"/>
			</xsl:attribute>
			<xsl:if test="not($index='')">
				<xsl:attribute name="index">
					<xsl:value-of select="$index"/>
				</xsl:attribute>
			</xsl:if>
			<xsl:for-each select="*">
				<xsl:choose>
					<xsl:when test="local-name()='value'">
						<xsl:element name="value">
							<!-- <xsl:attribute name="datatype"><xsl:value-of select="$paramtype"/></xsl:attribute> -->
							<xsl:call-template name="convertDatatype">
								<xsl:with-param name="datatype">
									<!-- <xsl:value-of select="$paramtype"/> -->
									<xsl:value-of select="@type"/>
								</xsl:with-param>
							</xsl:call-template>
							<xsl:choose>
								<xsl:when test="@type='boolean' or @type='logical'">
									<xsl:choose>
										<xsl:when test=".='1' or .='true'">.TRUE.</xsl:when>
										<xsl:when test=".='0' or .='false'">.FALSE.</xsl:when>
									</xsl:choose>
								</xsl:when>
								<xsl:otherwise>
									<xsl:choose>
										<xsl:when test="$mixedcontent='true'">
											<xsl:value-of disable-output-escaping="yes" select="normalize-space(.)"/>
										</xsl:when>
										<xsl:otherwise>
											<xsl:value-of select="normalize-space(.)"/>
										</xsl:otherwise>
									</xsl:choose>
								</xsl:otherwise>
							</xsl:choose>
						</xsl:element>
					</xsl:when>
					<xsl:otherwise>
						<xsl:call-template name="description"/>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<!--	<xsl:template name="paramArray">
		<xsl:param name="paramArrayname"/>
		<xsl:param name="paramArraytype"/>
		<xsl:param name="paramArraysize"/>
		<xsl:element name="paramArray">
			<xsl:attribute name="name"><xsl:value-of select="$paramArrayname"/></xsl:attribute>
			<xsl:attribute name="dimension">1</xsl:attribute>
			<xsl:attribute name="extent"><xsl:value-of select="$paramArraysize"/></xsl:attribute>
			<xsl:call-template name="paramArrayElement">
				<xsl:with-param name="string"><xsl:value-of select="."/></xsl:with-param>
				<xsl:with-param name="datatype"><xsl:value-of select="$paramArraytype"/></xsl:with-param>
				<xsl:with-param name="paramArrayname"><xsl:value-of select="$paramArrayname"/></xsl:with-param>
				<xsl:with-param name="index">1</xsl:with-param>
			</xsl:call-template>
		</xsl:element>
	</xsl:template> -->

	<xsl:template name="paramArray">
		<xsl:param name="paramArrayName"/>
		<xsl:element name="paramArray">
			<xsl:attribute name="name">
				<xsl:value-of select="$paramArrayName"/>
			</xsl:attribute>
			<xsl:attribute name="dimension">1</xsl:attribute>
			<xsl:attribute name="extent">
				<xsl:value-of select="substring-after(child::*[position()=last()]/attribute::size,'1 ')"/>
			</xsl:attribute>
			<xsl:for-each select="*">
				<xsl:choose>
					<xsl:when test="local-name()='description'">
						<xsl:call-template name="description"/>
					</xsl:when>
					<xsl:when test="local-name()=$paramArrayName">
						<xsl:for-each select="value">
							<xsl:call-template name="paramArrayEl">
								<xsl:with-param name="paramArrayName">
									<xsl:value-of select="$paramArrayName"/>
								</xsl:with-param>
								<xsl:with-param name="index">
									<xsl:value-of select="@idx"/>
								</xsl:with-param>
							</xsl:call-template>
						</xsl:for-each>
					</xsl:when>
				</xsl:choose>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<xsl:template name="paramArrayEl">
		<xsl:param name="paramArrayName"/>
		<xsl:param name="index"/>
		<xsl:element name="param">
			<xsl:attribute name="name">
				<xsl:value-of select="$paramArrayName"/>
				<xsl:text>(</xsl:text>
				<xsl:value-of select="$index"/>
				<xsl:text>)</xsl:text>
			</xsl:attribute>
			<xsl:attribute name="index">
				<xsl:value-of select="$index"/>
			</xsl:attribute>
			<xsl:element name="value">
				<xsl:call-template name="convertDatatype">
					<xsl:with-param name="datatype">
						<xsl:value-of select="@type"/>
					</xsl:with-param>
				</xsl:call-template>
				<xsl:choose>
					<xsl:when test="@type='boolean' or @type='logical'">
						<xsl:choose>
							<xsl:when test=".='1' or .='true'">.TRUE.</xsl:when>
							<xsl:when test=".='0' or .='false'">.FALSE.</xsl:when>
						</xsl:choose>
					</xsl:when>
					<xsl:otherwise>
						<xsl:choose>
							<xsl:when test="$mixedcontent='true'">
								<xsl:value-of disable-output-escaping="yes" select="normalize-space(.)"/>
							</xsl:when>
							<xsl:otherwise>
								<xsl:value-of select="normalize-space(.)"/>
							</xsl:otherwise>
						</xsl:choose>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:element>
			<xsl:for-each select="preceding-sibling::description[@idx=$index] | following-sibling::description[@idx=$index]">
				<xsl:call-template name="description"/>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<xsl:template name="paramArrayElement">
		<xsl:param name="string"/>
		<xsl:param name="datatype"/>
		<xsl:param name="paramArrayname"/>
		<xsl:param name="index"/>
		<xsl:element name="param">
			<xsl:attribute name="name">
				<xsl:value-of select="concat($paramArrayname, '(', $index, ')')"/>
			</xsl:attribute>
			<xsl:attribute name="index">
				<xsl:value-of select="$index"/>
			</xsl:attribute>
			<xsl:element name="value">
				<!-- <xsl:attribute name="datatype"><xsl:value-of select="$datatype"/></xsl:attribute> -->
				<xsl:call-template name="convertDatatype">
					<xsl:with-param name="datatype">
						<xsl:value-of select="$datatype"/>
					</xsl:with-param>
				</xsl:call-template>
				<xsl:choose>
					<xsl:when test="substring-before($string,' ')=''">
						<xsl:value-of select="$string"/>
					</xsl:when>
					<xsl:otherwise>
						<xsl:value-of select="substring-before($string,' ')"/>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:element>
		</xsl:element>
		<xsl:if test="not(substring-after($string,' ')='')">
			<xsl:call-template name="paramArrayElement">
				<xsl:with-param name="string">
					<xsl:value-of select="substring-after($string,' ')"/>
				</xsl:with-param>
				<xsl:with-param name="datatype">
					<xsl:value-of select="$datatype"/>
				</xsl:with-param>
				<xsl:with-param name="paramArrayname">
					<xsl:value-of select="$paramArrayname"/>
				</xsl:with-param>
				<xsl:with-param name="index">
					<xsl:value-of select="$index+1"/>
				</xsl:with-param>
			</xsl:call-template>
		</xsl:if>
	</xsl:template>

	<!-- Specify the mapping from Matlab datatypes to GENIE datatypes -->
	<xsl:template name="convertDatatype">
		<xsl:param name="datatype"/>
		<xsl:choose>
			<xsl:when test="$datatype='boolean' or $datatype='logical'">
				<xsl:attribute name="datatype">boolean</xsl:attribute>
			</xsl:when>
			<xsl:when test="$datatype='uint8' or $datatype='uint16' or $datatype='uint32' or $datatype='uint64' or $datatype='int8' or $datatype='int16' or $datatype='int32' or $datatype='int64'">
				<xsl:attribute name="datatype">integer</xsl:attribute>
			</xsl:when>
			<xsl:when test="$datatype='single' or $datatype='double'">
				<xsl:attribute name="datatype">real</xsl:attribute>
			</xsl:when>
			<xsl:otherwise>
				<xsl:attribute name="datatype">string</xsl:attribute>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template name="description">
		<xsl:element name="description">
			<!-- <xsl:attribute name="type">char</xsl:attribute>
			<xsl:attribute name="idx">1</xsl:attribute>
			<xsl:attribute name="size">
				<xsl:text>1 </xsl:text>
				<xsl:value-of select="string-length(normalize-space(.))"/>
			</xsl:attribute> -->
			<xsl:value-of select="normalize-space(.)"/>
		</xsl:element>
	</xsl:template>

</xsl:stylesheet>
