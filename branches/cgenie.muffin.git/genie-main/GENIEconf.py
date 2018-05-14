#! /usr/bin/python

#
# Functionality which allows to handle (query/set/remove) parameters
# in configuration files for GENIE (to be extended with functionality
# to handle global variables, build macros, and testing variables)
#
# Syntax of the main script: ./GENIEconf.py <command> <component>
# <parameter> [<value>] [<configuration file>} Currently accepted
# commands are of type 'get_param', 'get_config_param', and
# 'get_default_param'
#
# command category | command                       | returns
# =========================================================================================================================================
# get_param*       |                               | the value or description of a parameter
# -----------------------------------------------------------------------------------------------------------------------------------------
#                  | get_param                     | the value of a parameter if defined in the config file,
#                                                  | if not its default value if the parameter exists, error otherwise
#                  | get_param_config              | the value of a parameter in a config file, error if not existing
#                  | get_param_default             | the default value of a parameter, error if not existing
#                  | get_param_description         | the description of a parameter if defined in the config file,
#                                                  | if not its default description if the parameter exists, error otherwise
#                  | get_param_config_description  | the descriptive text of a parameter found in the config file,
#                                                  | error parameter exists in config file
#                  | get_param_default_description | the default description of a parameter, error if not existing
#                  | get_param_units               | the units of a parameter if defined in the config file,
#                                                  | if not its default units if the parameter exists, error otherwise
#                  | get_param_config_units        | the units of a parameter found in the config file,
#                                                  | error parameter exists in config file
#                  | get_param_default_units       | the default description of a parameter, error if not existing
# -----------------------------------------------------------------------------------------------------------------------------------------
# set_param        |                               | insert or reset a parameter
# -----------------------------------------------------------------------------------------------------------------------------------------
# insert_param     |                               | insert a new parameter, error if parameter already exists
# -----------------------------------------------------------------------------------------------------------------------------------------
# get_var (vars, make-arg, testing)
# -----------------------------------------------------------------------------------------------------------------------------------------
# set_var (vars, make-arg, testing)
# -----------------------------------------------------------------------------------------------------------------------------------------
#
# Currently accepted parameter components are either 'control' or
# 'model[<model name>]'
#
# Currently accepted var sections are either 'vars', 'make-arg',
# or 'testing'
#
# Parameters may be actual names of scalar/strin parameters or the
# actual names of 'paramArray' variables with the index of the
# selected element added as a suffixt in square brackes ([]). This
# newly implies that parameter names must not end in numbers in square
# brackets.
#
# If no configuration is specified for operations 'get_param',
# 'get_param_description', or 'get_param_units', then the default
# value specified in the 'defaults.xml' document is returned.
#

import sys
import os.path
import xml.dom.minidom
import re

##################
# Helper functions
##################

# read in DOM of document
def get_dom(document_file):
    # Test if config document exists
    if not os.path.isfile(document_file):
        return False
    # Read in config document as DOM
    return xml.dom.minidom.parse(document_file)

# select elements matching a hierarchical set of tag names
def select_elements(dom,names):
    """ returns a list of elements accoding to a list of hierarchical tag names, i.e. <names[0]><names[1]><names[2]>... """
    elements = [ dom ]
    for name in names:
        child_elements = []
        for element in elements:
            all_child_elements = element.childNodes
            for child_element in all_child_elements:
                if (child_element.nodeName == name):
                    child_elements.append(child_element)
        elements = child_elements
    return elements    

# extract a list of values of a named attribute from a list of
# elements
def get_attribute_values(elements,name):
    """ returns a list of attributes 'name' from a list of elements """
    attribute_values = []
    for element in elements:
        attributes = element.attributes
        for n in range(attributes.length):
            attribute = attributes.item(n)
            if attribute.name == name:
                attribute_values.append(attribute.value)
    return attribute_values

# test if parameter name contains an index and should be interpreted
# as an element of a 'paramArray' tag
def is_param_array(param_name):
    re_param_index = re.compile(r'^(.*)\[([0-9]+)\]$')
    param_index_match = re_param_index.match(param_name)
    if (param_index_match is None):
        return False
    else:
        param_index = [ param_index_match.group(1), param_index_match.group(2) ]
        return param_index

def split_component_name(component_name):
    # Test if name of component denotes a module or the central
    # 'control' section and return the corresponding 'model' or
    # 'control' element
    re_model_component = re.compile(r'^([^\[]*)\[([^\]]*)\]$')
    model_component_match = re_model_component.match(component_name)
    if (model_component_match):
        return [ model_component_match.group(1), model_component_match.group(2) ]
    else:
        return [ component_name, False ]

def extract_model_element(dom_config,model_name,definition):
    if (definition):
        path = ['definition','parameters','model']
    else:
        path = ['job','parameters','model']
    elements = select_elements(dom_config,path)
    element_names = get_attribute_values(elements,'name')
    if (model_name in element_names):
        return elements[element_names.index(model_name)]
    else:
        return False

def extract_control_element(dom_config,definition):
    # There can only be one 'control' element, take the first
    # found
    if (definition):
        path = ['definition','parameters','control']
    else:
        path = ['job','parameters','control']
    element = select_elements(dom_config,path)
    if (len(element) > 0):
        return element[0]
    else:
        return False

def extract_var_section_element(dom_config,element_name,definition):
    if (definition):
        path = ['definition',element_name]
    else:
        path = ['job',element_name]
    element = select_elements(dom_config,path)
    if (len(element) > 0):
        return element[0]
    else:
        return False

def extract_component_element(dom, component_name, definition):
    # Extract component element which has to be modified
    element = False
    [ component_type, component_name ] = split_component_name(component_name)
    if (component_type == 'model'):
        element = extract_model_element(dom,component_name,definition)
    elif (component_type == 'control'):
        element = extract_control_element(dom,definition)
    return element

def add_component_element(dom, component_name):
    # Create and return component element
    [ component_type, component_name ] = split_component_name(component_name)
    # add 'job' element unless there already
    job_element = select_elements(dom_config, [ 'job' ])
    if (not job_element):
        job_element = dom_config.createElement('job')
        dom_config.appendChild(job_element)
    # add 'parameters' element unless there already
    parameters_element = select_elements(dom_config, [ 'job', 'parameters' ])
    if (not parameters_element):
        parameters_element = dom_config.createElement('parameters')
        job_element[0].appendChild(parameters_element)
    if (component_type == 'model'):
        component_element = dom_config.createElement('model')
        component_element.setAttribute('name',component_name)
        parameters_element.appendChild(component_element)
    elif (component_type == 'control'):
        component_element = dom_config.createElement('control')
        parameters_element.appendChild(component_element)
    return component_element

def add_var_section_element(dom, section_name):
    # add 'job' element unless there already
    job_element = select_elements(dom_config, [ 'job' ])
    if (not job_element):
        job_element = dom_config.createElement('job')
        dom_config.appendChild(job_element)
    # add element unless there already
    element = select_elements(dom_config, [ 'job', section_name ])
    if (not element):
        element = dom_config.createElement(section_name)
        job_element[0].appendChild(element)
    return section_element

def extract_value(element):
    nodes = element.childNodes
    text = ""
    for node in nodes:
        if node.nodeType == node.TEXT_NODE:
            text = text + node.data.strip()
        else:
            text = text + node.toxml()
    return text

################
# Main functions
################

# Add variable (of different types) to a config file
def set_var(dom_config, dom_definition, section_name, var_name, var_value, overwrite=True):
    # Test if variable known
    if (get_var(dom_definition, section_name, var_name, True)):
        # Test if already existing in config
        if (get_var(dom_config, section_name, var_name, False)):
            if (overwrite):
                remove_var(dom_config, section_name, var_name)
            else:
                return False
        if (section_name == 'make-arg'):
            section_name = 'build'
            var_element_name = 'make-arg'
        else:
            var_element_name = 'var'
        element = extract_var_section_element(dom_config,section_name, False)
        # Add section section unless there already
        if (not element):
            element = add_var_section_element(dom_config,section_name)
        var_element = dom_config.createElement(var_element_name)
        var_element_value = dom_config.createTextNode(str(var_value))
        var_element.setAttribute('name',var_name)
        var_element.appendChild(var_element_value)
        element.appendChild(var_element)
        return dom_config
    else:
        print "Error: Variable '%s' does not exist!" % var_name
        return False

# Extract value of a var entry either from a config document or the
# definition document
def get_var(dom, section_name, var_name, definition=False):
    if (section_name == 'make-arg'):
        element_name = 'build'
        path = [ section_name ]
    else:
        element_name = section_name
        path = [ 'var' ]
    element = extract_var_section_element(dom,element_name,definition)
    if (not element):
        return False
    var_elements = select_elements(element,path)
    var_element_names = get_attribute_values(var_elements,'name')
    if (var_name in var_element_names):
        var_element = var_elements[var_element_names.index(var_name)]
    else:
        return False
    value = extract_value(var_element)
    return value

# Add parameter to a config file
def set_param(dom_config, dom_definition, component_name, param_name, param_value, overwrite=True):
    # Test if parameter known
    if (get_param(dom_definition, component_name, param_name, True)):
        # Test if already existing in config
        if (get_param(dom_config, component_name, param_name, False)):
            if (overwrite):
                remove_param(dom_config, component_name, param_name)
            else:
                return False
        element = extract_component_element(dom_config,component_name, False)
        # Add component section unless there already
        if (not element):
            element = add_component_element(dom_config,component_name)
        # Test if a 'paramArray' or a 'param' has to be added
        param_array = is_param_array(param_name)
        if (param_array):
            # Test if 'paramArray' element has to be created first
            param_name = param_array[0]
            path = ['paramArray']
            param_elements = select_elements(element,path)
            param_element_names = get_attribute_values(param_elements,'name')
            if (param_name in param_element_names):
                element = param_elements[param_element_names.index(param_name)]
            else:
                param_array_element = dom_config.createElement('paramArray')
                param_array_element.setAttribute('name',param_array[0])
                element.appendChild(param_array_element)
                element = param_array_element
            param_element = dom_config.createElement('param')
            param_element.setAttribute('index',param_array[1])
            param_element_value = dom_config.createTextNode(str(param_value))
            param_element.appendChild(param_element_value)
            element.appendChild(param_element)
        else:
            param_element = dom_config.createElement('param')
            param_element_value = dom_config.createTextNode(str(param_value))
            param_element.setAttribute('name',param_name)
            param_element.appendChild(param_element_value)
            element.appendChild(param_element)
        return dom_config
    else:
        print "Error: Parameter '%s' does not exist!" % param_name
        return False

# Remove parameter
def remove_param(dom, component_name, param_name):
    element = extract_component_element(dom, component_name, False)
    param_array = is_param_array(param_name)
    if (param_array):
        param_name = param_array[0]
        path = ['paramArray']
        param_elements = select_elements(element,path)
    else:
        path = ['param']
        param_elements = select_elements(element,path)
    param_element_names = get_attribute_values(param_elements,'name')
    if (param_name in param_element_names):
        param_element = param_elements[param_element_names.index(param_name)]
        if (param_array):
            param_array_elements = select_elements(param_element,['param'])
            param_array_element_indices = get_attribute_values(param_array_elements,'index')
            # if selected, extract description or units from 'paramArray'
            # before proceeding to the selected element of the array
            if (param_array[1] in param_array_element_indices):
                param_element = param_array_elements[param_array_element_indices.index(param_array[1])]
        parent_element = param_element.parentNode
        parent_element.removeChild(param_element)
        param_element.unlink()
    return dom

# Remove parameter
def remove_var(dom, section_name, var_name):
    if (section_name == 'make-arg'):
        section_name = 'build'
        path = ['make-arg']
    else:
        path = ['var']
    element = extract_var_section_element(dom,section_name,False)
    var_elements = select_elements(element,path)
    var_element_names = get_attribute_values(var_elements,'name')
    if (var_name in var_element_names):
        var_element = var_elements[var_element_names.index(var_name)]
        parent_element = var_element.parentNode
        parent_element.removeChild(var_element)
        var_element.unlink()
    return dom

# Extract parameter value or description either from a config document or the definition document
def get_param(dom, component_name, param_name, definition=False, extract='value'):
    element = extract_component_element(dom,component_name,definition)
    if (not element):
        return False
    # Test if an element of a 'paramArray' has to be looked up and
    # look up parameter
    param_array = is_param_array(param_name)
    if (param_array):
        param_name = param_array[0]
        if (definition):
            path = ['file','namelist','paramArray']
        else:
            path = ['paramArray']
        param_elements = select_elements(element,path)
    else:
        if (definition):
            path = ['file','namelist','param']
        else:
            path = ['param']
        param_elements = select_elements(element,path)
    param_element_names = get_attribute_values(param_elements,'name')
    if (param_name in param_element_names):
        param_element = param_elements[param_element_names.index(param_name)]
    else:
        return False
    value = ""
    if (param_array):
        param_array_elements = select_elements(param_element,['param'])
        param_array_element_indices = get_attribute_values(param_array_elements,'index')
        # if selected, extract description or units from 'paramArray'
        # before proceeding to the selected element of the array
        if (extract=='description'):
            value_element = select_elements(param_element,['description'])
            if (len(value_element)>0):
                value = extract_value(value_element[0])
                if (len(value)>0):
                    value = value+":\n"
        elif (extract=='units'):
            value_element = select_elements(param_element,['units'])
            if (len(value_element)>0):
                value = extract_value(value_element[0])
                if (len(value)>0):
                    value = value+":\n"
        # overwrite 'param_element' with selected element of
        #'paramArray'
        if (param_array[1] in param_array_element_indices):
            param_element = param_array_elements[param_array_element_indices.index(param_array[1])]
        else:
            return False
    # Extract value
    if ((definition) and (extract=='value')):
        value_element = select_elements(param_element,['value'])
        if (len(value_element) > 0):
            value_element = value_element[0]
        else:
            return False
    elif (extract=='description'):
        value_element = select_elements(param_element,['description'])
        if (len(value_element) > 0):
            value_element = value_element[0]
        else:
            return False
    elif (extract=='units'):
        value_element = select_elements(param_element,['units'])
        if (len(value_element) > 0):
            value_element = value_element[0]
        else:
            return False
    else:
        value_element = param_element
    value = value+extract_value(value_element)
    return value

# Extract parameter value from a config document
def get_param_config_value(dom, component_name, param_name):
    return get_param(dom, component_name, param_name, definition=False)

# Extract parameter value from the definition document
def get_param_default_value(dom, component_name, param_name):
    return get_param(dom, component_name, param_name, definition=True)

# Extract description for specified parameter from the config document
def get_param_config_description(dom, component_name, param_name):
    return get_param(dom, component_name, param_name, definition=False, extract='description')

# Extract description for specified parameter from the definition document
def get_param_default_description(dom, component_name, param_name):
    return get_param(dom, component_name, param_name, definition=True, extract='description')

# Extract units for specified parameter from the config document
def get_param_config_units(dom, component_name, param_name):
    return get_param(dom, component_name, param_name, definition=False, extract='units')

# Extract units for specified parameter from the definition document
def get_param_default_units(dom, component_name, param_name):
    return get_param(dom, component_name, param_name, definition=True, extract='units')

#################################################
# Wrapper if 'GENIEconf.py' is called as a script
#################################################
if __name__ == "__main__":
    import sys
# test for read command
    definition_file = 'src/xml-config/xml/definition.xml'
    dom_definition = get_dom(definition_file)
    if (len(sys.argv) < 2):
        print "Error: No command specified!"
        sys.exit(1)
    command = sys.argv[1]
    if (len(sys.argv) < 3):
        print "Error: No component/section specified!"
        sys.exit(1)
    component_name = sys.argv[2]
    section_name = component_name
    if (len(sys.argv) < 4):
        print "Error: No parameter name specified!"
        sys.exit(1)
    name = sys.argv[3]
    if (len(sys.argv) < 5):
        config_file = False
    else:
        if (len(sys.argv) > 5):
            config_file = sys.argv[5]
            value = sys.argv[4]
        else:
            config_file = sys.argv[4]
        dom_config = get_dom(config_file)
        if (not dom_config):
            implementation = xml.dom.minidom.getDOMImplementation()
            dom_config = implementation.createDocument(None,"job",None)
    if (command == 'get_param_config'):
        if (not config_file):
            dom_definition.unlink()
            sys.exit(1)
        result = get_param_config_value(dom_config,component_name,name)
    elif (command == 'get_param_default'):
        result = get_param_default_value(dom_definition,component_name,name)
    elif (command == 'get_param'):
        result = False
        if (config_file):
            result = get_param_config_value(dom_config,component_name,name)
        if (not result):
            result = get_param_default_value(dom_definition,component_name,name)
    elif (command == 'get_param_config_description'):
        if (not config_file):
            dom_definition.unlink()
            sys.exit(1)
        result = get_param_config_description(dom_config,component_name,name)
    elif (command == 'get_param_default_description'):
        result = get_param_default_description(dom_definition,component_name,name)
    elif (command == 'get_param_description'):
        result = False
        if (config_file):
            result = get_param_config_description(dom_config,component_name,name)
        if (not result):
            result = get_param_default_description(dom_definition,component_name,name)
    elif (command == 'get_param_config_units'):
        if (not config_file):
            dom_definition.unlink()
            sys.exit(1)
        result = get_param_config_value(dom_config,component_name,name)
    elif (command == 'get_param_default_units'):
        result = get_param_default_value(dom_definition,component_name,name)
    elif (command == 'get_param_units'):
        result = False
        if (config_file):
            result = get_param_config_units(dom_config,component_name,name)
        if (not result):
            result = get_param_default_units(dom_definition,component_name,name)
    elif (command == 'insert_param'):
        result = set_param(dom_config,dom_definition,component_name,name,value,False)
        if (result):
            result = result.toprettyxml()
    elif (command == 'set_param'):
        result = set_param(dom_config,dom_definition,component_name,name,value,True)
        if (result):
            result = result.toprettyxml()
    elif (command == 'get_var'):
        result = False
        if (config_file):
            result = get_var(dom_config,section_name,name,False)
        if (not result):
            result = get_var(dom_definition,section_name,name,True)
    elif (command == 'set_var'):
        result = set_var(dom_config,dom_definition,section_name,name,value,True)
        if (result):
            result = result.toprettyxml()
    else:
        print "Error: Command '%s' not available!" % sys.argv[1]
        if (config_file):
            dom_config.unlink()
        dom_definition.unlink()
        sys.exit(1)
    if (result):
        print result
        if (config_file):
            dom_config.unlink()
        dom_definition.unlink()
        sys.exit(0)
    else:
        if (config_file):
            dom_config.unlink()
        dom_definition.unlink()
        sys.exit(1)
