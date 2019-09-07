#! /usr/bin/python

def translate_config(config_filename,script_name='translate_config.py'):
    #
    # Overview of inner workings of the script:
    # =========================================
    #
    # i) Parse old-style parameters from configuration file:
    #    - parse lines of format \s*<variable_name>\s*=\s*<value>.*,
    #                            \s*<variable_name>\s*=\s*'<value>'.*, or
    #                            \s*<variable_name>\s*=\s*"<value>".*
    #      (<variable_name> can't contain '=', whitespace, '\'', or '\"';
    #      unquoted <value>s can't contain whitespace, '\'', '\"', and '#'),
    #      and discard the remainder of such lines (e.g., additional comments
    #      following a '#')
    #    - remove '$(DEFINE)' in <value>
    #    - build list of key-value pairs in dictionary, after
    #      dictionary was set with defaults for module selection (which
    #      used to be selected by default)
    #         ma_flag_igcmatmos=.TRUE.
    #         ma_flag_fixedocean=.TRUE.
    #         ma_flag_fixedseaice=.TRUE.
    #         ma_flag_fixedicesheet=.TRUE.
    #
    # ii) Section by section loop through any possible parameter, remove
    # all matching parameters from dictionary and buid XML segment for the
    # corresponding section
    #
    # iii) write translated configuration
    #
    
    # Extract parameters from 'definition.xml' for various sections or
    # subsets and use translations to filter parameters from old-style
    # configuration for the individual sections
    # ================================================================
    #
    # Section i: 'flagname' attribute from all '/definition/config/model' elements
    #      translation: flag_* -> ma_flag_*
    #                   only model flags set to .true. or .TRUE. are copied into
    #                   translated configuration, while parameters set to
    #                   .false. or .FALSE. have to be absent in the translated
    #                   configuration
    #
    # Section ii: read 'name' attribute from all '/definition/build/make-arg' elements
    #      no translation required
    #
    # Section iii: read 'handle' attribute from all '/definition/build/macro' elements
    #      translation: value in the old configuration contains two parts,
    #      which have to be copied to the two different elements
    #      <identifier> and <replacement>
    #
    # Section iv: read 'name' attribute from all '/definition/testing/var' elements
    #      no translation required
    #
    # Section v: read 'name' attribute from '/definition/parameters/control/file';
    #           -> remove prefix 'data_', translate to <prefix>
    #      read 'name' attribute from all '/definition/parameters/control/file/namelist/param' elements
    #           -> <var>
    #      translation: if in list of exceptions replace accordingly,
    #                  otherwise <var> -> <prefix>_<var>
    #
    # Section vi: read 'name' attribute from '/definition/control';
    #           -> translate to <prefix>, look up <array_convention>
    #      read 'name' attribute from all '/definition/control/file/namelist/paramArray/param' elements
    #           -> <var>
    #      read 'dimension' attribute from all '/definition/control/file/namelist/paramArray' elements
    #           -> abort if >1
    #      read 'index' attribute from all '/definition/control/file/namelist/paramArray/param' elements
    #           -> n
    #      translation: if in list of exceptions replace accordingly,
    #                  otherwise construct <prefix>_<var><array_convention>n
    #
    # Section vii: read 'name' attribute from '/definition/parameter/model';
    #           -> translate to <prefix>
    #      read 'name' attribute from all '/definition/parameter/model/file/namelist/param' elements
    #           -> <var>
    #      translation: if in list of exceptions replace accordingly,
    #                  otherwise <var> -> <prefix>_<var>
    #
    # Section viii: read 'name' attribute from '/definition/model';
    #           -> translate to <prefix>, look up <array_convention>
    #      read 'name' attribute from all '/definition/model/file/namelist/paramArray/param' elements
    #           -> <var>
    #      read 'dimension' attribute from all '/definition/model/file/namelist/paramArray' elements
    #           -> abort if >1
    #      read 'index' attribute from all '/definition/model/file/namelist/paramArray/param' elements
    #           -> n
    #      translation: if in list of exceptions replace accordingly,
    #                  otherwise construct <prefix>_<var><array_convention>n
    
    # Translation rules:
    # ==================
    #
    # Prefixes of old-style parameter definitions:
    # --------------------------------------------
    # GENIE main control: 'ma_'
    # embm:               'ea_'
    # igcm:               'ia_'
    # fixed atmos:        'fa_'
    # fake atmos:         'fk_'
    # goldstein:          'go_'
    # slab ocean:         'so_'
    # fixed ocean:        'fo_'
    # goldstein sea ice:  'gs_'
    # slab sea ice:       'ss_'
    # fixed sea ice:      'fs_'
    # fixed ice sheet:    'fi_'
    # fixed chem:         'fc_'
    # ichem:              'ic_'
    # moses triffid:      'ml_'
    # ents:               'ents_'
    # GEM:                'gm_'
    # BIOGEM:             'bg_'
    # ATCHEM:             'ac_'
    # SEDGEM:             'sg_'
    # ROKGEM:             'rg_'
    # ECOGEM:             'eg_'
    # wind:               'wi_'
    #
    # Conventions used to set parameter arrays:
    # -----------------------------------------
    # igcm: <var>(n) -> <var>n
    # moses triffid: <var>(n) -> <var>n
    # gem,biogem,ecogem,atchem: <var>(n) -> <var>_n
    #
    # Non-standard parameter translations:
    # ------------------------------------
    # embm:
    #     indir_name->ea_1
    #     outdir_name->ea_2
    #     igrid->ea_grid
    #     world->ea_topo
    #     xu_wstress->ea_taux_u
    #     yu_wstress->ea_tauy_u
    #     xv_wstress->ea_taux_v
    #     yv_wstress->ea_tauy_v
    #     u_wspeed->ea_adv_u
    #     v_wspeed->ea_adv_v
    #     npstp->ea_3, 
    #     iwstp->ea_4, 
    #     itstp->ea_5
    #     ianav->ea_6
    #     ans->ea_7
    #     yearlen->ea_8
    #     nyear->ea_9
    #     ndta->ea_10
    #     scf->ea_11
    #     diffamp(1)->ea_12
    #     diffamp(2)->ea_13
    #     diffwid->ea_14
    #     difflin->ea_15
    #     betaz(1)->ea_16
    #     betaz(2)->ea_18
    #     betam(1)->ea_17
    #     betam(2)->ea_19
    #     tatm->ea_22
    #     relh0_ocean->ea_23
    #     relh0_land->ea_24
    #     extra1a->ea_25
    #     extra1b->ea_26
    #     extra1c->ea_27
    #     scl_fwf->ea_28
    #     tdatafile->ea_Tdata
    #     qdatafile->ea_Hdata
    #     lout->ea_29
    #     netin->ea_30
    #     netout->ea_31
    #     ascout->ea_32
    #     filenetin->ea_33
    #     dirnetout->ea_34
    #     lin->ea_35
    #     atchem_radfor->ea_36
    #     olr_adj0->olr_adj0
    #     ents_radfor->ea_37
    #     orbit_radfor->ea_38
    #     t_orbit->ea_ea_39
    #     norbit->ea_40
    #     orbitsteps->ea_41
    #     filenameorbit->ea_42
    # IGCM:
    #     lvar_albedo->ia_lvaralbedo
    # Fixed atmosphere:
    #     tair2m_flag->fa_tair_flag
    #     qair2m_flag->fa_qair_flag
    #     windu10m_flag->fa_windu_flag
    #     windv10m_flag->fa_windv_flag
    # GOLDSTEIN:
    #     indir_name->go_1
    #     outdir_name->go_2
    #     igrid->go_grid
    #     world->go_topo
    #     npstp->go_3
    #     iwstp->go_4
    #     itstp->go_5
    #     ianav->go_6
    #     conserv_per->go_6b
    #     ans->go_7
    #     yearlen->go_8
    #     nyear->go_9
    #     temp0->go_10
    #     temp1->go_11
    #     rel->go_12
    #     scf->go_13,                 
    #     diff(1)->go_14
    #     diff(2)->go_15,                               
    #     adrag->go_16
    #     fwanomin->go_fwanom
    #     cmip_model->go_fwanomfile,                              
    #     albocn->go_albedo,                               
    #     iconv->go_ocnconv
    #     tdatafile->go_Tdata
    #     sdatafile->go_Sdata
    #     lout->go_17,                                      
    #     netin->go_18,                                      
    #     netout->go_19,                                      
    #     ascout->go_20,                                      
    #     filenetin->go_21
    #     dirnetout->go_22
    #     lin->go_23
    # GOLDSTEIN sea ice:
    #     indir_name->gs_1
    #     outdir_name->gs_2
    #     igrid->gs_grid
    #     world->gs_topo
    #     npstp->gs_3
    #     iwstp->gs_4
    #     itstp->gs_5
    #     ianav->gs_6
    #     conserv_per->gs_6b
    #     ans->gs_7
    #     yearlen->gs_8
    #     nyear->gs_9
    #     diffsic->gs_11
    #     lout->gs_12
    #     netin->gs_13
    #     netout->gs_14
    #     ascout->gs_15
    #     filenetin->gs_16
    #     dirnetout->gs_17
    #     lin->gs_18
    # ENTS:
    #     indir_name->ents_1
    #     outdir_name->ents_2
    #     condir_name->ents_config
    #     ents_igrid->ents_grid
    #     ents_npstp->ents_3
    #     ents_iwstp->ents_4
    #     ents_itstp->ents_5
    #     ents_ianav->ents_6
    #     ents_restart->ents_7
    #     ents_yearlen->ents_8
    #     ents_out_name->ents_17
    #     ents_netin->ents_18
    #     ents_netout->ents_19
    #     ents_ascout->ents_20
    #     ents_filenetin->ents_21
    #     ents_dirnetout->ents_22
    #     ents_restart_file->ents_24
    #     ents_offlineswitch->ents_25
    
    prefixes = {'genie' : 'ma', 'embm' : 'ea', 'goldstein' : 'go', 'goldsteinseaice' : 'gs', 'plasim' : 'pl', 'ents' : 'el', 'GEM' : 'gm', 'biogem' : 'bg', 'ecogem' : 'eg', 'atchem' : 'ac', 'sedgem' : 'sg', 'rokgem' : 'rg', 'wind' : 'wi', 'gemlite' : 'gl', 'goldlite' : 'gi'}
    array_conventions = {'gm': '_',
                         'bg': '_',
                         'eg': '_',
                         'ac': '_'}
    exceptions_ea = {'indir_name': 'ea_1',
                     'outdir_name': 'ea_2',
                     'igrid': 'ea_grid',
                     'world': 'ea_topo',
                     'xu_wstress': 'ea_taux_u',
                     'yu_wstress': 'ea_tauy_u',
                     'xv_wstress': 'ea_taux_v',
                     'yv_wstress': 'ea_tauy_v',
                     'u_wspeed': 'ea_adv_u',
                     'v_wspeed': 'ea_adv_v',
                     'npstp': 'ea_3',
                     'iwstp': 'ea_4',
                     'itstp': 'ea_5',
                     'ianav': 'ea_6',
                     'ans': 'ea_7',
                     'yearlen': 'ea_8',
                     'nyear': 'ea_9',
                     'ndta': 'ea_10',
                     'scf': 'ea_11',
                     'diffamp(1)': 'ea_12',
                     'diffamp(2)': 'ea_13',
                     'diffwid': 'ea_14',
                     'difflin': 'ea_15',
                     'betaz(1)': 'ea_16',
                     'betaz(2)': 'ea_18',
                     'betam(1)': 'ea_17',
                     'betam(2)': 'ea_19',
                     'tatm': 'ea_22',
                     'relh0_ocean': 'ea_23',
                     'relh0_land': 'ea_24',
                     'extra1a': 'ea_25',
                     'extra1b': 'ea_26',
                     'extra1c': 'ea_27',
                     'scl_fwf': 'ea_28',
                     'tdatafile': 'ea_Tdata',
                     'qdatafile': 'ea_Hdata',
                     'lout': 'ea_29',
                     'netin': 'ea_30',
                     'netout': 'ea_31',
                     'ascout': 'ea_32',
                     'filenetin': 'ea_33',
                     'dirnetout': 'ea_34',
                     'lin': 'ea_35',
                     'atchem_radfor': 'ea_36',
                     'olr_adj0': 'olr_adj0',
                     'ents_radfor': 'ea_37',
                     'orbit_radfor': 'ea_38',
                     't_orbit': 'ea_39',
                     'norbit': 'ea_40',
                     'orbitsteps': 'ea_41',
                     'filenameorbit': 'ea_42'}
    exceptions_go = {'indir_name': 'go_1',
                     'outdir_name': 'go_2',
                     'igrid': 'go_grid',
                     'world': 'go_topo',
                     'npstp': 'go_3',
                     'iwstp': 'go_4',
                     'itstp': 'go_5',
                     'ianav': 'go_6',
                     'conserv_per': 'go_6b',
                     'ans': 'go_7',
                     'yearlen': 'go_8',
                     'nyear': 'go_9',
                     'temp0': 'go_10',
                     'temp1': 'go_11',
                     'rel': 'go_12',
                     'scf': 'go_13',
                     'diff(1)': 'go_14',
                     'diff(2)': 'go_15',
                     'adrag': 'go_16',
                     'fwanomin': 'go_fwanom',
                     'cmip_model': 'go_fwanomfile',
                     'albocn': 'go_albedo',
                     'iconv': 'go_ocnconv',
                     'tdatafile': 'go_Tdata',
                     'sdatafile': 'go_Sdata',
                     'lout': 'go_17',
                     'netin': 'go_18',
                     'netout': 'go_19',
                     'ascout': 'go_20',
                     'filenetin': 'go_21',
                     'dirnetout': 'go_22',
                     'lin': 'go_23'}
    exceptions_gs = {'indir_name': 'gs_1',
                     'outdir_name': 'gs_2',
                     'igrid': 'gs_grid',
                     'world': 'gs_topo',
                     'npstp': 'gs_3',
                     'iwstp': 'gs_4',
                     'itstp': 'gs_5',
                     'ianav': 'gs_6',
                     'conserv_per': 'gs_6b',
                     'ans': 'gs_7',
                     'yearlen': 'gs_8',
                     'nyear': 'gs_9',
                     'diffsic': 'gs_11',
                     'lout': 'gs_12',
                     'netin': 'gs_13',
                     'netout': 'gs_14',
                     'ascout': 'gs_15',
                     'filenetin': 'gs_16',
                     'dirnetout': 'gs_17',
                     'lin': 'gs_18'}
    exceptions_el = {'indir_name': 'el_1',
                       'outdir_name': 'el_2',
                       'condir_name': 'el_config',
                       'ents_igrid': 'el_grid',
                       'ents_npstp': 'el_3',
                       'ents_iwstp': 'el_4',
                       'ents_itstp': 'el_5',
                       'ents_ianav': 'el_6',
                       'ents_restart': 'el_7',
                       'ents_yearlen': 'el_8',
                       'ents_out_name': 'el_17',
                       'ents_netin': 'el_18',
                       'ents_netout': 'el_19',
                       'ents_ascout': 'el_20',
                       'ents_filenetin': 'el_21',
                       'ents_dirnetout': 'el_22',
                       'ents_restart_file': 'el_24',
                       'ents_offlineswitch': 'el_25'}
    exceptions = { 'ea': exceptions_ea,
                   'go': exceptions_go,
                   'gs': exceptions_gs,
                   'el': exceptions_el }
    
    # Parse old-style parameters from configuration file:
    #    - parse lines of format \s*<variable_name>\s*=\s*<value>.*,
    #                            \s*<variable_name>\s*=\s*'<value>'.*, or
    #                            \s*<variable_name>\s*=\s*"<value>".*
    #      (<variable_name> can't contain '=', whitespace, '\'', or '\"';
    #      unquoted <value>s can't contain whitespace, '\'', '\"', and '#'),
    #      and discard the remainder of such lines (e.g., additional comments
    #      following a '#')
    #    - remove '$(DEFINE)' in <value>
    #    - build list of key-value pairs in dictionary, after
    #      dictionary was set with defaults for module selection (which
    #      used to be selected by default)
    #         ma_flag_igcmatmos=.TRUE.
    #         ma_flag_fixedocean=.TRUE.
    #         ma_flag_fixedseaice=.TRUE.
    #         ma_flag_fixedicesheet=.TRUE.
    import sys
    import os.path
    import re
    
    # ensure that the arg is a genuine file
    if not os.path.isfile(config_filename):
        print "Error: File %s not found!" % configname
        sys.exit(1)
    else:
        re_key_val_1 = re.compile(r'^\s*([^\s\'\"]+?)\s*=\s*\'([^\']*)\'')
        re_key_val_2 = re.compile(r'^\s*([^\s\'\"]+?)\s*=\s*\"([^\"]*)\"')
        re_key_val_3 = re.compile(r'^\s*([^\s\'\"]+?)\s*=\s*([^\'\"\s\#]*)')
    # set default model configuration implicitely used in the old
    # configuration scheme
        old_config = {}
        config_file = open(config_filename)
        while 1:
            line = config_file.readline()
            if not line:
                break
    # remove newlines
            line = line.strip()
    # remove comment (includes lines starting with '#' and 'echo') and
    # empty lines
            if line.startswith('#') or line.startswith('echo') or (line == ''):
                continue
            key_val = re_key_val_1.match(line)
            if key_val is None:
                key_val = re_key_val_2.match(line)
                if key_val is None:
                    key_val = re_key_val_3.match(line)
                    if key_val is None:
                        continue
            key, val = key_val.group(1), key_val.group(2)
    # remove '$(DEFINE)'
            val = val.replace('$(DEFINE)','')
    # create dictionary from key-value pairs
            old_config[key] = val
        config_file.close()

    # Read 'definition.xml' and extract parameters for various sections or
    # subsets
    # ====================================================================
    
    # Use DOM approach to extract parameters
    import xml.dom.minidom
    
    dom_definition = xml.dom.minidom.parse('./src/xml-config/xml/definition.xml')
    
    # Helper functions to extract nodes and attributes
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
    
    # Section i: 'name' attribute from all '/var/vars' elements
    def get_vars(dom_definition):
        elements = select_elements(dom_definition,['definition','vars','var'])
        attribute_values = get_attribute_values(elements,'name')
        return attribute_values
    
    # Section i: 'flagname' attribute from all '/definition/config/model' elements
    def get_config_model_flags(dom_definition):
        elements = select_elements(dom_definition,['definition','config','model'])
        model_flags = {}
        for element in elements:
            flag = get_attribute_values([element],'flagname')[0]
            model = get_attribute_values([element],'name')[0]
            model_flags[flag] = model
        return model_flags
    
    # Section ii: read 'name' attribute from all '/definition/build/make-arg' elements
    def get_build_make_args(dom_definition):
        elements = select_elements(dom_definition,['definition','build','make-arg'])
        attribute_values = get_attribute_values(elements,'name')
        return attribute_values
    
    # Section iii: read 'handle' attribute from all '/definition/build/macro' elements
    def get_build_macros(dom_definition):
        elements = select_elements(dom_definition,['definition','build','macro'])
        attribute_values = get_attribute_values(elements,'handle')
        return attribute_values
    
    # Section iv: read 'name' attribute from all '/definition/testing/var' elements
    def get_testing_vars(dom_definition):
        elements = select_elements(dom_definition,['definition','testing','var'])
        attribute_values = get_attribute_values(elements,'name')
        return attribute_values
    
    # Section v: read 'name' attribute from '/definition/parameters/control/file';
    #           -> remove prefix 'data_', translate to <prefix>
    #      read 'name' attribute from all '/definition/parameters/control/file/namelist/param' elements
    #           -> <var>
    #      translation: if in list of exceptions replace accordingly,
    #                  otherwise <var> -> <prefix>_<var>
    def get_control_params(dom_definition):
        elements = select_elements(dom_definition,['definition','parameters','control','file'])
        control_params = {}
        for element in elements:
            filename = get_attribute_values([element],'name')[0]
            name = filename.split('_')[1]
            params = select_elements(element,['namelist','param'])
            param_names = get_attribute_values(params,'name')
            control_params[name] = param_names
        return control_params
    
    # Section vi: read 'name' attribute from '/definition/control';
    #           -> translate to <prefix>, look up <array_convention>
    #      read 'name' attribute from all '/definition/control/file/namelist/paramArray/param' elements
    #           -> <var>
    #      read 'dimension' attribute from all '/definition/control/file/namelist/paramArray' elements
    #           -> abort if >1
    #      read 'index' attribute from all '/definition/control/file/namelist/paramArray/param' elements
    #           -> n
    #      translation: if in list of exceptions replace accordingly,
    #                  otherwise construct <prefix>_<var><array_convention>n
    def get_control_param_arrays(dom_definition):
        elements = select_elements(dom_definition,['definition','parameters','control','file'])
        control_params = {}
        for element in elements:
            filename = get_attribute_values([element],'name')[0]
            name = filename.split('_')[1]
            param_arrays = select_elements(element,['namelist','paramArray'])
            arrays = {}
            for param_array in param_arrays:
                param_array_name = get_attribute_values([param_array],'name')[0]
                params = select_elements(param_array,['param'])
                param_indices = get_attribute_values(params,'index')
                arrays[param_array_name] = param_indices
            control_params[name] = arrays
        return control_params
    
    # Section vii: read 'name' attribute from '/definition/parameter/model';
    #           -> translate to <prefix>
    #      read 'name' attribute from all '/definition/parameter/model/file/namelist/param' elements
    #           -> <var>
    #      translation: if in list of exceptions replace accordingly,
    #                  otherwise <var> -> <prefix>_<var>
    def get_model_params(dom_definition):
        elements = select_elements(dom_definition,['definition','parameters','model'])
        control_params = {}
        for element in elements:
            name = get_attribute_values([element],'name')[0]
            params = select_elements(element,['file','namelist','param'])
            param_names = get_attribute_values(params,'name')
            control_params[name] = param_names
        return control_params
    
    # Section viii: read 'name' attribute from '/definition/model';
    #           -> translate to <prefix>, look up <array_convention>
    #      read 'name' attribute from all '/definition/model/file/namelist/paramArray/param' elements
    #           -> <var>
    #      read 'dimension' attribute from all '/definition/model/file/namelist/paramArray' elements
    #           -> abort if >1
    #      read 'index' attribute from all '/definition/model/file/namelist/paramArray/param' elements
    #           -> n
    #      translation: if in list of exceptions replace accordingly,
    #                  otherwise construct <prefix>_<var><array_convention>n
    def get_model_param_arrays(dom_definition):
        elements = select_elements(dom_definition,['definition','parameters','model'])
        model_params = {}
        for element in elements:
            name = get_attribute_values([element],'name')[0]
            param_arrays = select_elements(element,['file','namelist','paramArray'])
            arrays = {}
            for param_array in param_arrays:
                param_array_name = get_attribute_values([param_array],'name')[0]
                params = select_elements(param_array,['param'])
                param_indices = get_attribute_values(params,'index')
                arrays[param_array_name] = param_indices
            model_params[name] = arrays
        return model_params
    
    vars = get_vars(dom_definition)
    model_flags = get_config_model_flags(dom_definition)
    make_args = get_build_make_args(dom_definition)
    macros = get_build_macros(dom_definition)
    testing_vars = get_testing_vars(dom_definition)
    control_params = get_control_params(dom_definition)
    control_param_arrays = get_control_param_arrays(dom_definition)
    model_params = get_model_params(dom_definition)
    model_param_arrays = get_model_param_arrays(dom_definition)
    
    dom_definition.unlink()
    
    from xml.dom.minidom import getDOMImplementation
    from os.path import basename
    from datetime import date
    
    # Create XML document for output of translated configuration
    dom_config = getDOMImplementation()
    job = dom_config.createDocument(None, "job", None)
    job_element = job.documentElement
    job_element.setAttribute('creator',os.path.basename(script_name))
    job_element.setAttribute('date',date.today().strftime("%Y-%m-%d"))
    
    # Helper function to add text nodes, which includes the translation of
    # variables by varref elements, and '/' separators by '<sep/>'
    # elements. Variables are identified if written in either of the
    # following forms '${<VAR>}', '$VAR ', '$VAR/', '$VAR (if at the end
    # of the variable text).
    def translateAndAppendParameterValue(document,element,text):
        re_sep = re.compile(r'\/')
        re_var = re.compile(r'(.*?)\$({)?([^}\/]+)(})?(.*)')
        text_parts = re_sep.split(text)
        for n in range(len(text_parts)):
            if n>0:
                sep_element = document.createElement('sep')
                element.appendChild(sep_element)
            done = 0
            while not done:
                text_part = re_var.match(text_parts[n])
                if text_part is None:
                    done = 1
                    if len(text_parts[n]):
                        text_element = document.createTextNode(text_parts[n])
                        element.appendChild(text_element)
                else:
                    if len(text_part.group(1)):
                        text_element = document.createTextNode(text_part.group(1))
                        element.appendChild(text_element)
                    var_element = document.createElement('varref')
                    text_element = document.createTextNode(text_part.group(3))
                    var_element.appendChild(text_element)
                    element.appendChild(var_element)
                    text_parts[n] = text_part.group(5)
        return None
    
    # Apply translations and filter parameters from old-style
    # configuration for the individual sections to build a translated XML
    # config file
    
    
    # Section i: '/var/vars' elements
    #      no translation required
    vars_element = job.createElement('vars')
    for var in vars:
        old_config_key = var
        if old_config.has_key(old_config_key):
            element = job.createElement('var')
            element.setAttribute('name',var)
            translateAndAppendParameterValue(job,element,old_config[old_config_key])
            del old_config[old_config_key]
            vars_element.appendChild(element)
    job_element.appendChild(vars_element)
    
    # Section ii: '/definition/config/model' elements
    #      translation: flag_* -> ma_flag_*
    #                   only parameters set to .true. are copied into
    #                   translated configuration, while parameters set to
    #                   .false. have to be absent in the translated
    #                   configuration
    config_element = job.createElement('config')
    for model_flag in model_flags.keys():
        old_config_key = "_".join(['ma',model_flag])
        if old_config.has_key(old_config_key):
            if (old_config[old_config_key] == '.true.') or (old_config[old_config_key] == '.TRUE.'):
                element = job.createElement('model')
                element.setAttribute('name',model_flags[model_flag])
                config_element.appendChild(element)
            del old_config[old_config_key]
    # Append config element
    job_element.appendChild(config_element)
    
    # /job/build element needed for both sections iii and iv
    build_element = job.createElement('build')
    
    # Section iii: '/definition/build/make-arg' elements
    #      no translation required
    for make_arg in make_args:
        old_config_key = make_arg
        if old_config.has_key(old_config_key):
            element = job.createElement('make-arg')
            element.setAttribute('name',make_arg)
            translateAndAppendParameterValue(job,element,old_config[old_config_key])
            del old_config[old_config_key]
            build_element.appendChild(element)
    
    # Section iv: '/definition/build/macro' elements
    #      translation: value in the old configuration contains two parts,
    #      which have to be copied to the two different elements
    #      <identifier> and <replacement>
    for macro in macros:
        old_config_key = macro
        if old_config.has_key(old_config_key):
            element = job.createElement('macro')
            element.setAttribute('handle',macro)
            element.setAttribute('status','defined')
            identifier_element = job.createElement('identifier')
            replacement_element = job.createElement('replacement')
            identifier, replacement = old_config[old_config_key].split('=')
            translateAndAppendParameterValue(job,identifier_element,identifier)
            translateAndAppendParameterValue(job,replacement_element,replacement)
            element.appendChild(identifier_element)
            element.appendChild(replacement_element)
            build_element.appendChild(element)
            del old_config[old_config_key]
    
    # Append build element
    job_element.appendChild(build_element)
    
    # Section v: '/definition/testing/var' elements
    #      no translation required
    testing_element = job.createElement('testing')
    for testing_var in testing_vars:
        old_config_key = testing_var
        if old_config.has_key(old_config_key):
            element = job.createElement('var')
            element.setAttribute('name',testing_var)
            translateAndAppendParameterValue(job,element,old_config[old_config_key])
            del old_config[old_config_key]
            testing_element.appendChild(element)
    job_element.appendChild(testing_element)
    
    # /job/parameters element needed for all sections vi to ix
    parameters_element = job.createElement('parameters')
    
    # /job/parameters/control element needed for both sections vi to ix
    control_element = job.createElement('control')
    
    # Section vi: '/definition/parameters/control/file/namelist/param' elements
    #      translation: if in list of exceptions replace accordingly,
    #                  otherwise <var> -> <prefix>_<var>
    for key in control_params.keys():
        param_list = control_params[key]
        for param in param_list:
            if exceptions.has_key(prefixes[key]) and exceptions[prefixes[key]].has_key(param):
                old_param = exceptions[prefixes[key]][param]
            else:
                old_param = '_'.join([prefixes[key],param])
            if old_config.has_key(old_param):
                param_element = job.createElement('param')
                param_element.setAttribute('name',param)
                translateAndAppendParameterValue(job,param_element,old_config[old_param])
                del old_config[old_param]
                control_element.appendChild(param_element)
    parameters_element.appendChild(control_element)
    
    # Section vii: '/definition/control/file/namelist/paramArray/param' elements
    #      translation: if in list of exceptions replace accordingly,
    #                  otherwise construct <prefix>_<var><array_convention>n
    for key in control_param_arrays.keys():
        param_dict = control_param_arrays[key]
        for param in param_dict.keys():
            # only create paramArray once per parameter
            param_array_element_created = 0
            for index in param_dict[param]:
                if exceptions.has_key(prefixes[key]) and exceptions[prefixes[key]].has_key(param+'('+index+')'):
                    old_param = exceptions[prefixes[key]][param+'('+index+')']
                else:
                    old_param = '_'.join([prefixes[key],param])+array_conventions[prefixes[key]]+index
                if old_config.has_key(old_param):
                    if not param_array_element_created:
                        param_array_element = job.createElement('paramArray')
                        param_array_element.setAttribute('name',param)
                        param_array_element_created = 1
                    param_element = job.createElement('param')
                    param_element.setAttribute('index',index)
                    translateAndAppendParameterValue(job,param_element,old_config[old_param])
                    del old_config[old_param]
                    param_array_element.appendChild(param_element)
            if param_array_element_created:
                control_element.appendChild(param_array_element)
    parameters_element.appendChild(control_element)
    
    for key in set(model_params.keys()).union(model_param_arrays.keys()):

    # /job/parameters/model element needed for both sections viii to ix, but will only be created when really required
        model_element_created = 0
    
    # Section viii: '/definition/parameter/model/file/namelist/param' elements
    #      translation: if in list of exceptions replace accordingly,
    #                  otherwise <var> -> <prefix>_<var>
        if model_params.has_key(key):
            param_list = model_params[key]
            for param in param_list:
                if exceptions.has_key(prefixes[key]) and exceptions[prefixes[key]].has_key(param):
                    old_param = exceptions[prefixes[key]][param]
                else:
                    old_param = '_'.join([prefixes[key],param])
                if old_config.has_key(old_param):
                    if not model_element_created:
                        model_element = job.createElement('model')
                        model_element.setAttribute('name',key)
                        model_element_created = 1
                    param_element = job.createElement('param')
                    param_element.setAttribute('name',param)
                    translateAndAppendParameterValue(job,param_element,old_config[old_param])
                    del old_config[old_param]
                    model_element.appendChild(param_element)
    
    # Section ix: '/definition/model/file/namelist/paramArray/param' elements
    #      translation: if in list of exceptions replace accordingly,
    #                  otherwise construct <prefix>_<var><array_convention>n
        if model_param_arrays.has_key(key):
            param_dict = model_param_arrays[key]
            for param in param_dict.keys():
                # only create paramArray once per parameter
                param_array_element_created = 0
                for index in param_dict[param]:
                    if exceptions.has_key(prefixes[key]) and exceptions[prefixes[key]].has_key(param+'('+index+')'):
                        old_param = exceptions[prefixes[key]][param+'('+index+')']
                    else:
                        old_param = '_'.join([prefixes[key],param])+array_conventions[prefixes[key]]+index
                    if old_config.has_key(old_param):
                        if not model_element_created:
                            model_element = job.createElement('model')
                            model_element.setAttribute('name',key)
                            model_element_created = 1
                        if not param_array_element_created:
                            param_array_element = job.createElement('paramArray')
                            param_array_element.setAttribute('name',param)
                            param_array_element_created = 1
                        param_element = job.createElement('param')
                        param_element.setAttribute('index',index)
                        translateAndAppendParameterValue(job,param_element,old_config[old_param])
                        del old_config[old_param]
                        param_array_element.appendChild(param_element)
                if param_array_element_created:
                    model_element.appendChild(param_array_element)

        if model_element_created:
            parameters_element.appendChild(model_element)
    
    job_element.appendChild(parameters_element)
    xml_config = job.toprettyxml(encoding="UTF-8")

    # all matched key-value pairs have been removed from 'old_config'
    # as the translation has proceeded.
    if old_config.has_key('BUILDTEST_NAME'):
        del old_config['BUILDTEST_NAME']
    # If there are any entries left in 'old-config', then it is
    # an unrecognised setting and so the configuration file is
    # bugged (perhaps just a typo).
    # To be safe, lets halt with an error in that case
    if len(old_config) != 0:
        sys.stderr.write("Error: unrecognised settings in config file (perhaps a typo?):\n")
        sys.stderr.write(repr(old_config)+"\n")
        sys.exit(1)

    return xml_config

#
# end function translate_config
#

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 2:
        print "Usage: translate_config.py <old-style config file>"
        sys.exit(1)
    print translate_config(sys.argv[1],sys.argv[0])

