*&---------------------------------------------------------------------*
*& Report zfi_sz_reportingdata_export
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_sz_reportingdata_export.

PARAMETERS: prldnr TYPE acdoca-rldnr DEFAULT '0L', "Ledger in General Ledger Accounting
            pgjahr TYPE acdoca-gjahr DEFAULT '2024', "Fiscal Year
            pbudat TYPE acdoca-budat DEFAULT sy-datum, "Deadline Key Date
            pbukrs TYPE acdoca-rbukrs DEFAULT '3001', "Company Code
            pversn TYPE t011-versn DEFAULT 'ACOA', "Hierarchy for ACOA
            pversz TYPE t011-versn DEFAULT 'SZ', "Hierarchy for SZ
            paccon TYPE fac_cds_uh_node-nodelowvalue DEFAULT '39999999', "ACOA Node
            prrcty TYPE string LOWER CASE DEFAULT 'Actuals', "Fixed value for prrcty
            pversi TYPE string LOWER CASE DEFAULT '#', "Fixed value for pversi
            pmeins AS CHECKBOX DEFAULT space. "select meins and quantity or not
"! utility to build a subset of nodes based on a given key
CLASS lcl_node_search DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_nodes,
             node_id   TYPE fac_cds_uh_node-nodeid,
             node_pid  TYPE fac_cds_uh_node-nodeparentid,
             node_lval TYPE fac_cds_uh_node-nodelowvalue,
             entity    TYPE fac_cds_uh_node-nodeentity,
           END OF ts_nodes.
    TYPES: tt_nodes TYPE STANDARD TABLE OF ts_nodes.
    DATA: mt_subnodes TYPE tt_nodes.
    METHODS constructor IMPORTING nodes TYPE tt_nodes
                                  key   TYPE fac_cds_uh_node-nodelowvalue.
  PRIVATE SECTION.
    DATA: mt_nodes TYPE tt_nodes.
    "! finds the node with a given key
    "! @parameter key | key to search for
    "! @parameter node | resulting node
    METHODS _find_by_key IMPORTING key         TYPE fac_cds_uh_node-nodelowvalue
                         RETURNING VALUE(node) TYPE ts_nodes.
    "! builds the sub tree based on the given node
    "! @parameter node | node to use as root
    METHODS _build_subtree IMPORTING node TYPE ts_nodes.
ENDCLASS.
CLASS lcl_node_search IMPLEMENTATION.
  METHOD constructor.
    mt_nodes = nodes.
    _build_subtree( _find_by_key( key ) ).
  ENDMETHOD.
  METHOD _find_by_key.
    READ TABLE mt_nodes INTO node WITH KEY node_lval = key.
  ENDMETHOD.
  METHOD _build_subtree.
    INSERT node INTO TABLE mt_subnodes.
    LOOP AT mt_nodes INTO DATA(ls_nodes) WHERE node_pid = node-node_id.
      _build_subtree( ls_nodes ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
"! progam logic
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_parameters,
             prldnr TYPE acdoca-rldnr,
             pgjahr TYPE acdoca-gjahr,
             pbudat TYPE acdoca-budat,
             pbukrs TYPE acdoca-rbukrs,
             pversn TYPE t011-versn,
             pversz TYPE t011-versn,
             paccon TYPE fac_cds_uh_node-nodelowvalue,
             prrcty TYPE string,
             pversi TYPE string,
             pmeins TYPE abap_bool,
           END OF ts_parameters.
    METHODS: constructor IMPORTING is_parameters TYPE ts_parameters,
      run.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_data,
             rbukrs  TYPE acdoca-rbukrs,
             plant   TYPE acdoca-rbukrs,
             periv   TYPE acdoca-periv,
             ifr0    TYPE gbst_key,
             rfarea  TYPE acdoca-rfarea,
             sznum   TYPE string,
             "rcntr   TYPE acdoca-rcntr,
             "budat   TYPE acdoca-budat,
             poper   TYPE acdoca-poper,
             rhcur   TYPE acdoca-rhcur,
             runit   TYPE acdoca-runit,
             gjahr   TYPE acdoca-gjahr,
             rrcty   TYPE string, "Actuals
             version TYPE string,
             hsl     TYPE acdoca-hsl,
             msl     TYPE acdoca-msl,
           END OF ts_data.
    TYPES tt_data TYPE STANDARD TABLE OF ts_data WITH DEFAULT KEY.
    TYPES: BEGIN OF ts_sz_funcmap,
             rfarea TYPE acdoca-rfarea,
             parent TYPE string,
           END OF ts_sz_funcmap,
           BEGIN OF ts_account,
             racct  TYPE acdoca-racct,
             parent TYPE fac_cds_uh_node-nodelowvalue,
           END OF ts_account,
           tt_accounts   TYPE SORTED TABLE OF ts_account WITH UNIQUE KEY racct,
           tt_sz_funcmap TYPE SORTED TABLE OF ts_sz_funcmap WITH UNIQUE KEY rfarea.
    "runtime parameters
    DATA ms_parameters TYPE ts_parameters.
    "result table
    DATA mt_result TYPE tt_data.
    "! displays a given table
    "! @parameter ct_data | data table to display
    METHODS _alvdisplay CHANGING ct_data TYPE ANY TABLE.
    "! prepares and selects the data
    METHODS _process.
    "! reads the hierarchy given with ms_parameters-pversz from uhdt_vrsn and fac_cds_uh_node
    "! and builds the map from FUNCTIONALAREA to SZ Number
    "! @parameter rt_sz_funcmap | resulting map from FUNCTIONALAREA to SZ Number
    METHODS _prepareszfuncmap RETURNING VALUE(rt_sz_funcmap) TYPE tt_sz_funcmap.
    "! prepares the accounts that will be used in selection based on the hierarchy read from
    "! uhdt_vrsn and fac_cds_uh_node based on ms_parameters-pversn
    "! @parameter rt_accounts | table with the accounts based on hierarchy
    METHODS _prepareaccountsforselect RETURNING VALUE(rt_accounts) TYPE tt_accounts.
    "! performs post processing on the mt_result base on the SZ function map and
    "! sets fixed fields rrcty, version
    METHODS _postprocess_mt_result.
    "! select acdoca with meins and quantity
    METHODS _selectwithmeins.
    "! select acdoca without meins and quantity
    METHODS _selectwithoutmeins.
    METHODS _columnsettext
      IMPORTING
        column TYPE REF TO cl_salv_column
        text   TYPE string.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD constructor.
    ms_parameters = is_parameters.
  ENDMETHOD.
  METHOD run.
    _process(  ).
    _alvdisplay( CHANGING ct_data = mt_result ).
  ENDMETHOD.
  METHOD _alvdisplay.
    TRY.
        cl_salv_table=>factory( IMPORTING  r_salv_table     = DATA(lo_alv)
                                CHANGING   t_table          = ct_data ).
        lo_alv->get_layout( )->set_key( VALUE #( report = sy-repid ) ).
        lo_alv->get_layout( )->set_default( abap_true ).
        lo_alv->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
        lo_alv->get_functions( )->set_all( abap_true ).
        "columns
        DATA(columns) = lo_alv->get_columns( ).
        _columnsettext( column = columns->get_column( 'RBUKRS' ) text = 'Company Code' ).
        _columnsettext( column = columns->get_column( 'PLANT' ) text = 'Plant' ).
        _columnsettext( column = columns->get_column( 'PERIV' ) text = 'Fiscal Year Variant' ).
        _columnsettext( column = columns->get_column( 'IFR0' ) text = 'IFR0' ).
        _columnsettext( column = columns->get_column( 'RFAREA' ) text = 'Functional Area' ).
        _columnsettext( column = columns->get_column( 'SZNUM' ) text = 'Cost Center' ).
        _columnsettext( column = columns->get_column( 'POPER' ) text = 'Posting date' ).
        _columnsettext( column = columns->get_column( 'RHCUR' ) text = 'Currency Key LC' ).
        _columnsettext( column = columns->get_column( 'RUNIT' ) text = 'Unit of measure' ).
        _columnsettext( column = columns->get_column( 'GJAHR' ) text = 'Planning Year' ).
        _columnsettext( column = columns->get_column( 'RRCTY' ) text = 'Scenario' ).
        _columnsettext( column = columns->get_column( 'VERSION' ) text = 'Version' ).
        _columnsettext( column = columns->get_column( 'HSL' ) text = 'Amount' ).
        _columnsettext( column = columns->get_column( 'MSL' ) text = 'Quantity' ).
        columns->get_column( 'RFAREA' )->set_technical( abap_true ).
        lo_alv->display( ).
      CATCH cx_root INTO DATA(lo_error).
        MESSAGE lo_error->get_text( ) TYPE if_xo_const_message=>abort.
    ENDTRY.
  ENDMETHOD.
  METHOD _process.
    "get acdoca
    IF ms_parameters-pmeins = abap_true.
      _selectwithmeins(  ).
    ELSE.
      _selectwithoutmeins(  ).
    ENDIF.
    _postprocess_mt_result(  ).
  ENDMETHOD.

  METHOD _selectwithoutmeins.
    "prepare accounts
    DATA(lt_accounts) = _prepareaccountsforselect( ).
    "get acdoca
    SELECT rbukrs,
            rbukrs AS plant,
            periv,
            accounts~parent AS ifr0,
            rfarea,
            "rcntr,
            "budat,
            poper,
            rhcur,
            "runit,
            gjahr,
            "rrcty,
            "version,
            SUM( hsl ) AS hsl
            "SUM( msl ) AS msl
          FROM @lt_accounts AS accounts
             INNER JOIN acdoca AS acdoca ON acdoca~racct = accounts~racct
          WHERE rldnr  = @ms_parameters-prldnr
          AND   rbukrs = @ms_parameters-pbukrs
          AND   gjahr = @ms_parameters-pgjahr
          AND   budat <= @ms_parameters-pbudat
          AND   rrcty = 0  "actuals
          GROUP BY rbukrs, periv, accounts~parent, rfarea, poper, rhcur, runit, gjahr
          ORDER BY rbukrs, periv, ifr0
          INTO CORRESPONDING FIELDS OF TABLE @mt_result.
  ENDMETHOD.



  METHOD _selectwithmeins.
    "prepare accounts
    DATA(lt_accounts) = _prepareaccountsforselect( ).
    "get acdoca
    SELECT rbukrs,
                 rbukrs AS plant,
                 periv,
                 accounts~parent AS ifr0,
                 rfarea,
                 "rcntr,
                 "budat,
                 poper,
                 rhcur,
                 runit,
                 gjahr,
                 "rrcty,
                 "version,
                 SUM( hsl ) AS hsl,
                 SUM( msl ) AS msl
               FROM @lt_accounts AS accounts
                  INNER JOIN acdoca AS acdoca ON acdoca~racct = accounts~racct
               WHERE rldnr  = @ms_parameters-prldnr
               AND   rbukrs = @ms_parameters-pbukrs
               AND   gjahr = @ms_parameters-pgjahr
               AND   budat <= @ms_parameters-pbudat
               AND   rrcty = 0  "actuals
               GROUP BY rbukrs, periv, accounts~parent, rfarea, poper, rhcur, runit, gjahr
               ORDER BY rbukrs, periv, ifr0
               INTO CORRESPONDING FIELDS OF TABLE @mt_result.

  ENDMETHOD.



  METHOD _postprocess_mt_result.
    "get sz functional map
    DATA(lt_sz_funcmap) = _prepareszfuncmap( ).
    LOOP AT mt_result ASSIGNING FIELD-SYMBOL(<result>).
      READ TABLE lt_sz_funcmap INTO DATA(ls_sz) WITH KEY rfarea = <result>-rfarea.
      <result>-sznum = ls_sz-parent.
      <result>-rrcty = ms_parameters-prrcty.
      <result>-version = ms_parameters-pversi.
    ENDLOOP.
    SORT mt_result BY rbukrs plant periv ifr0 sznum poper.
  ENDMETHOD.



  METHOD _prepareszfuncmap.
    SELECT hierarchynodeset~nodeid AS node_id,
               hierarchynodeset~nodeparentid AS node_pid,
               hierarchynodeset~nodelowvalue AS node_lval,
               hierarchynodeset~nodeentity AS entity
        FROM uhdt_vrsn AS uhdt_vrsn INNER JOIN fac_cds_uh_node  AS hierarchynodeset ON hierarchynodeset~versionid = uhdt_vrsn~ver_id
        WHERE hier_hid = @ms_parameters-pversz AND ver_status = 'A'
        ORDER BY hierarchynodeset~nodeid ASCENDING
        INTO TABLE @DATA(sz_nodes).
    LOOP AT sz_nodes INTO DATA(ls_sz_nodes) WHERE entity = 'FUNCTIONALAREA'.
      READ TABLE sz_nodes INTO DATA(ls_sz_nodes_parent) WITH KEY node_id = ls_sz_nodes-node_pid.
      INSERT VALUE #( rfarea = ls_sz_nodes-node_lval parent = |{ ms_parameters-pbukrs }{ ls_sz_nodes_parent-node_lval }| ) INTO TABLE rt_sz_funcmap.
    ENDLOOP.

  ENDMETHOD.



  METHOD _prepareaccountsforselect.

    DATA lt_acoa_nodes TYPE STANDARD TABLE OF lcl_node_search=>ts_nodes.

    SELECT hierarchynodeset~nodeid AS node_id,
               hierarchynodeset~nodeparentid AS node_pid,
               hierarchynodeset~nodelowvalue AS node_lval,
               hierarchynodeset~nodeentity AS entity
        FROM uhdt_vrsn AS uhdt_vrsn INNER JOIN fac_cds_uh_node  AS hierarchynodeset ON hierarchynodeset~versionid = uhdt_vrsn~ver_id
        WHERE hier_hid = @ms_parameters-pversn AND ver_status = 'A'
        ORDER BY hierarchynodeset~nodeid ASCENDING
        INTO TABLE @lt_acoa_nodes.
    DATA(lo_acoa_nodes) = NEW lcl_node_search( nodes = lt_acoa_nodes key = ms_parameters-paccon ).
    DATA lt_accounts TYPE SORTED TABLE OF ts_account WITH UNIQUE KEY racct.
    LOOP AT lo_acoa_nodes->mt_subnodes INTO DATA(ls_acoa_nodes) WHERE entity = 'GLACCOUNT'.
      READ TABLE lo_acoa_nodes->mt_subnodes INTO DATA(ls_acoa_parent) WITH KEY node_id = ls_acoa_nodes-node_pid.
      INSERT VALUE #( racct = ls_acoa_nodes-node_lval parent = ls_acoa_parent-node_lval ) INTO TABLE rt_accounts.
    ENDLOOP.

  ENDMETHOD.



  METHOD _columnsettext.
    column->set_short_text( space ).
    column->set_medium_text( space ).
    column->set_long_text( CONV #( text ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW lcl( VALUE lcl=>ts_parameters( prldnr = prldnr
                                     pgjahr = pgjahr
                                     pbudat = pbudat
                                     pbukrs = pbukrs
                                     pversn = pversn
                                     pversz = pversz
                                     paccon = paccon
                                     prrcty = prrcty
                                     pversi = pversi
                                     pmeins = pmeins ) )->run(  ).
