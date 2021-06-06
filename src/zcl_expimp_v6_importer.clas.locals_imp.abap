*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_tool DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_byte_or_char_length
      IMPORTING
        type          TYPE zif_expimp_vx=>ty_ityp
        length        TYPE i
      RETURNING
        VALUE(result) TYPE i.
    CONSTANTS:
      c_ityp  LIKE zif_expimp_vx=>c_ityp VALUE zif_expimp_vx=>c_ityp.
ENDCLASS.

CLASS lcl_dd_table DEFINITION
    INHERITING FROM lcl_dd.
  PUBLIC SECTION.
    METHODS read REDEFINITION.
    METHODS get_rtti REDEFINITION.
    DATA:
      line_type    TYPE REF TO lcl_dd.
ENDCLASS.

CLASS lcl_dd_elementary DEFINITION ABSTRACT
  INHERITING FROM lcl_dd.
  PUBLIC SECTION.
    METHODS get_rtti REDEFINITION.
    DATA:
      type TYPE zif_expimp_vx=>ty_ityp,
      flen LIKE data_description-flen,
      decs LIKE data_description-decs.
ENDCLASS.

CLASS lcl_dd_from_header DEFINITION
  INHERITING FROM lcl_dd_elementary.
  PUBLIC SECTION.
    METHODS read REDEFINITION.
    METHODS get_type REDEFINITION.
    METHODS get_length REDEFINITION.
ENDCLASS.

CLASS lcl_dd_structure DEFINITION
  INHERITING FROM lcl_dd.
  PUBLIC SECTION.
    TYPES: ty_components TYPE STANDARD TABLE OF REF TO lcl_dd WITH EMPTY KEY.

    METHODS read REDEFINITION.
    METHODS get_rtti REDEFINITION.

    DATA:
      components TYPE TABLE OF REF TO lcl_dd.

ENDCLASS.

CLASS lcl_dd_simple DEFINITION
  INHERITING FROM lcl_dd_elementary.

  PUBLIC SECTION.

    METHODS read REDEFINITION.

ENDCLASS.

CLASS lcl_dv_string_xstring DEFINITION
    INHERITING FROM lcl_dv.
  PUBLIC SECTION.
    METHODS read REDEFINITION.
    METHODS get_value REDEFINITION.
    DATA:
      c_x_string_position TYPE i READ-ONLY,
      " number of characters or number of bytes
      c_x_string_length   TYPE i READ-ONLY.
ENDCLASS.

CLASS lcl_dv_simple DEFINITION
    INHERITING FROM lcl_dv.
  PUBLIC SECTION.
    METHODS read REDEFINITION.
    METHODS get_value REDEFINITION.
ENDCLASS.

CLASS lcl_dv_table DEFINITION
    INHERITING FROM lcl_dv.
  PUBLIC SECTION.
    METHODS read REDEFINITION.
    METHODS get_value REDEFINITION.
    DATA:
      lines TYPE TABLE OF REF TO lcl_dv.
ENDCLASS.

CLASS lcl_dv_interval DEFINITION
    INHERITING FROM lcl_dv.
  PUBLIC SECTION.
    METHODS read REDEFINITION.
    METHODS get_value REDEFINITION.
    DATA:
      interval_position TYPE i READ-ONLY,
      " number of bytes
      interval_length   TYPE i READ-ONLY.
ENDCLASS.

CLASS lcl_tool IMPLEMENTATION.

  METHOD get_byte_or_char_length.

    result = SWITCH #( type
        WHEN c_ityp-char OR c_ityp-date OR c_ityp-num OR c_ityp-string OR c_ityp-time
          THEN length / cl_abap_char_utilities=>charsize
        ELSE length ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dd IMPLEMENTATION.

  METHOD constructor.

    me->do = do.
    me->reader = do->reader.
    me->position = reader->get_position( ).

  ENDMETHOD.

  METHOD create.

    DATA(reader) = do->reader.
    DATA(id) = reader->get_current_byte( ).

    CASE id.

      WHEN c_dd_id-structure_include-start.
*        get_dd_structure_include( ).

      WHEN c_dd_id-boxed_component-start.
*        get_dd_boxed_component( ).

      WHEN c_dd_id-filler.
*        skip_dd_filler( ).

      WHEN c_dd_id-primitive.
        result = NEW lcl_dd_simple( do ).

      WHEN c_dd_id-structure-start.
        result = NEW lcl_dd_structure( do ).

      WHEN c_dd_id-table-start.
        result = NEW lcl_dd_table( do ).

      WHEN OTHERS.
        " The current byte is either a byte for "start of data value" or "end of data"
        " Data description is optional)
        result = NEW lcl_dd_from_header( do ).
    ENDCASE.

  ENDMETHOD.

  METHOD read.

    reader->read_structure( IMPORTING data = data_description ).
    start_id = data_description-id.

    result = me.

  ENDMETHOD.

  METHOD get_type.

    result = data_description-type.

  ENDMETHOD.

  METHOD get_length.

    result = lcl_tool=>get_byte_or_char_length( type = data_description-type length = CONV #( data_description-flen ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dd_from_header IMPLEMENTATION.

  METHOD read.

    type = do->object_header-ityp.
    flen = do->object_header-leng.
    decs = do->object_header-decs.

    result = me.

  ENDMETHOD.

  METHOD get_type.

    result = type.

  ENDMETHOD.

  METHOD get_length.

    result = lcl_tool=>get_byte_or_char_length( type = type length = CONV #( flen ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dd_elementary IMPLEMENTATION.

  METHOD get_rtti.

    TRY.
        CASE type.
          WHEN c_ityp-char.
            result = cl_abap_elemdescr=>get_c( SWITCH #( reader->get_encoding( ) WHEN '4102' OR '4103' THEN flen / 2 ELSE flen ) ).
          WHEN c_ityp-date.
            result = cl_abap_elemdescr=>get_d( ).
          WHEN c_ityp-decfloat16.
            result = cl_abap_elemdescr=>get_decfloat16( ).
          WHEN c_ityp-decfloat34.
            result = cl_abap_elemdescr=>get_decfloat34( ).
          WHEN c_ityp-float.
            result = cl_abap_elemdescr=>get_f( ).
          WHEN c_ityp-hex.
            result = cl_abap_elemdescr=>get_x( CONV #( flen ) ).
          WHEN c_ityp-int.
            result = cl_abap_elemdescr=>get_i( ).
          WHEN c_ityp-int1.
            result = cl_abap_elemdescr=>get_int1( ).
          WHEN c_ityp-int2.
            result = cl_abap_elemdescr=>get_int2( ).
          WHEN c_ityp-int8.
            result = cl_abap_elemdescr=>get_int8( ).
          WHEN c_ityp-num.
            result = cl_abap_elemdescr=>get_n( SWITCH #( reader->get_encoding( ) WHEN '4102' OR '4103' THEN flen / 2 ELSE flen ) ).
          WHEN c_ityp-packed.
            result = cl_abap_elemdescr=>get_p( p_length = CONV #( flen ) p_decimals = CONV #( decs ) ).
          WHEN c_ityp-string.
            result = cl_abap_elemdescr=>get_string( ).
          WHEN c_ityp-time.
            result = cl_abap_elemdescr=>get_t( ).
          WHEN c_ityp-xstring.
            result = cl_abap_elemdescr=>get_xstring( ).
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_expimp.
        ENDCASE.
      CATCH cx_parameter_invalid_range INTO DATA(lx).
        " error has occurred at GET_C, GET_N, GET_P or GET_X.
        RAISE EXCEPTION TYPE zcx_expimp EXPORTING previous = lx.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dd_table IMPLEMENTATION.

  METHOD read.
    DATA: data_description_end TYPE zif_expimp_v6=>ty_data_description.

    super->read( ).

    CASE reader->get_current_byte( ).
      WHEN c_dd_id-primitive.
        line_type = NEW lcl_dd_simple( do )->read( ).
        reader->read_structure( IMPORTING data = data_description_end ).
        IF data_description_end-id <> c_dd_id-table-end.
          RAISE EXCEPTION TYPE zcx_expimp.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    result = me.

  ENDMETHOD.

  METHOD get_rtti.

    result = cl_abap_tabledescr=>get( line_type->get_rtti( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dd_structure IMPLEMENTATION.

  METHOD read.

    DATA: data_description_end TYPE zif_expimp_v6=>ty_data_description.

    super->read( ).

    WHILE reader->get_current_byte( ) <> c_dd_id-structure-end.
      APPEND lcl_dd=>create( do )->read( ) TO components.
    ENDWHILE.

    reader->read_structure( IMPORTING data = data_description_end ).
    IF data_description_end-id <> c_dd_id-structure-end.
      RAISE EXCEPTION TYPE zcx_expimp.
    ENDIF.

    result = me.

  ENDMETHOD.

  METHOD get_rtti.

    DATA(rtti_components) = VALUE abap_component_tab(
        FOR <component> IN components INDEX INTO i
        ( name = |CMP{ i WIDTH = 4 ALIGN = RIGHT PAD = '0' }|
          type = <component>->get_rtti( ) ) ).

    result = cl_abap_structdescr=>get( p_components = rtti_components ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dd_simple IMPLEMENTATION.

  METHOD read.

    super->read( ).

    type = data_description-type.
    flen = data_description-flen.
    decs = data_description-decs.

    result = me.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dv IMPLEMENTATION.

  METHOD create.

    CASE do->reader->get_current_byte( ).
      WHEN c_dv_id-single.
        RAISE EXCEPTION TYPE zcx_expimp.
*        result = NEW lcl_dv_single( reader ).
        ASSERT 1 = 1. " debug helper
      WHEN c_dv_id-boxed_component-start.
        RAISE EXCEPTION TYPE zcx_expimp.
        ASSERT 1 = 1. " debug helper
      WHEN c_dv_id-string_xstring-start.
        result = NEW lcl_dv_string_xstring( do ).
        ASSERT 1 = 1. " debug helper
      WHEN c_dv_id-interval-start.
        result = NEW lcl_dv_interval( do ).
        ASSERT 1 = 1. " debug helper
      WHEN c_dv_id-table-start.
        result = NEW lcl_dv_table( do ).
        ASSERT 1 = 1. " debug helper
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_expimp.
    ENDCASE.

  ENDMETHOD.

  METHOD constructor.

    me->do = do.
    me->reader = do->reader.

  ENDMETHOD.

  METHOD read.

    reader->read_structure( IMPORTING data = data_value ).
    position = reader->get_position( ).
    length = lcl_tool=>get_byte_or_char_length( type = do->dd->get_type( ) length = conv #( data_value-len ) ).

    result = me.

  ENDMETHOD.

  METHOD get_structure.

    DATA(dd_struct) = CAST lcl_dd_structure( do->dd ).
    LOOP AT dd_struct->components INTO DATA(dd_component).
      ASSIGN COMPONENT sy-tabix OF STRUCTURE structure TO FIELD-SYMBOL(<component>).
      reader->read( EXPORTING n = dd_component->get_length( ) IMPORTING data = <component> ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dv_simple IMPLEMENTATION.

  METHOD read.

  ENDMETHOD.

  METHOD get_value.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dv_string_xstring IMPLEMENTATION.

  METHOD read.

    super->READ( ).

    position = reader->get_position( ).
    length = lcl_tool=>get_byte_or_char_length( type = do->dd->get_type( ) length = CONV #( data_value-len ) ).
    reader->skip_x( CONV #( data_value-len ) ).
    reader->skip_given_byte( c_dv_id-string_xstring-end ).

    result = me.

  ENDMETHOD.

  METHOD get_value.

    IF do->dd IS INSTANCE OF lcl_dd_structure.
      get_structure( IMPORTING structure = value ).
    ELSE.
      reader->reposition( position ).
      reader->read( EXPORTING n = length IMPORTING data = value ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dv_table IMPLEMENTATION.

  METHOD read.

    DATA:
      table_header   TYPE zif_expimp_v6=>ty_data_value_table,
      data_value_end TYPE zif_expimp_v6=>ty_data_value_end.

    reader->read_structure( IMPORTING data = table_header ).

    WHILE reader->get_current_byte( ) <> c_dv_id-table-end.
      APPEND lcl_dv=>create( do )->read( ) TO lines.
    ENDWHILE.

    reader->read_structure( IMPORTING data = data_value_end ).
    IF data_value_end-id <> c_dv_id-table-end.
      RAISE EXCEPTION TYPE zcx_expimp.
    ENDIF.

    result = me.

  ENDMETHOD.

  METHOD get_value.

    FIELD-SYMBOLS:
      <target_table> TYPE table.

    ASSIGN value TO <target_table>.

    LOOP AT lines INTO DATA(source_line).
      APPEND INITIAL LINE TO <target_table> ASSIGNING FIELD-SYMBOL(<target_line>).
      source_line->get_value( CHANGING value = <target_line> ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dv_interval IMPLEMENTATION.

  METHOD get_value.

    reader->reposition( interval_position ).
    IF do->dd IS INSTANCE OF lcl_dd_structure.
      get_structure( IMPORTING structure = value ).
    ELSE.
      reader->read( EXPORTING n = interval_length IMPORTING data = value ).
    ENDIF.

  ENDMETHOD.

  METHOD read.

    interval_position = reader->get_position( ).
    interval_length = data_value-len.
    reader->skip_x( interval_length ).
    reader->skip_given_byte( c_dv_id-interval-end ).

    result = me.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_do IMPLEMENTATION.

  METHOD read.

    IF reader->get_current_byte( ) = c_object_id-end_of_data.
      reader->skip_x( 1 ).
      IF reader->get_position( ) <> reader->length.
        RAISE EXCEPTION TYPE zcx_expimp.
      ENDIF.
      RAISE EXCEPTION TYPE zcx_expimp EXPORTING textid = zcx_expimp=>end_of_data_objects.
    ENDIF.

    reader->read_structure( IMPORTING data = object_header ).
    reader->read(
        EXPORTING n = CONV #( object_header-nlen )
        IMPORTING data = data_object_name ).
    data(last_position) = object_header-next - 1.

    dd = lcl_dd=>create( me )->read( ).

    WHILE reader->get_position( ) <= last_position.
      APPEND lcl_dv=>create( me )->read( ) TO dvs.
    endwhile.

    result = me.

  ENDMETHOD.

  METHOD create.

    result = NEW lcl_do( ).
    result->reader = reader.

  ENDMETHOD.

ENDCLASS.
