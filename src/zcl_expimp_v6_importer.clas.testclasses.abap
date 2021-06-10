*"* use this source file for your ABAP unit test classes

*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION DEFERRED.
CLASS zcl_expimp_v6_importer DEFINITION LOCAL FRIENDS ltc_main.

CLASS ltc_helper DEFINITION
      FOR TESTING.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        blob TYPE xstring.
    METHODS compare
      RAISING zcx_expimp.
    METHODS compare2
      IMPORTING
                partab2 TYPE tab_cpar
      RAISING   zcx_expimp.
    DATA ref_blob TYPE REF TO xstring.
ENDCLASS.

CLASS ltc_helper IMPLEMENTATION.

  METHOD compare.

    TRY.
        DATA(partab2) = cl_abap_expimp_utilities=>dbuf_import_create_data( ref_blob->* ).
      CATCH cx_sy_import_format_error INTO DATA(lx1).
        ASSERT 1 = 1. "handle exception
    ENDTRY.

*    TRY.
    DATA(partab_line) = NEW zcl_expimp_v6_importer( ref_blob->* )->get_next_data_object( ).
    DATA(partab) = VALUE tab_cpar( ( partab_line ) ).
*      CATCH cx_root INTO DATA(lx2).
*        DATA(b) = 0.
*        b = b + 1.
*        cl_abap_unit_assert=>fail( msg = 'error during custom IMPORT' ).
*    ENDTRY.

    " COMPARISON
    IF lines( partab2 ) <> lines( partab ).
      cl_abap_unit_assert=>fail( msg = 'are different' ).
    ENDIF.
    LOOP AT partab INTO DATA(cpar).
      DATA(cpar2) = VALUE #( partab2[ sy-tabix ] OPTIONAL ).
      ASSIGN cpar-dref->* TO FIELD-SYMBOL(<fs>).
      ASSIGN cpar2-dref->* TO FIELD-SYMBOL(<fs2>).
      IF cpar-name <> cpar2-name OR <fs> <> <fs2>.
        cl_abap_unit_assert=>fail( msg = 'are different' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD compare2.

*    TRY.
    DATA(partab_line) = NEW zcl_expimp_v6_importer( ref_blob->* )->get_next_data_object( ).
    DATA(partab) = VALUE tab_cpar( ( partab_line ) ).
*        DATA(partab) = NEW zcl_expimp_utilities( )->dbuf_import_create_data( CHANGING dbuf = ref_blob->* ).
*      CATCH cx_root INTO DATA(lx2).
*        cl_abap_unit_assert=>fail( msg = 'error during custom IMPORT' ).
*    ENDTRY.

    " COMPARISON
    IF lines( partab2 ) <> lines( partab ).
      cl_abap_unit_assert=>fail( msg = 'are different' ).
    ENDIF.
    LOOP AT partab INTO DATA(cpar).
      DATA(cpar2) = VALUE #( partab2[ sy-tabix ] OPTIONAL ).
      ASSIGN cpar-dref->* TO FIELD-SYMBOL(<fs>).
      ASSIGN cpar2-dref->* TO FIELD-SYMBOL(<fs2>).
      IF cpar-name <> cpar2-name OR <fs> <> <fs2>.
        cl_abap_unit_assert=>fail( msg = 'are different' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.

    ref_blob = REF #( blob ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    METHODS nested_structure FOR TESTING RAISING cx_static_check.
    METHODS filler FOR TESTING RAISING cx_static_check.
    METHODS deep_structure FOR TESTING RAISING cx_static_check.
    METHODS filler_flat_structure FOR TESTING RAISING cx_static_check.
    METHODS clike_flat_structure FOR TESTING RAISING cx_static_check.
    METHODS primitive_c FOR TESTING RAISING cx_static_check.
    METHODS primitive_i FOR TESTING RAISING cx_static_check.
    METHODS primitive_f FOR TESTING RAISING cx_static_check.
    METHODS primitive_string FOR TESTING RAISING cx_static_check.
    METHODS itab_without_structure FOR TESTING RAISING cx_static_check.
    METHODS itab_with_structure FOR TESTING RAISING cx_static_check.
    METHODS iso_8859_1 FOR TESTING RAISING cx_static_check.

    METHODS setup.

    DATA blob TYPE xstring.
    DATA helper TYPE REF TO ltc_helper.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD setup.
    helper = NEW ltc_helper( blob ).
  ENDMETHOD.

  METHOD primitive_c.

    DATA aa TYPE c LENGTH 2 VALUE 'AB'.
    EXPORT character-variable = aa TO DATA BUFFER blob.
    helper->compare( ).

  ENDMETHOD.

  METHOD primitive_i.

    EXPORT integer = 25 TO DATA BUFFER blob.
    helper->compare( ).

  ENDMETHOD.

  METHOD primitive_f.

    DATA f TYPE f.

    f = 25.
    EXPORT f = f TO DATA BUFFER blob.
    helper->compare( ).

  ENDMETHOD.

  METHOD primitive_string.

    EXPORT string = `Hello world` TO DATA BUFFER blob.
    helper->compare( ).

  ENDMETHOD.

  METHOD nested_structure.

    DATA: BEGIN OF struct,
            a TYPE i,
            BEGIN OF b,
              b TYPE decfloat16,
              c TYPE decfloat34,
              d TYPE int8,
            END OF b,
          END OF struct.

    struct = VALUE #( a = 25 b-b = 10 b-d = 30 ).
    EXPORT structure-variable = struct TO DATA BUFFER blob.
    helper->compare2( VALUE #(
        ( name = 'STRUCTURE-VARIABLE' dref = REF #( struct ) ) ) ).

  ENDMETHOD.

  METHOD filler.

    DATA: BEGIN OF struct,
            a TYPE i,
            b TYPE decfloat16,
          END OF struct.

    struct = VALUE #( a = 25 b = 10 ).
    EXPORT structure-variable = struct TO DATA BUFFER blob.
    helper->compare( ).

  ENDMETHOD.

  METHOD clike_flat_structure.

    DATA: BEGIN OF struct,
            aaa TYPE c LENGTH 5,
            bbb TYPE c LENGTH 5,
          END OF struct.

    struct = VALUE #( aaa = |hello| bbb = |world| ).
    EXPORT structure-variable = struct TO DATA BUFFER blob.
    helper->compare( ).

  ENDMETHOD.

  METHOD filler_flat_structure.

    DATA: BEGIN OF struct,
            aaa TYPE c LENGTH 5,
            bbb TYPE decfloat16,
          END OF struct.

    struct = VALUE #( aaa = |hello| bbb = 25 ).
    EXPORT structure-variable = struct TO DATA BUFFER blob.
    helper->compare( ).

  ENDMETHOD.

  METHOD deep_structure.

    DATA: BEGIN OF struct,
            aaa TYPE string,
            bbb TYPE string,
          END OF struct.

    struct = VALUE #( aaa = |hello| bbb = |world| ).
    EXPORT structure-variable = struct TO DATA BUFFER blob.
    helper->compare( ).

  ENDMETHOD.

  METHOD itab_without_structure.

    DATA: line TYPE string,
          itab LIKE TABLE OF line.

    EXPORT the-itab = itab TO DATA BUFFER blob.

    DATA(dump) = NEW zcl_expimp_v6_importer( blob )->_get_next_data_object( )->get_dump( ).

    cl_abap_unit_assert=>assert_equals( act = dump exp = VALUE string_table(
        ( |DATA OBJECT:| )
        ( |  HEADER:| )
        ( |    06  deep table| )
        ( |    13  string| )
        ( |    00  decimals| )
        ( |    00000008  length| )
        ( |    0000004F  next data object| )
        ( |    08  name length| )
        ( |    00000000  thash| )
        ( |    00000000000000000000000000000000  typid| )
        ( |  NAME:| )
        ( |    THE-ITAB| )
        ( |  DATA DESCRIPTION:| )
        ( |    AD  start of table| )
        ( |    13  string| )
        ( |    00  decimals| )
        ( |    00000008  length| )
        ( |  NO DATA VALUE| ) ) ).

  ENDMETHOD.

  METHOD itab_with_structure.

    DATA: BEGIN OF line,
            aaa TYPE string,
          END OF line,
          itab LIKE TABLE OF line.

    EXPORT the-itab = itab TO DATA BUFFER blob.

    DATA(dump) = NEW zcl_expimp_v6_importer( blob )->_get_next_data_object( )->get_dump( ).

    cl_abap_unit_assert=>assert_equals( act = dump exp = VALUE string_table(
        ( |DATA OBJECT:| )
        ( |  HEADER:| )
        ( |    06  deep table| )
        ( |    13  string| )
        ( |    00  decimals| )
        ( |    00000008  length| )
        ( |    0000004F  next data object| )
        ( |    08  name length| )
        ( |    00000000  thash| )
        ( |    00000000000000000000000000000000  typid| )
        ( |  NAME:| )
        ( |    THE-ITAB| )
        ( |  DATA DESCRIPTION:| )
        ( |    AD  start of table| )
        ( |    13  string| )
        ( |    00  decimals| )
        ( |    00000008  length| )
        ( |  NO DATA VALUE| ) ) ).

  ENDMETHOD.

  METHOD iso_8859_1.

    " EXPORT P_ACT_ID = 'SLSCN_COLLECT_DATA' (40C)
    blob = 'FF060201010280003131303000000000' " Transport header version 06 (16 bytes)
        && '0100000000002800000000080000000000000000000000000000000000000000' " Object header v06 (32 bytes)
        && '505F4143545F4944' " Object name : P_ACT_ID
        && 'BC00000028'
        && '534C53434E5F434F4C4C4543545F444154412020' " SLSCN_COLLECT_DATA (20 characters)
        && '2020202020202020202020202020202020202020' " spaces (20 characters)
        && 'BD04'.

    helper->compare( ).

  ENDMETHOD.

ENDCLASS.
