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
      IMPORTING
        dbuf TYPE xstring
      RAISING zcx_expimp_vx.
    METHODS compare2
      IMPORTING
                partab2 TYPE tab_cpar
      RAISING   zcx_expimp_vx.
    DATA ref_blob TYPE REF TO xstring.
ENDCLASS.

CLASS ltc_helper IMPLEMENTATION.

  METHOD compare.

    TRY.
        DATA(partab2) = cl_abap_expimp_utilities=>dbuf_import_create_data( dbuf ).
      CATCH cx_sy_import_format_error INTO DATA(lx1).
        ASSERT 1 = 1. "handle exception
    ENDTRY.

*    TRY.
    DATA(partab_line) = NEW zcl_expimp_v6_importer( dbuf )->get_next_data_object( ).
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
    CLASS-METHODS class_setup.
    TYPES: BEGIN OF ty_tests,
          primitive_c TYPE zcl_expimp_vx_importer=>ty_test,
          primitive_i TYPE zcl_expimp_vx_importer=>ty_test,
          primitive_f TYPE zcl_expimp_vx_importer=>ty_test,
          primitive_string TYPE zcl_expimp_vx_importer=>ty_test,
          nested_structure TYPE zcl_expimp_vx_importer=>ty_test,
          filler TYPE zcl_expimp_vx_importer=>ty_test,
          clike_flat_structure TYPE zcl_expimp_vx_importer=>ty_test,
          filler_flat_structure TYPE zcl_expimp_vx_importer=>ty_test,
          deep_structure TYPE zcl_expimp_vx_importer=>ty_test,
          itab_without_structure TYPE zcl_expimp_vx_importer=>ty_test,
          itab_with_structure TYPE zcl_expimp_vx_importer=>ty_test,
           END OF ty_tests.
    class-DATA: tests TYPE ty_tests.

ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD setup.
    helper = NEW ltc_helper( blob ).
  ENDMETHOD.

  METHOD class_setup.
    tests-primitive_c = VALUE #(
        id   = 'primitive_c'
        code = VALUE #(
                ( `DATA variable TYPE c LENGTH 2 VALUE 'DD'.` ) )
        dbuf = CONV xstring( 'FF060201010280003431303300000000010000000000040000002C01000000000000000000000000'
            && '00000000000000004100BC0000000444004400BD04' ) ).
    tests-primitive_i = VALUE #(
        id = 'primitive_i'
        code = VALUE #(
                ( `DATA variable TYPE i VALUE 68.` ) )
        dbuf = CONV xstring( 'FF060201010280003431303300000000010800000000040000002C01000000000000000000000000'
            && '00000000000000004100BC0000000444000000BD04' ) ).
    tests-primitive_f = VALUE #(
        id   = 'primitive_f'
        code = VALUE #(
                ( `DATA variable TYPE f VALUE '3.14'.` ) )
        dbuf = CONV xstring( 'FF060201010280003431303300000000010700000000080000003001000000000000000000000000'
            && '00000000000000004100BC000000081F85EB51B81E0940BD04' ) ).
    tests-primitive_string = VALUE #(
        id   = 'primitive_string'
        code = VALUE #(
                ( `DATA variable TYPE string VALUE ``DD``.` ) )
        dbuf = CONV xstring( 'FF060201010280003431303300000000071300000000080000002C01000000000000000000000000'
            && '00000000000000004100CA0000000444004400CB04' ) ).
    tests-nested_structure = VALUE #(
        id   = 'nested_structure'
        code = VALUE #(
                ( `TYPES: BEGIN OF struct,   ` )
                ( `         a TYPE i,        ` )
                ( `         BEGIN OF sub,    ` )
                ( `           b TYPE i,      ` )
                ( `         END OF sub,      ` )
                ( `       END OF struct.     ` )
                ( `DATA variable TYPE struct.` ) )
        dbuf = CONV xstring( 'FF060201010280003431303300000000020E00000000080000005A01000000000000000000000000'
   && '00000000000000004100AB0E0000000008AA080000000004AB0E0000000004AA080000000004AC0E'
            && '0000000004AC0E0000000008BC000000080000000000000000BD04' ) ).
    tests-filler = VALUE #(
        id   = 'filler'
        code = VALUE #(
                ( `TYPES: BEGIN OF struct,    ` )
                ( `         a TYPE i,         ` )
                ( `         b TYPE decfloat16,` )
                ( `       END OF struct.      ` )
                ( `DATA variable TYPE struct. ` ) )
        dbuf = CONV xstring( 'FF060201010280003431303300000000020E00000000100000005B01000000000000000000000000'
            && '00000000000000004100AB0E0000000010AA080000000004AF040000000004AA170000000008AC0E'
            && '0000000010BC0000001000000000000000000000000000003822BD04' ) ).
    tests-clike_flat_structure = VALUE zcl_expimp_vx_importer=>ty_test(
*        id   = 'clike_flat_structure'
        code = VALUE #(
                ( `TYPES: BEGIN OF struct,    ` )
                ( `         a TYPE c LENGTH 5,` )
                ( `         b TYPE c LENGTH 5,` )
                ( `       END OF struct.      ` )
                ( `DATA variable TYPE struct. ` ) )
        dbuf = CONV xstring( 'FF060201010280003431303300000000020E00000000140000005801000000000000000000000000'
          && '00000000000000004100AB0E0000000014AA00000000000AAA00000000000AAC0E0000000014BC00'
            && '0000142000200020002000200020002000200020002000BD04' ) ).
    tests-filler_flat_structure = VALUE #(
        id   = 'filler_flat_structure'
        code = VALUE #(
                ( `TYPES: BEGIN OF struct,    ` )
                ( `         a TYPE c LENGTH 5,` )
                ( `         b TYPE decfloat16,` )
                ( `       END OF struct.      ` )
                ( `DATA variable TYPE struct. ` ) )
        dbuf = CONV xstring( 'FF060201010280003431303300000000020E00000000180000006301000000000000000000000000'
            && '00000000000000004100AB0E0000000018AA00000000000AAF040000000006AA170000000008AC0E'
            && '0000000018BC00000018200020002000200020000000000000000000000000003822BD04' ) ).
    tests-deep_structure = VALUE #(
        id   = 'deep_structure'
        code = VALUE #(
                ( `TYPES: BEGIN OF struct,` )
                ( `         a TYPE string,` )
                ( `         b TYPE string,` )
                ( `       END OF struct.  ` )
                ( `DATA variable TYPE struct.` ) )
        dbuf = CONV xstring( 'FF060201010280003431303300000000050F00000000100000004A01000000000000000000000000'
            && '00000000000000004100AB0F0000000010AA130000000008AA130000000008AC0F0000000010CA00'
            && '000000CBCA00000000CB04' ) ).
    tests-itab_without_structure = VALUE #(
        id   = 'itab_without_structure'
        code = VALUE #(
                ( `DATA variable TYPE TABLE OF string.` ) )
        dbuf = CONV xstring( 'FF060201010280003431303300000000061300000000080000004101000000000000000000000000'
            && '00000000000000004100AD130000000008AA130000000008AE130000000008BE0000000800000000'
            && 'BF04' ) ).
    tests-itab_with_structure = VALUE zcl_expimp_vx_importer=>ty_test(
        id   = 'itab_with_structure'
        code = VALUE #(
                ( `TYPES: BEGIN OF struct,` )
                ( `         a TYPE string,` )
                ( `       END OF struct.  ` )
                ( `DATA variable TYPE TABLE OF struct.` ) )
        dbuf = CONV xstring( 'FF060201010280003431303300000000060F00000000080000004101000000000000000000000000'
            && '00000000000000004100AD0F0000000008AA130000000008AE0F0000000008BE0000000800000000'
            && 'BF04' ) ).
  ENDMETHOD.

  METHOD primitive_c.

    helper->compare( tests-primitive_c-dbuf ).

  ENDMETHOD.

  METHOD primitive_i.

    helper->compare( tests-primitive_i-dbuf ).

  ENDMETHOD.

  METHOD primitive_f.

*    helper->compare( ).

  ENDMETHOD.

  METHOD primitive_string.

*    helper->compare( ).

  ENDMETHOD.

  METHOD nested_structure.

*    DATA: BEGIN OF struct,
*            a TYPE i,
*            BEGIN OF b,
*              b TYPE decfloat16,
*              c TYPE decfloat34,
*              d TYPE int8,
*            END OF b,
*          END OF struct.
*
*    struct = VALUE #( a = 25 b-b = 10 b-d = 30 ).
*    EXPORT structure-variable = struct TO DATA BUFFER blob.
*    helper->compare2( VALUE #(
*        ( name = 'STRUCTURE-VARIABLE' dref = REF #( struct ) ) ) ).

  ENDMETHOD.

  METHOD filler.

*    helper->compare( ).

  ENDMETHOD.

  METHOD clike_flat_structure.

*    helper->compare( ).

  ENDMETHOD.

  METHOD filler_flat_structure.

*    helper->compare( ).

  ENDMETHOD.

  METHOD deep_structure.

*    helper->compare( ).

  ENDMETHOD.

  METHOD itab_without_structure.

    DATA(dump) = NEW zcl_expimp_v6_importer( tests-itab_without_structure-dbuf )->_get_next_data_object( )->get_dump( ).

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

    " Below DBUF variable is generated from this code:
    "   TYPES: BEGIN OF struct,
    "            a TYPE string,
    "          END OF struct.
    "   DATA variable TYPE TABLE OF struct.
    "   EXPORT a = variable TO DATA BUFFER dbuf.
    DATA(dbuf) = CONV xstring( 'FF060201010280003431303300000000060F00000000080000004101000000000000000000000000'
            && '00000000000000004100AD0F0000000008AA130000000008AE0F0000000008BE0000000800000000'
            && 'BF04' ).

    DATA(dump) = NEW zcl_expimp_v6_importer( dbuf )->_get_next_data_object( )->get_dump( ).

    cl_abap_unit_assert=>assert_equals( act = dump exp = VALUE string_table(
        ( |DATA OBJECT:| )
        ( |  HEADER:| )
        ( |    06  deep table| )
        ( |    0F  struct2| )
        ( |    00  decimals| )
        ( |    00000008  length| )
        ( |    00000041  next data object| )
        ( |    01  name length| )
        ( |    00000000  thash| )
        ( |    00000000000000000000000000000000  typid| )
        ( |  NAME:| )
        ( |    A| )
        ( |  DATA DESCRIPTION:| )
        ( |    AD  start of table| )
        ( |    0F  struct2| )
        ( |    00  decimals| )
        ( |    00000008  length| )
        ( |  DATA DESCRIPTION:| )
        ( |    AA  primitive| )
        ( |    13  string| )
        ( |    00  decimals| )
        ( |    00000008  length| )
        ( |  DATA DESCRIPTION:| )
        ( |    AE  end of table| )
        ( |    0F  struct2| )
        ( |    00  decimals| )
        ( |    00000008  length| )
        ( |  DATA VALUE:| )
        ( |    BE  start of table| )
        ( |    00000008  length (8)| )
        ( |    00000000  number of lines (0)| )
        ( |    BF  end of table| ) ) ).

  ENDMETHOD.

  METHOD iso_8859_1.

    " EXPORT P_ACT_ID = 'SLSCN_COLLECT_DATA' (40C)

    helper->compare( CONV #( 'FF060201010280003131303000000000' " Transport header version 06 (16 bytes)
        && '0100000000002800000000080000000000000000000000000000000000000000' " Object header v06 (32 bytes)
        && '505F4143545F4944' " Object name : P_ACT_ID
        && 'BC00000028'
        && '534C53434E5F434F4C4C4543545F444154412020' " SLSCN_COLLECT_DATA (20 characters)
        && '2020202020202020202020202020202020202020' " spaces (20 characters)
        && 'BD04' ) ).

  ENDMETHOD.

ENDCLASS.
