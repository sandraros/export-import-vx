*&---------------------------------------------------------------------*
*& Report ZEXPIMP_GENERATE_TEST_CODE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zexpimp_vx_generate_test_code LINE-SIZE 150.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS get_input_test_code
      RETURNING
        VALUE(result) TYPE string_table.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD get_input_test_code.
    DATA: dbuf                    TYPE xstring,
          message                 TYPE c LENGTH 255,
          code_type_tests         TYPE string_table,
          code_method_class_setup TYPE string_table.

    DATA(tests) = zcl_expimp_vx_tests=>get( ).
    LOOP AT tests REFERENCE INTO DATA(test).
      CALL FUNCTION 'Z_EXPIMP_VX_GENERATE_TEST_CODE'
        DESTINATION 'NONE'
        EXPORTING
          source_code           = test->code
        IMPORTING
          dbuf                  = dbuf
        EXCEPTIONS
          communication_failure = 1 MESSAGE message
          system_failure        = 2 MESSAGE message
          OTHERS                = 3.
      IF sy-subrc = 0.
        DATA(i) = 40.
        DATA(xstring_table) = VALUE xstring_table( FOR j = 0 THEN j + i WHILE j < xstrlen( dbuf )
            LET i2 = nmin( val1 = i val2 = xstrlen( dbuf ) - j ) IN ( dbuf+j(i2) ) ).

*        code_type_tests = VALUE #(
*            BASE code_type_tests
*            ( |          { test->id } TYPE zcl_expimp_importer=>ty_test,| ) ).
*
*        code_method_class_setup = VALUE #(
*            BASE code_method_class_setup
*            ( |    tests-{ test->id } = VALUE #(| )
*            ( |        id   = '{ test->id }'| )
*            ( LINES OF VALUE #( FOR <code_line> IN test->code INDEX INTO k
*            ( |{ COND #( WHEN k = 1 THEN |        code = VALUE #(| ) }| )
*            ( |{ COND #( WHEN k >= 1 THEN |                ( `{ replace( val = <code_line> sub = |`| with = |``| occ = 0 ) }` ){
*                COND #( WHEN k = lines( test->code ) THEN ` )` ) }| ) }| ) ) )
*            ( LINES OF VALUE #( FOR <xstring> IN xstring_table INDEX INTO l
*            ( |{ COND #( WHEN l = 1 THEN `        dbuf = CONV xstring( ` ) }{
*                COND #( WHEN l > 1 THEN `            && ` )
*                }'{ <xstring> }'{
*                COND #( WHEN l = lines( xstring_table ) THEN ` ) ).` ) }| ) ) ) ).
      ENDIF.
    ENDLOOP.

*    result = VALUE #(
*        LET temp_code_type_tests = code_type_tests
*            temp_code_method_class_setup = code_method_class_setup IN
*        ( |  PRIVATE SECTION.| )
*        ( )
*        ( |    METHODS class_setup.| )
*        ( )
*        ( |    TYPES: BEGIN OF ty_tests,| )
*        ( LINES OF temp_code_type_tests )
*        ( |           END OF ty_tests.| )
*        ( )
*        ( |    DATA: tests TYPE ty_tests.| )
*        ( )
*        ( |  METHOD class_setup.| )
*        ( LINES OF temp_code_method_class_setup )
*        ( |  ENDMETHOD.| ) ).
  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.
  DATA(code) = NEW lcl_app( )->get_input_test_code( ).
  LOOP AT code REFERENCE INTO DATA(line).
    WRITE / line->*.
  ENDLOOP.
  ASSERT 1 = 1. " debug helper
*write lines of itab2
