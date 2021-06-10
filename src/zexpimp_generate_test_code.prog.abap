*&---------------------------------------------------------------------*
*& Report ZEXPIMP_GENERATE_TEST_CODE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zexpimp_generate_test_code.

DATA(tests) = zcl_expimp_tests=>get( ).
LOOP AT tests REFERENCE INTO DATA(test).
  DATA: dbuf    TYPE xstring,
        message TYPE c LENGTH 255,
        code    TYPE string_table.
  CALL FUNCTION 'Z_EXPIMP_GENERATE_TEST_CODE'
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
    code = VALUE #(
        BASE code
        FOR <xstring> IN xstring_table INDEX INTO k
        ( |{ COND #( WHEN k = 1 THEN `xstring = ` ) }{
            COND #( WHEN k > 1 THEN `&& ` )
            }'{ <xstring> }'{
            COND #( WHEN k = lines( xstring_table ) THEN `.` ) }| ) ).
  ENDIF.
ENDLOOP.
ASSERT 1 = 1. " debug helper
*write lines of itab2
