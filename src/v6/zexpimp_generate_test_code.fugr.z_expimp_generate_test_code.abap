FUNCTION Z_EXPIMP_GENERATE_TEST_CODE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SOURCE_CODE) TYPE  STRING_TABLE
*"  EXPORTING
*"     VALUE(DBUF) TYPE  XSTRING
*"----------------------------------------------------------------------
  DATA(itab) = VALUE string_table(
      ( |PROGRAM.| )
      ( |FORM export_to_data_buffer| )
      ( |CHANGING dbuf TYPE xstring.| )
      ( LINES OF source_code )
      ( |EXPORT a = variable TO DATA BUFFER dbuf.| )
      ( |ENDFORM.| ) ).

  DATA program TYPE program.
  GENERATE SUBROUTINE POOL itab NAME program.
  IF sy-subrc = 0.
    PERFORM export_to_data_buffer IN PROGRAM (program)
      CHANGING dbuf.
  ENDIF.

ENDFUNCTION.
