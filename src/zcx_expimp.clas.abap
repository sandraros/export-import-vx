CLASS zcx_expimp DEFINITION
  INHERITING FROM cx_static_check
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_text REDEFINITION.
    CONSTANTS:
      table_end_expected  TYPE sotr_conc VALUE '058C3CCA090D42D491C532F58B6F3FFD',
      end_of_data_objects TYPE sotr_conc VALUE '158C3CCA090D42D491C532F58B6F3FFD',
      invalid_dd_id TYPE sotr_conc VALUE '258C3CCA090D42D491C532F58B6F3FFD'.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_expimp IMPLEMENTATION.


  METHOD get_text.
    CASE textid.
      WHEN table_end_expected.
        result = 'Table end expected (byte AE)'(001).
      WHEN end_of_data_objects.
        result = 'End of data objects read (byte 04)'(002).
      WHEN invalid_dd_id.
        result = 'Invalid Data Description ID (may be a valid Data Value ID)'(003).
      WHEN OTHERS.
        super->get_text( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
