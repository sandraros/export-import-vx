*&---------------------------------------------------------------------*
*& Report zexpimp_demo_1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zexpimp_v6_demo.

class lcl_app definition.
  public section.
    methods itab_with_structure
              RAISING
                zcx_expimp_vx.
    methods compare
              RAISING
                zcx_expimp_vx.
    data: blob type xstring.
endclass.

class lcl_app implementation.
  METHOD itab_with_structure.

    DATA: BEGIN OF line,
            aaa TYPE c LENGTH 5,
            bbb TYPE i,
          END OF line,
          itab LIKE TABLE OF line.

*    itab = VALUE #( ).
*    EXPORT the-itab = itab TO DATA BUFFER blob.
*    compare( ).

    itab = VALUE #( ( aaa = |hello| bbb = 8 ) ( aaa = |world| bbb = 20 ) ).
    EXPORT the-itab = itab TO DATA BUFFER blob.
    compare( ).

  ENDMETHOD.

  METHOD compare.
    DATA: importer TYPE REF TO zcl_expimp_v6_importer,
          partab_line TYPE cpar.

    importer = NEW zcl_expimp_v6_importer( blob ).
    data(output) = importer->get_dump( ).

  ENDMETHOD.
endclass.

START-OF-SELECTION.
  DATA(app) = new lcl_app( ).
  app->itab_with_structure( ).
