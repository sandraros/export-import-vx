CLASS zcl_expimp_importer DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        dbuf TYPE xstring
      RAISING
        cx_sy_compression_error
        zcx_expimp.

    DATA: transport_header TYPE zif_expimp_vx=>ty_transport_header READ-ONLY,
          version          TYPE zif_expimp_vx=>ty_transport_header-version READ-ONLY,
          reader           TYPE REF TO zcl_expimp_reader READ-ONLY.
    TYPES : BEGIN OF ty_test,
              id   TYPE c LENGTH 30,
              code TYPE string_table,
              dbuf TYPE xstring,
            END OF ty_test.

  PROTECTED SECTION.

    TYPES: ty_byte TYPE x LENGTH 1.

    "! If the 5th byte = '02', the BLOB must be uncompressed by kernel AB_IMPORT_DECOMPRESS
    METHODS uncompress
      IMPORTING
        dbuf          TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring
      RAISING
        cx_sy_compression_error
        zcx_expimp.

  PRIVATE SECTION.

    METHODS get_reader
      IMPORTING
        dbuf          TYPE xstring
      RETURNING
        VALUE(reader) TYPE REF TO zcl_expimp_reader.

ENDCLASS.



CLASS zcl_expimp_importer IMPLEMENTATION.


  METHOD constructor.

    IF xstrlen( dbuf ) < 16.
      RAISE EXCEPTION TYPE zcx_expimp.
    ENDIF.

    IF dbuf+4(1) = '02'. "compressed data
      " decompress export/import document if compressed
      DATA(dbuf2) = uncompress( dbuf ).
    ELSE.
      dbuf2 = dbuf.
    ENDIF.

    reader = get_reader( dbuf2 ).

  ENDMETHOD.


  METHOD uncompress.

    " This code is copied from subroutine READ_BLOB of program RSINDX00 and adapted.

    DATA: l_off   TYPE i,
          l_len   TYPE i,
          lr_blob TYPE REF TO indx_clust_blob ##NEEDED,
          datalen TYPE p,
          BEGIN OF l_xdat,
            srtf2  TYPE indx-srtf2,
            clustr TYPE indx-clustr,
            clustd TYPE x LENGTH 1000,
          END OF l_xdat,
          l_otab LIKE TABLE OF l_xdat,
          l_stab LIKE TABLE OF l_xdat.

    datalen = xstrlen( dbuf ).

    IF datalen < 4.
      RAISE EXCEPTION TYPE zcx_expimp.
    ENDIF.

    " re-format the XSTRING into data cluster
    l_off = 0.
    DO.
      IF l_off < datalen.
        IF datalen - l_off >= 1000.
          l_len = 1000.
        ELSE.
          l_len = datalen - l_off.
        ENDIF.
        l_xdat-srtf2 = sy-index - 1.
        l_xdat-clustr = l_len.
        l_xdat-clustd = dbuf+l_off(l_len).
        APPEND l_xdat TO l_stab.
        l_off = l_off + 1000.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.


    CLEAR result.

    CALL 'AB_IMPORT_DECOMPRESS'
      ID 'SRCTAB' FIELD l_stab[]
      ID 'DSTTAB' FIELD l_otab[].

    CLEAR l_stab.


    LOOP AT l_otab REFERENCE INTO DATA(l_o).
      CONCATENATE result l_o->clustd(l_o->clustr) INTO result IN BYTE MODE.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_reader.

    " TRANSPORT HEADER
    DATA: transport_header_x TYPE x LENGTH 16.

    transport_header_x = dbuf(16).
    reader = zcl_expimp_reader=>create( encoding = '1100' input = transport_header_x ).
    reader->read_structure( IMPORTING data = transport_header length = DATA(transport_header_length) ).

    version = transport_header-version.

    DATA(sap_codepage) = CONV cpcodepage( cl_abap_codepage=>convert_from(
        source   = CONV xstring( transport_header-codepage )
        codepage = 'US-ASCII' ) ).
    DATA(codepage) = cl_abap_codepage=>sap_to_http( sap_codepage ).

    " Recreate CONV with right code page
    reader = zcl_expimp_reader=>create(
            encoding = CONV #( sap_codepage )
            endian   = COND #( WHEN transport_header-intformat = '01' THEN 'B' ELSE 'L' )
            input    = dbuf ).
    reader->skip_x( transport_header_length ).

  ENDMETHOD.

ENDCLASS.
