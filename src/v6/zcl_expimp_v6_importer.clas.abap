CLASS zcl_expimp_v6_importer DEFINITION
  INHERITING FROM zcl_expimp_vx_importer
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        dbuf TYPE xstring
      RAISING
        cx_sy_compression_error
        zcx_expimp_vx.

    METHODS get_next_data_object
      RETURNING
        VALUE(result) TYPE cpar
      RAISING
        zcx_expimp_vx.

    METHODS get_dump
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        zcx_expimp_vx.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS _get_next_data_object
      RETURNING
        VALUE(result) TYPE REF TO lcl_do
      RAISING
        zcx_expimp_vx.

    CONSTANTS:
      c_object_id LIKE zif_expimp_vx=>c_object_id VALUE zif_expimp_vx=>c_object_id,
      c_dd_id     LIKE zif_expimp_vx=>c_dd_id VALUE zif_expimp_vx=>c_dd_id.
    DATA: do TYPE REF TO lcl_do.

ENDCLASS.



CLASS zcl_expimp_v6_importer IMPLEMENTATION.

  METHOD constructor.

    super->constructor( dbuf ).

    ASSERT version = '06'.

  ENDMETHOD.

  METHOD _get_next_data_object.

    result = lcl_do=>create( reader )->read( ).

  ENDMETHOD.

  METHOD get_next_data_object.

    do = _get_next_data_object( ).

    result-name = do->data_object_name.

    DATA(rtti) = do->dd->get_rtti( ).
    CREATE DATA result-dref TYPE HANDLE rtti.
    ASSIGN result-dref->* TO FIELD-SYMBOL(<value>).
    LOOP AT do->dvs INTO DATA(dv).
      dv->get_value( CHANGING value = <value> ).
    ENDLOOP.

  ENDMETHOD.

  METHOD get_dump.

    result = NEW lcl_dump( me )->get( ).

  ENDMETHOD.

ENDCLASS.
