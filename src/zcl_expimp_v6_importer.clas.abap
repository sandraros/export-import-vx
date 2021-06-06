CLASS zcl_expimp_v6_importer DEFINITION
  INHERITING FROM zcl_expimp_importer
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        dbuf TYPE xstring
      RAISING
        cx_sy_compression_error
        zcx_expimp.

    METHODS get_next_data_object
      RETURNING
        VALUE(result) TYPE cpar
      RAISING
        zcx_expimp.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS:
      c_object_id LIKE zif_expimp_vx=>c_object_id VALUE zif_expimp_vx=>c_object_id,
      c_dd_id     LIKE zif_expimp_vx=>c_dd_id VALUE zif_expimp_vx=>c_dd_id.

ENDCLASS.



CLASS zcl_expimp_v6_importer IMPLEMENTATION.

  METHOD constructor.

    super->constructor( dbuf ).

    ASSERT version = '06'.

  ENDMETHOD.

  METHOD get_next_data_object.

    DATA(do) = lcl_do=>create( reader )->read( ).

    result-name = do->data_object_name.

    DATA(rtti) = do->dd->get_rtti( ).
    CREATE DATA result-dref TYPE HANDLE rtti.
    ASSIGN result-dref->* TO FIELD-SYMBOL(<value>).
    LOOP AT do->dvs INTO DATA(dv).
      dv->get_value( CHANGING value = <value> ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
