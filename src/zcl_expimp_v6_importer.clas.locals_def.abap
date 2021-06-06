*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_dd DEFINITION DEFERRED.
CLASS lcl_dv DEFINITION DEFERRED.

"! Class to read a data object
CLASS lcl_do DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        reader        TYPE REF TO zcl_expimp_reader
      RETURNING
        VALUE(result) TYPE REF TO lcl_do
      RAISING
        zcx_expimp.

    METHODS read
      RETURNING
        VALUE(result) TYPE REF TO lcl_do
      RAISING
        zcx_expimp.

    DATA: reader           TYPE REF TO zcl_expimp_reader READ-ONLY.
    DATA: object_header    TYPE zif_expimp_v6=>ty_object_header READ-ONLY,
          data_object_name TYPE string READ-ONLY,
          "! Data Description
          dd               TYPE REF TO lcl_dd READ-ONLY,
          "! Data Values
          dvs              TYPE TABLE OF REF TO lcl_dv READ-ONLY.

    CONSTANTS:
      c_object_id LIKE zif_expimp_vx=>c_object_id VALUE zif_expimp_vx=>c_object_id.

  PRIVATE SECTION.

ENDCLASS.

"! Data Description (Type of Data Object)
CLASS lcl_dd DEFINITION ABSTRACT.
  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        do            TYPE REF TO lcl_do
      RETURNING
        VALUE(result) TYPE REF TO lcl_dd
      RAISING
        zcx_expimp.

    METHODS constructor
      IMPORTING
        do TYPE REF TO lcl_do.

    METHODS get_type
      RETURNING
        VALUE(result) TYPE zif_expimp_vx=>ty_ityp.

    METHODS get_length
      RETURNING
        VALUE(result) TYPE i.

    METHODS read
      RETURNING
        VALUE(result) TYPE REF TO lcl_dd
      RAISING
        zcx_expimp.

    METHODS get_rtti ABSTRACT
      RETURNING
        VALUE(result) TYPE REF TO cl_abap_datadescr
      RAISING
        zcx_expimp.

    DATA: do               TYPE REF TO lcl_do READ-ONLY,
          reader           TYPE REF TO zcl_expimp_reader READ-ONLY,
          "! describes the type of data definition (ID)
          start_id         TYPE zif_expimp_vx=>ty_dd_id,
          "! Segment from the Export/Import buffer.
          "! It may not be present for primary values, the type from the Object Header is then used.
          data_description TYPE zif_expimp_v6=>ty_data_description READ-ONLY,
          position         TYPE i READ-ONLY.

    CONSTANTS:
      c_ityp  LIKE zif_expimp_vx=>c_ityp VALUE zif_expimp_vx=>c_ityp,
      c_dd_id LIKE zif_expimp_vx=>c_dd_id VALUE zif_expimp_vx=>c_dd_id.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_dv DEFINITION ABSTRACT.
  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        do            TYPE REF TO lcl_do
      RETURNING
        VALUE(result) TYPE REF TO lcl_dv
      RAISING
        zcx_expimp.

    METHODS read
      RETURNING
        VALUE(result) TYPE REF TO lcl_dv
      RAISING
        zcx_expimp.

    METHODS get_value ABSTRACT
      CHANGING
        VALUE(value) TYPE any
      RAISING
        zcx_expimp.

    METHODS constructor
      IMPORTING
        do TYPE REF TO lcl_do
      RAISING
        zcx_expimp.

    METHODS get_structure
      EXPORTING
        structure TYPE any.

    DATA: reader         TYPE REF TO zcl_expimp_reader READ-ONLY,
          data_value     TYPE zif_expimp_v6=>ty_data_value READ-ONLY,
          do             TYPE REF TO lcl_do READ-ONLY,
          "! position of value
          position       TYPE i READ-ONLY,
          "! length of value in number of characters or bytes
          length         TYPE i READ-ONLY.
    CONSTANTS:
      c_dv_id LIKE zif_expimp_vx=>c_dv_id VALUE zif_expimp_vx=>c_dv_id.

  PRIVATE SECTION.

ENDCLASS.
