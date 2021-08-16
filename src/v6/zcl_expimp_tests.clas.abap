CLASS zcl_expimp_tests DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES : BEGIN OF ty_test,
              id   TYPE c LENGTH 30,
              code TYPE string_table,
            END OF ty_test,
            ty_tests TYPE HASHED TABLE OF ty_test WITH UNIQUE KEY id.

    CLASS-METHODS get
      RETURNING
        VALUE(result) TYPE ty_tests.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_expimp_tests IMPLEMENTATION.
  METHOD get.
  result = value #(
    ( id   = 'primitive_c'
      code = VALUE #(
        ( |DATA variable TYPE c LENGTH 2 VALUE 'DD'.| ) ) )
    ( id   = 'primitive_i'
      code = VALUE #(
        ( |DATA variable TYPE i VALUE 68.| ) ) )
    ( id   = 'primitive_f'
      code = VALUE #(
        ( |DATA variable TYPE f VALUE '3.14'.| ) ) )
    ( id   = 'primitive_string'
      code = VALUE #(
        ( |DATA variable TYPE string VALUE `DD`.| ) ) )
    ( id   = 'nested_structure'
      code = VALUE #(
        ( |TYPES: BEGIN OF struct,   | )
        ( |         a TYPE i,        | )
        ( |         BEGIN OF sub,    | )
        ( |           b TYPE i,      | )
        ( |         END OF sub,      | )
        ( |       END OF struct.     | )
        ( |DATA variable TYPE struct.| ) ) )
    ( id   = 'filler'
      code = VALUE #(
        ( |TYPES: BEGIN OF struct,    | )
        ( |         a TYPE i,         | )
        ( |         b TYPE decfloat16,| )
        ( |       END OF struct.      | )
        ( |DATA variable TYPE struct. | ) ) )
    ( id   = 'clike_flat_structure'
      code = VALUE #(
        ( |TYPES: BEGIN OF struct,    | )
        ( |         a TYPE c LENGTH 5,| )
        ( |         b TYPE c LENGTH 5,| )
        ( |       END OF struct.      | )
        ( |DATA variable TYPE struct. | ) ) )
    ( id   = 'filler_flat_structure'
      code = VALUE #(
        ( |TYPES: BEGIN OF struct,    | )
        ( |         a TYPE c LENGTH 5,| )
        ( |         b TYPE decfloat16,| )
        ( |       END OF struct.      | )
        ( |DATA variable TYPE struct. | ) ) )
    ( id   = 'deep_structure'
      code = VALUE #(
        ( |TYPES: BEGIN OF struct,| )
        ( |         a TYPE string,| )
        ( |         b TYPE string,| )
        ( |       END OF struct.  | )
        ( |DATA variable TYPE struct.| ) ) )
    ( id   = 'itab_without_structure'
      code = VALUE #(
        ( |DATA variable TYPE TABLE OF string.| ) ) )
    ( id   = 'itab_with_structure'
      code = VALUE #(
        ( |TYPES: BEGIN OF struct,| )
        ( |         a TYPE string,| )
        ( |       END OF struct.  | )
        ( |DATA variable TYPE TABLE OF struct.| ) ) ) ).

  ENDMETHOD.
ENDCLASS.
