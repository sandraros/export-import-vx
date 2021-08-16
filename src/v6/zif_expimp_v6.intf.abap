INTERFACE zif_expimp_v6
  PUBLIC .

  INTERFACES zif_expimp_vx.

  ALIASES:
  ty_byte FOR zif_expimp_vx~ty_byte,
  ty_object_id FOR zif_expimp_vx~ty_object_id,
  ty_ityp FOR zif_expimp_vx~ty_ityp,
  ty_dd_id FOR zif_expimp_vx~ty_dd_id,
  ty_dv_id FOR zif_expimp_vx~ty_dv_id.

  "==========================================================
  "
  "             code below taken from RSINDX00
  "
  "==========================================================

***** IMPLEMENTATION OF C-STRUCTURES FROM ABCONNE.C

* (2) Object header

*     release 2.2 to 3.0F


  TYPES:
    "! Object header
    BEGIN OF ty_object_header,
      "! Field type category
      id    TYPE ty_object_id,
      "! Field type
      ityp  TYPE ty_ityp,
      "! Decimals for type P
      decs  TYPE ty_byte,
      "! Field length in number of bytes
      leng  TYPE x LENGTH 4,
      "! Length of the whole Object block including the header, useful to reach directly the next object; or zero if it's the last object
      next  TYPE x LENGTH 4,
      "! Length of object name, in number of characters
      nlen  TYPE ty_byte,
      "! Hash key for UID
      thash TYPE x LENGTH 4,
      "! Type identifier (check info)
      typid TYPE x LENGTH 16,
    END OF ty_object_header.

  TYPES:
    "! data description
    BEGIN OF ty_data_description,
      id   TYPE ty_dd_id,
      type TYPE ty_ityp,
      decs TYPE ty_byte,
      flen TYPE x LENGTH 4,
    END OF ty_data_description.

  TYPES:
      BEGIN OF ty_data_value,
        id  TYPE ty_dv_id,
        len TYPE x LENGTH 4,
      END OF ty_data_value,

      BEGIN OF ty_data_value_table,
        lines TYPE x LENGTH 4,
      END OF ty_data_value_table,

      BEGIN OF ty_data_value_c_x_string,
        id  TYPE ty_dv_id,
        len TYPE x LENGTH 4,
      END OF ty_data_value_c_x_string,

      BEGIN OF ty_data_value_end,
        id  TYPE ty_dv_id,
      END OF ty_data_value_end,

    "! data area headers
    BEGIN OF ty_dah,

      BEGIN OF interval,
        id  TYPE ty_dv_id,
        len TYPE x LENGTH 4,
      END OF interval,

      BEGIN OF table,
        id    TYPE ty_dv_id,
        len   TYPE x LENGTH 4,
        lines TYPE x LENGTH 4,
      END OF table,

      BEGIN OF string_xstring,
        id  TYPE ty_dv_id,
        len TYPE x LENGTH 4,
      END OF string_xstring,

    END OF ty_dah.

ENDINTERFACE.
