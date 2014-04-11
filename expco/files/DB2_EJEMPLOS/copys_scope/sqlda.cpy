       01 SQLDA sync.
          05 SQLDAID               PIC X(8)  VALUE "SQLDA  ".
          05 SQLDABC               PIC S9(9) COMP-5 value 0.
          05 SQLN                  PIC S9(4) COMP-5 value 0.
          05 SQLD                  PIC S9(4) COMP-5 value 0.
          05 SQLVAR OCCURS 0 TO 1489 TIMES DEPENDING ON SQLD.
             10 SQLTYPE            PIC S9(4) COMP-5.
             10 SQLLEN             PIC S9(4) COMP-5.
             10 SQLDATA            USAGE POINTER.
             10 SQLIND             USAGE POINTER.
             10 SQLNAME.
                15 SQLNAMEL        PIC S9(4) COMP-5.
                15 SQLNAMEC        PIC X(30).

      * Values for SQLTYPE

       78  ESQL-DATE-CHAR              VALUE 384.
       78  ESQL-DATE-CHAR-NULL         VALUE 385.
       78  ESQL-DATE-REC               VALUE 386.
       78  ESQL-DATE-REC-NULL          VALUE 387.
       78  ESQL-TIME-CHAR              VALUE 388.
       78  ESQL-TIME-CHAR-NULL         VALUE 389.
       78  ESQL-TIME-REC               VALUE 390.
       78  ESQL-TIME-REC-NULL          VALUE 391.
       78  ESQL-TIMESTAMP-CHAR         VALUE 392.
       78  ESQL-TIMESTAMP-CHAR-NULL    VALUE 393.
       78  ESQL-TIMESTAMP-REC          VALUE 394.
       78  ESQL-TIMESTAMP-REC-NULL     VALUE 395.
       78  ESQL-LONGVARBINARY          VALUE 404.
       78  ESQL-LONGVARBINARY-NULL     VALUE 405.
       78  ESQL-LONGVARCHAR            VALUE 408.
       78  ESQL-LONGVARCHAR-NULL       VALUE 409.
       78  ESQL-BINARY                 VALUE 444.
       78  ESQL-BINARY-NULL            VALUE 445.
       78  ESQL-VARBINARY              VALUE 446.
       78  ESQL-VARBINARY-NULL         VALUE 447.
       78  ESQL-VARCHAR                VALUE 448.
       78  ESQL-VARCHAR-NULL           VALUE 449.

       78  ESQL-CHARVARYING            VALUE 450.  *> added esq03n31
       78  ESQL-CHARVARYING-NULL       VALUE 451.  *> added esq03n31

       78  ESQL-CHAR                   VALUE 452.
       78  ESQL-CHAR-NULL              VALUE 453.

       78  ESQL-CHAR-FIXED             VALUE 454.  *> added esq03n31
       78  ESQL-CHAR-FIXED-NULL        VALUE 455.  *> added esq03n31

       78  ESQL-DOUBLE                 VALUE 480.
       78  ESQL-DOUBLE-NULL            VALUE 481.
       78  ESQL-REAL                   VALUE 482.
       78  ESQL-REAL-NULL              VALUE 483.
       78  ESQL-DECIMAL                VALUE 484.
       78  ESQL-DECIMAL-NULL           VALUE 485.
       78  ESQL-INTEGER                VALUE 496.
       78  ESQL-INTEGER-NULL           VALUE 497.
       78  ESQL-SMALLINT               VALUE 500.
       78  ESQL-SMALLINT-NULL          VALUE 501.
       78  ESQL-TINYINT                VALUE 502.
       78  ESQL-TINYINT-NULL           VALUE 503.

