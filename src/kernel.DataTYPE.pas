UNIT kernel.DataTYPE;


INTERFACE


TYPE

  TdtaType = (aTXT,    // text (ansiStrings)           ansiChar
              SI08,    // signed integer   08 bits     shortInt
              UI08,    // unsigned integer 08 bits     byte
              uTXT,    // text (unicode: strings)      char
              SI16,    // signed integer   16 bits     int16
              UI16,    // unsigned integer 16 bits     word
              SI32,    // signed integer   32 bits     integer
              UI32,    // unsigned integer 32 bits     cardinal
              FT32,    // float            32 bits     single
              SI64,    // signed integer   64 bits     int64
              FT64,    // float            64 bits     double
              RCRD,    // record
              UKNW);

  TdtaUnit = (UNDEFINED,Millimiter,Centimeter,Meter,sqrM,Hectare,sqrKm,
                        Inch,Feet,Mile,sqrMile,Acre);



IMPLEMENTATION



END.
