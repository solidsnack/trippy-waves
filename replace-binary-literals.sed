##  Replacements leave the old literal in a comment, for auditing.
s|0b(00000000);|0x00; //\1|g
s|0b(00000001);|0x01; //\1|g
s|0b(00000011);|0x03; //\1|g
s|0b(00000100);|0x04; //\1|g
s|0b(00001000);|0x08; //\1|g
s|0b(00010000);|0x10; //\1|g
s|0b(00010111);|0x17; //\1|g
s|0b(00011000);|0x18; //\1|g
s|0b(00100000);|0x20; //\1|g
s|0b(00111111);|0x3F; //\1|g
s|0b(01000010);|0x42; //\1|g
s|0b(01110000);|0x70; //\1|g
s|0b(01111001);|0x79; //\1|g
s|0b(11001111);|0xCF; //\1|g
s|0b(11110000);|0xF0; //\1|g
s|0b(11111000);|0xF8; //\1|g
s|0b(11111011);|0xFB; //\1|g

