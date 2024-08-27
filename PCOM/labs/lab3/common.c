// common.c


#include "common.h"

uint8_t simple_csum(uint8_t *buf, size_t len) {
    /* TODO 1.1: Implement the simple checksum algorithm */
    uint32_t sum = 0;

    while (len > 0) {
        sum += *buf;
        buf += 1;
        len -= 1;
    }

    uint8_t checksum = sum % 256;
    return checksum;
}


uint32_t crc32(uint8_t *buf, size_t len) {
    /* CRC32 polynomial: 0xEDB88320 */
    const uint32_t POLY = 0xEDB88320;
    uint32_t crc = ~0;  // Initial value (all bits set to 1)

    /* Iterate through each byte of buf */
    while (len--) {
        /* crc contine restul impartirii la fiecare etapa */
        /* nu ne intereseaza catul */
        /* adunam urmatorii 8 bytes din buffer */
		
        crc ^= *buf++;  // XOR with the next byte in buf

        /* Process each bit in the current byte */
        for (int bit = 0; bit < 8; bit++) {
            /* 10011 ) 11010110110000 = Bytes of payload
                =Poly   10011,,.,,....
                        -----,,.,,....
                         10011,.,,....  (operatia de xor cand primul bit e 1)
                         10011,.,,....
                         -----,.,,....
                          00001.,,....  (asta e noua valoare a lui crc) (crc >> 1) ^ POLY
            */
            if (crc & 1)
                crc = (crc >> 1) ^ POLY;  // XOR with the polynomial if the LSB is 1
            else
                /* 10011 ) 11010110110000 = Bytes of payload
                    =Poly   10011,,.,,....
                            -----,,.,,....
                             10011,.,,....  
                             10011,.,,....
                             -----,.,,....
                              00001.,,....  primul bit e 0, 
                              00000.,,....  
                              -----.,,....
                               00010,,.... am facut shift la dreapta, pentru ca suntem pe **little endian**
                */
                crc = crc >> 1;  // Shift right if the LSB is 0
        }
    }

    /* Finalize CRC value (flip all bits) */
    crc = ~crc;
    return crc;
}

