// send.c

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "common.h"
#include "link_emulator/lib.h"
#include <arpa/inet.h>
#include "include/utils.h"

#define HOST "127.0.0.1"
#define PORT 10000

int main(int argc, char **argv) {
    init(HOST, PORT);

    /* Look in common.h for the definition of l3_msg */
    struct l3_msg t;

    /* We set the payload */
    sprintf(t.payload, "Hello my World of PC!");
    t.hdr.len = strlen(t.payload) + 1;

    /* Add the checksum */
    /* Note that we compute the checksum for both header and data. Thus
     * we set the checksum equal to 0 when computing it */
    t.hdr.sum = 0;

    /* Since sum is on 32 bits, we have to convert it to network order */
    t.hdr.sum = htonl(simple_csum((void *)&t, sizeof(struct l3_msg)));

	printf("Payload: %s\n", t.payload);
	printf("Length: %d\n", t.hdr.len);

    /* TODO 2.0: Call crc32 function */

    uint32_t crc_value = crc32((uint8_t *)&t, sizeof(struct l3_msg));

	printf("good 1 \n");


    /* Send the message */
    link_send(&t, sizeof(struct l3_msg));


	printf("good 2 \n");


    /* TODO 3.1: Receive the confirmation */
    struct l3_msg confirmation;
    link_recv(&confirmation, sizeof(struct l3_msg));

	printf("good 3 \n");

    /* TODO 3.2: If we received a NACK, retransmit the previous frame */
    if (ntohl(confirmation.hdr.sum) != crc32((uint8_t *)&confirmation, sizeof(struct l3_msg))) {
        link_send(&t, sizeof(struct l3_msg));
    }

    /* TODO 3.3: Update this to read the content of a file and send it as
     * chunks of that file given a MTU of 1500 bytes */
    const char *filename = "file_to_send.txt";
    FILE *file = fopen(filename, "rb");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    struct stat st;
    stat(filename, &st);
    size_t file_size = st.st_size;

    while (file_size > 0) {
        size_t chunk_size = (file_size > sizeof(t.payload)) ? sizeof(t.payload) : file_size;
        fread(t.payload, 1, chunk_size, file);
        t.hdr.len = chunk_size;

        t.hdr.sum = 0;
        t.hdr.sum = htonl(crc32((uint8_t *)&t, sizeof(struct l3_msg)));

        link_send(&t, sizeof(struct l3_msg));

        file_size -= chunk_size;
    }

    fclose(file);

    return 0;
}
