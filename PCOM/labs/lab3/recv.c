// recv.c

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include "common.h"
#include "link_emulator/lib.h"
#include "include/utils.h"

/**
 * You can change these to communicate with another colleague.
 * There are several factors that could stop this from working over the
 * internet, but if you're on the same network it should work.
 * Just fill in their IP here and make sure that you use the same port.
 */

#define HOST "127.0.0.1"
#define PORT 10001

int main(int argc, char **argv) {
    /* Don't modify this */
	printf("good rcv 1 \n");

    init(HOST, PORT);

	printf("good rcv 2 \n");

    struct l3_msg t;

	printf("good rcv 2 \n");

    /* TODO 3.1: In a loop, recv a frame and check if the CRC is good */
    while (1) {
		printf("good rcv 3 \n");
        int len = link_recv(&t, sizeof(struct l3_msg));
        DIE(len < 0, "Receive message");

		

        /* We have to convert it to host order */
        uint32_t recv_sum = ntohl(t.hdr.sum);
        t.hdr.sum = 0;

        /* TODO 2: Change to crc32 */
        uint32_t computed_crc = crc32((uint8_t *)&t, sizeof(struct l3_msg));
        int crc_ok = (recv_sum == computed_crc);




        /* TODO 3.2: If the crc is bad, send a NACK frame */
        if (!crc_ok) {
            struct l3_msg nack_frame;

			printf("Frame corrupted \n");
			
			memset(&nack_frame, 0, sizeof(struct l3_msg));
            
			printf("good rcv 4 \n");
			
			
			link_send(&nack_frame, sizeof(struct l3_msg));
            
			printf("good rcv 5 \n");
			
			continue;  // Go back to the beginning of the loop
        }

		printf("good rcv 6 \n");


        /* TODO 3.2: Otherwise, write the frame payload to a file recv.data */
        FILE *file = fopen("recv.data", "ab");  // Open the file in binary append mode
        DIE(file == NULL, "Error opening file");

        fwrite(t.payload, 1, t.hdr.len, file);
        fclose(file);

        printf("[RECV] len=%d; crc(%s)=0x%08x; payload=\"%.*s\";\n",
               t.hdr.len, crc_ok ? "GOOD" : "BAD", recv_sum, t.hdr.len, t.payload);

        /* TODO 3.3: Adjust the corruption rate */
		/* set the bash variable $CORRUPT with the value of the curruption */
		
		/*  setenv - change or add an environment variable
		* #include <stdlib.h>
       	*
       	* int setenv(const char *name, const char *value, int overwrite);
       	*
       	* int unsetenv(const char *name);
	   	*/
		setenv("CORRUPT", "value_of_corruption", 1);

    }

    return 0;
}
