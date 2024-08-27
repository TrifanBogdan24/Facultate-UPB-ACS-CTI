#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include "link_emulator/lib.h"
#include "include/utils.h"
#include "common.h"

#define HOST "127.0.0.1"
#define PORT 10001

static int ID = 123131;

int recv_frame(char *buf, int size) {
    // TODO 1.1: Call recv_byte() until we receive the frame start delimiter.
    char c1, c2;
    do {
        c1 = recv_byte();
        c2 = recv_byte();
    } while (!(c1 == DLE && c2 == STX));

    // TODO 2.1: The first two 2 * sizeof(int) bytes represent sender and receiver ID.
    int sender_id = recv_byte();
    int receiver_id = recv_byte();

    // TODO 2.2: Check that the frame was sent to me.
    if (receiver_id != ID) {
        // Frame not intended for this receiver.
        return 0;
    }

    // TODO 1.2: Read bytes and copy them to buf until we receive the end of the frame.
    int index = 0;
    char current_char;
    do {
        current_char = recv_byte();
        buf[index++] = current_char;
    } while (!(current_char == DLE && recv_byte() == ETX));

    // If everything went well return the number of bytes received.
    return index;
}

int main(int argc, char **argv) {
    init(HOST, PORT);

    // TODO 1.0: Allocate a buffer and call recv_frame.
    char recv_buffer[100];
    int bytes_received = recv_frame(recv_buffer, sizeof(recv_buffer));

    // TODO 3: Measure latency in a while loop for any frame that contains a timestamp.
    while (bytes_received > 0) {
        // Extract timestamp from the frame payload (assuming payload is null-terminated).
        char *timestamp_str = strchr(recv_buffer, '\0') + 1;

        // Convert timestamp_str to a numerical timestamp (if needed).

        // Print frame_size and latency.
        printf("Received Frame Size: %d, Latency: %d ms\n", bytes_received, /* Calculate latency */);

        // Continue receiving frames.
        bytes_received = recv_frame(recv_buffer, sizeof(recv_buffer));
    }

    printf("[RECEIVER] Finished transmission\n");
    return 0;
}
