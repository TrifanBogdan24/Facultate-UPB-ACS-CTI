#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "link_emulator/lib.h"
#include "include/utils.h"
#include "common.h"

#define HOST "127.0.0.1"
#define PORT 10000

static int ID = 123131;

int send_frame(char *buf, int size) {
    // TODO 1.1: Create a new frame.
    struct Frame frame;

    // TODO 1.2: Copy the data from buffer to our frame structure.
    strncpy(frame.payload, buf, sizeof(frame.payload));

    // TODO 2.1: Set the destination and source.
    frame.source_id = ID;
    frame.destination_id = 456789;  // Set the destination ID accordingly.

    // TODO 1.3: Iterate through sizeof(struct Frame) bytes calling send_bytes.
    send_bytes((char *)&frame, sizeof(frame));

    // If all went well, return 0.
    return 0;
}

int main(int argc, char **argv) {
    init(HOST, PORT);

    // TODO 1.0: Get some input in a buffer and call send_frame with it.
    char send_buffer[] = "Hello, this is a message to be sent.";
    send_frame(send_buffer, sizeof(send_buffer));

    // TODO 3.1: Get a timestamp of the current time and copy it into the payload.
    char timestamp[20];  // Adjust the size based on your timestamp format.
    get_current_timestamp(timestamp);

    // TODO 3.0: Update the maximum size of the payload in Frame to 100, send the frame.
    struct Frame timestamp_frame;
    strncpy(timestamp_frame.payload, timestamp, sizeof(timestamp_frame.payload));
    timestamp_frame.source_id = ID;
    timestamp_frame.destination_id = 456789;  // Set the destination ID accordingly.
    send_bytes((char *)&timestamp_frame, sizeof(timestamp_frame));

    // TODO 3.0: Update the maximum size of the payload in Frame to 300, send the frame.
    // Similar steps as above.

    return 0;
}
