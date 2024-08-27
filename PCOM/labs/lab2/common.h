#define DLE (char)0
#define STX (char)2
#define ETX (char)3

struct __attribute__((packed)) Frame {
    char frame_delim_start[2]; /* DLE STX */

    int source_id;            /* Source ID */
    int destination_id;       /* Destination ID */

    char payload[300];        /* Maximum payload size */
    char frame_delim_end[2];   /* DLE ETX */
};
