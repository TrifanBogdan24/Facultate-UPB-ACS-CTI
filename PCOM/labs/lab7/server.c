#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "common.h"
#include "helpers.h"

#define MAX_CONNECTIONS 32

int receive_and_send(int connfd1, int connfd2, size_t len) {
  int bytes_received;
  char buffer[len];

  bytes_received = recv_all(connfd1, buffer, len);
  if (bytes_received == 0) {
    return 0;
  }
  DIE(bytes_received < 0, "recv");

  printf("Server received from client: %s\n", buffer); // Afișează mesajul primit de la client

  int rc = send_all(connfd2, buffer, len);
  if (rc <= 0) {
    perror("send_all");
    return -1;
  }

  return bytes_received;
}

void run_chat_server(int listenfd) {
  struct sockaddr_in client_addr1;
  struct sockaddr_in client_addr2;
  socklen_t clen1 = sizeof(client_addr1);
  socklen_t clen2 = sizeof(client_addr2);

  int connfd1 = -1;
  int connfd2 = -1;
  int rc;

  rc = listen(listenfd, 2);
  DIE(rc < 0, "listen");

  printf("Astept conectarea primului client...\n");
  connfd1 = accept(listenfd, (struct sockaddr *)&client_addr1, &clen1);
  DIE(connfd1 < 0, "accept");

  printf("Astept connectarea clientului 2...\n");

  connfd2 = accept(listenfd, (struct sockaddr *)&client_addr2, &clen2);
  DIE(connfd2 < 0, "accept");

  while (1) {
    printf("Primesc de la 1 si trimit catre 2...\n");
    int rc = receive_and_send(connfd1, connfd2, sizeof(struct chat_packet));
    if (rc <= 0) {
      break;
    }

    rc = receive_and_send(connfd2, connfd1, sizeof(struct chat_packet));
    if (rc <= 0) {
      break;
    }
  }

  close(connfd1);
  close(connfd2);
}

void run_chat_multi_server(int listenfd) {

  struct pollfd poll_fds[MAX_CONNECTIONS];
  int num_clients = 1;
  int rc;

  struct chat_packet received_packet;

  rc = listen(listenfd, MAX_CONNECTIONS);
  DIE(rc < 0, "listen");

  poll_fds[0].fd = listenfd;
  poll_fds[0].events = POLLIN;

  while (1) {

    rc = poll(poll_fds, num_clients, -1);
    DIE(rc < 0, "poll");

    for (int i = 0; i < num_clients; i++) {
      if (poll_fds[i].revents & POLLIN) {
        if (poll_fds[i].fd == listenfd) {
          struct sockaddr_in cli_addr;
          socklen_t cli_len = sizeof(cli_addr);
          int newsockfd =
              accept(listenfd, (struct sockaddr *)&cli_addr, &cli_len);
          DIE(newsockfd < 0, "accept");

          poll_fds[num_clients].fd = newsockfd;
          poll_fds[num_clients].events = POLLIN;
          num_clients++;

          printf("Noua conexiune de la %s, port %d, socket client %d\n",
                 inet_ntoa(cli_addr.sin_addr), ntohs(cli_addr.sin_port),
                 newsockfd);
        } else {
          int rc = recv_all(poll_fds[i].fd, &received_packet,
                            sizeof(received_packet));
          DIE(rc < 0, "recv");

          if (rc == 0) {
            printf("Socket-ul client %d a inchis conexiunea\n", i);
            close(poll_fds[i].fd);

            for (int j = i; j < num_clients - 1; j++) {
              poll_fds[j] = poll_fds[j + 1];
            }

            num_clients--;

          } else {
            printf("S-a primit de la clientul de pe socketul %d mesajul: %s\n",
                   poll_fds[i].fd, received_packet.message);

            for (int j = 0; j < num_clients; j++) {
                if (poll_fds[j].fd != listenfd && poll_fds[j].fd != poll_fds[i].fd) {
                    int rc = send_all(poll_fds[j].fd, &received_packet, sizeof(received_packet));
                    if (rc <= 0) {
                        perror("send_all");
                        // Handle error or connection closure if necessary
                    }
                }
            }
          }
        }
      }
    }
  }
}

int main(int argc, char *argv[]) {
  if (argc != 3) {
    printf("\n Usage: %s <ip> <port>\n", argv[0]);
    return 1;
  }

  uint16_t port;
  int rc = sscanf(argv[2], "%hu", &port);
  DIE(rc != 1, "Given port is invalid");

  int listenfd = socket(AF_INET, SOCK_STREAM, 0);
  DIE(listenfd < 0, "socket");

  struct sockaddr_in serv_addr;
  socklen_t socket_len = sizeof(struct sockaddr_in);

  int enable = 1;
  if (setsockopt(listenfd, SOL_SOCKET, SO_REUSEADDR, &enable, sizeof(int)) < 0)
    perror("setsockopt(SO_REUSEADDR) failed");

  memset(&serv_addr, 0, socket_len);
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(port);
  rc = inet_pton(AF_INET, argv[1], &serv_addr.sin_addr.s_addr);
  DIE(rc <= 0, "inet_pton");

  rc = bind(listenfd, (const struct sockaddr *)&serv_addr, sizeof(serv_addr));
  DIE(rc < 0, "bind");

  // run_chat_server(listenfd);
  run_chat_multi_server(listenfd);

  close(listenfd);

  return 0;
}
