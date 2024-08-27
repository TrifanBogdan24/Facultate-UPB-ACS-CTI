// Protocoale de comunicatii
// Laborator 10 - DNS
// dns.c

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>

int usage(char* name)
{
	printf("Usage:\n\t%s -n <NAME>\n\t%s -a <IP>\n", name, name);
	return 1;
}

// Receives a name and prints IP addresses
void get_ip(char* name)
{
	int ret;
	struct addrinfo hints, *result, *p;

	memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_family = AF_UNSPEC;    // adresele IPv4 si IPv6
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_CANONNAME;  // timpul canonic

	ret = getaddrinfo(name, NULL, &hints, &result);
	if (ret != 0) {
		fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(ret));
		exit(EXIT_FAILURE);
	}

	for (p = result; p != NULL; p = p->ai_next) {
		void *addr;
		char ipstr[INET6_ADDRSTRLEN];

		if (p->ai_family == AF_INET) {
			// adresa IPv4
			struct sockaddr_in *ipv4 = (struct sockaddr_in *)p->ai_addr;
			addr = &(ipv4->sin_addr);
		} else {
			// adresa IPv6
			struct sockaddr_in6 *ipv6 = (struct sockaddr_in6 *)p->ai_addr;
			addr = &(ipv6->sin6_addr);
		}

		inet_ntop(p->ai_family, addr, ipstr, sizeof(ipstr));
		printf("IP address: %s\n", ipstr);
	}

	freeaddrinfo(result);
}

// afiseaza numele si serviciul unei adrese
void get_name(char* ip)
{
	struct sockaddr_in addr;
	char host[1024];
	char service[20];

	memset(&addr, 0, sizeof(struct sockaddr_in));
	addr.sin_family = AF_INET;
	inet_pton(AF_INET, ip, &(addr.sin_addr));

	if (getnameinfo((struct sockaddr *)&addr, sizeof(struct sockaddr), host, sizeof(host), service, sizeof(service), 0) != 0) {
		fprintf(stderr, "getnameinfo failed\n");
		exit(EXIT_FAILURE);
	}

	printf("Name: %s\nService: %s\n", host, service);
}

int main(int argc, char **argv)
{
	if (argc < 3) {
		return usage(argv[0]);
	}

	if (strncmp(argv[1], "-n", 2) == 0) {
		get_ip(argv[2]);
	} else if (strncmp(argv[1], "-a", 2) == 0) {
		get_name(argv[2]);
	} else {
		return usage(argv[0]);
	}

	return 0;
}
