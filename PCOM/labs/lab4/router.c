#include <arpa/inet.h> /* ntoh, hton and inet_ functions */
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include "lib.h"
#include "protocols.h"

#include <string.h>

/* Routing table */
struct route_table_entry *rtable;
int rtable_len;

/* Mac table */
struct mac_entry *mac_table;
int mac_table_len;

/*
 Returns a pointer (eg. &rtable[i]) to the best matching route, or NULL if there
 is no matching route.
*/
struct route_table_entry *get_best_route(uint32_t ip_dest) {
	/* TODO 2.2: Implement the LPM algorithm */
	/* We can iterate through rtable for (int i = 0; i < rtable_len; i++). Entries in
	 * the rtable are in network order already */

    struct route_table_entry *best_match = NULL;

    for (int i = 0; i < rtable_len; i++) {
        struct route_table_entry *entry = &rtable[i];

        // Check if the destination IP matches the network address
        if ((ip_dest & entry->mask) == entry->prefix) {
            // Update best_match if this entry is more specific
            if (best_match == NULL || entry->mask > best_match->mask) {
                best_match = entry;
            }
        }
    }

    return best_match;
}

struct mac_entry *get_mac_entry(uint32_t given_ip) {
	/* TODO 2.4: Iterate through the MAC table and search for an entry
	 * that matches given_ip. */

	/* We can iterate thrpigh the mac_table for (int i = 0; i <
	 * mac_table_len; i++) */
	for (int i = 0; i < mac_table_len; i++) {
        struct mac_entry *entry = &mac_table[i];

        // Check if the IP matches the given IP
        if (entry->ip == given_ip) {
            return entry;
        }
    }

	// No matching MAC entry found
    return NULL;
}



void print_mac(uint8_t *mac) {
    printf("%02X:%02X:%02X:%02X:%02X:%02X",
           mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
}


int main(int argc, char *argv[])
{
	int interface;
	char packet[MAX_LEN];
	int packet_len;

	/* Don't touch this */
	init();

	/* Code to allocate the MAC and route tables */
	rtable = malloc(sizeof(struct route_table_entry) * 100);
	/* DIE is a macro for sanity checks */
	DIE(rtable == NULL, "memory");

	mac_table = malloc(sizeof(struct  mac_entry) * 100);
	DIE(mac_table == NULL, "memory");
	
	/* Read the static routing table and the MAC table */
	rtable_len = read_rtable("rtable.txt", rtable);
	mac_table_len = read_mac_table(mac_table);

	printf("reached main function\n");


	while (1) {
		/* We call get_packet to receive a packet. get_packet returns
		the interface it has received the data from. And writes to
		len the size of the packet. */
		printf("reached infinte while loop\n");

		interface = recv_from_all_links(packet, &packet_len);

		printf("get interface\n");

		DIE(interface < 0, "get_message");
		printf("We have received a packet\n");
		
		/* Extract the Ethernet header from the packet. Since protocols are
		 * stacked, the first header is the ethernet header, the next header is
		 * at m.payload + sizeof(struct ether_header) */
		struct ether_header *eth_hdr = (struct ether_header *) packet;
		struct iphdr *ip_hdr = (struct iphdr *)(packet + sizeof(struct ether_header));

		/* Check if we got an IPv4 packet */
		if (eth_hdr->ether_type != ntohs(ETHERTYPE_IP)) {
			printf("Pachetul nu este IPv4. Se ignora pachetul\n");
			continue;
		}

		/* TODO 2.1: Check the ip_hdr integrity using ip_checksum((uint16_t *)ip_hdr, sizeof(struct iphdr)) */
		/*
		 * 
		*/

		int ip_check_field = ntohs(ip_hdr->check);
		int ip_check_sum_val = ip_checksum((uint16_t *)ip_hdr, sizeof(struct iphdr));
		
		if (ip_check_sum_val == 0) {
			// afiseaza valoarea lui ip_checksum si a campului check
			printf("IP checksum: 0x%04X\n", ip_checksum((uint16_t *)ip_hdr, sizeof(struct iphdr)));
			printf("IP heck field: 0x%04X\n", ntohs(ip_hdr->check));


			printf("IP header checksum: %d\n", ip_check_sum_val);
			printf("IP header check field: %d\n", ip_check_field);

			printf("checksum-ul este invalid. Se ignora pachetul.\n");
			continue;
		}

		/* TODO 2.2: Call get_best_route to find the most specific route, continue; (drop) if null */
		struct route_table_entry *best_route = get_best_route(ntohl(ip_hdr->daddr));
		if (best_route == NULL) {
			printf("Nu s-a gasit adresa in tabela de rutare. Se ignora pachetul.\n");
			continue;
		}


		/* TODO 2.3: Check TTL >= 1. Update TLL. Update checksum  */
		if (ip_hdr->ttl < 1) {
			printf("TTL a expirat. Poate a creat o bucla, poate a intrat in prea multe noduri");
			printf("Se ignora pachetul.\n");
			continue;
		}

		/* Update TTL */
		ip_hdr->ttl--;



		/* Update checksum */
		ip_hdr->check = 0;
		ip_hdr->check = ip_checksum((uint16_t *)ip_hdr, sizeof(struct iphdr));

		/* TODO 2.4: Update the ethernet addresses. Use get_mac_entry to find the destination MAC
		 * address. Use get_interface_mac(m.interface, uint8_t *mac) to
		 * find the mac address of our interface. */
		struct mac_entry *dest_mac_entry = get_mac_entry(ntohl(ip_hdr->daddr));
		if (dest_mac_entry == NULL) {
			printf("Destination MAC address not found. Dropping packet.\n");
			continue;
		}

		// Call send_to_link(best_router->interface, packet, packet_len);
		send_to_link(best_route->interface, packet, packet_len);

		/* Update source MAC address */
		get_interface_mac(interface, eth_hdr->ether_shost);



		uint8_t *mac = NULL;

		printf("Packet forwarded successfully to interface %d\n", best_route->interface);
        printf("Destination MAC address: ");

		mac = dest_mac_entry->mac;
    	printf("%02X:%02X:%02X:%02X:%02X:%02X",
           mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
        // print_mac(dest_mac_entry_mac);

        printf("\n");
        printf("Source MAC address: ");

		mac = eth_hdr->ether_shost;
    	printf("%02X:%02X:%02X:%02X:%02X:%02X",
           mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);

        // print_mac(eth_hdr->ether_shost);

        printf("\n");
        printf("Destination IP address: %s\n", inet_ntoa(*(struct in_addr *)&ip_hdr->daddr));
        printf("Source IP address: %s\n", inet_ntoa(*(struct in_addr *)&ip_hdr->saddr));
        printf("Packet length: %d bytes\n", packet_len);
        printf("\n");

	}


	// comanda de verificare in terminal
	// $ sudo pkill ovs-test ; sudo python3 topo.py
}
