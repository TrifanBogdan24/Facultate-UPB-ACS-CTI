#include <arpa/inet.h> /* ntoh, hton and inet_ functions */
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <net/ethernet.h>
#include "lib.h"
#include "protocols.h"

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
    struct route_table_entry *best_route = NULL;
    uint32_t best_mask = 0;

    for (int i = 0; i < rtable_len; i++) {
        struct route_table_entry *entry = &rtable[i];
        uint32_t entry_prefix = entry->mask & ip_dest;
        
        if (entry_prefix == entry->prefix && entry->mask >= best_mask) {
            best_route = entry;
            best_mask = entry->mask;
        }
    }
    
    return best_route;
}

struct mac_entry *get_mac_entry(uint32_t given_ip) {
	/* TODO 2.4: Iterate through the MAC table and search for an entry
	 * that matches given_ip. */
	for (int i = 0; i < mac_table_len; i++) {
        if (mac_table[i].ip == given_ip) {
            return &mac_table[i];
        }
    }
    return NULL;

	/* We can iterate thrpigh the mac_table for (int i = 0; i <
	 * mac_table_len; i++) */
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

	while (1) {
		/* We call get_packet to receive a packet. get_packet returns
		the interface it has received the data from. And writes to
		len the size of the packet. */
		interface = recv_from_all_links(packet, &packet_len);
		DIE(interface < 0, "get_message");
		printf("We have received a packet\n");
		
		/* Extract the Ethernet header from the packet. Since protocols are
		 * stacked, the first header is the ethernet header, the next header is
		 * at m.payload + sizeof(struct ether_header) */
		struct ether_header *eth_hdr = (struct ether_header *) packet;
		struct iphdr *ip_hdr = (struct iphdr *)(packet + sizeof(struct ether_header));

		/* Check if we got an IPv4 packet */
		if (eth_hdr->ether_type != ntohs(ETHERTYPE_IP)) {
			printf("Ignored non-IPv4 packet\n");
			continue;
		}

		/* TODO 2.1: Check the ip_hdr integrity using ip_checksum((uint16_t *)ip_hdr, sizeof(struct iphdr)) */
		if (ip_checksum((uint16_t *)ip_hdr, sizeof(struct iphdr)) != 0) {
            printf("IP header checksum verification failed. Packet dropped.\n");
            continue;
        }

		/* TODO 2.2: Call get_best_route to find the most specific route, continue; (drop) if null */
		struct route_table_entry *best_route = get_best_route(ip_hdr);

		/* TODO 2.3: Check TTL >= 1. Update TLL. Update checksum  */
		if (best_route == NULL) {
			printf("No matching route found. Packet dropped.\n");
			continue;
		}

		/* Update TTL */
		if (ip_hdr->ttl <= 1) {
			printf("TTL expired. Packet dropped.\n");
			continue;
		}
		ip_hdr->ttl--; // Decrement TTL
		ip_hdr->check = 0; // Reset checksum for recalculation
		ip_hdr->check = ip_checksum((uint16_t *)ip_hdr, sizeof(struct iphdr));

		/* TODO 2.4: Update the ethernet addresses. Use get_mac_entry to find the destination MAC
		 * address. Use get_interface_mac(m.interface, uint8_t *mac) to
		 * find the mac address of our interface. */
		struct mac_entry *dst_mac_entry = get_mac_entry(best_route->next_hop);

		if (dst_mac_entry == NULL) {
			printf("No MAC entry found for the destination IP. Packet dropped.\n");
			continue; // Drop the packet
		}

		// Get source MAC address of outgoing interface
		uint8_t src_mac[ETH_ALEN];
		get_interface_mac(interface, src_mac);

		// Update destination and source MAC addresses in the Ethernet header
		memcpy(eth_hdr->ether_dhost, dst_mac_entry->mac, ETH_ALEN); // Destination MAC
		memcpy(eth_hdr->ether_shost, src_mac, ETH_ALEN);
		  
		send_to_link(best_route->interface, packet, packet_len);
		
	}
}
