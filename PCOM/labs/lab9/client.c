#include <stdio.h>      /* printf, sprintf */
#include <stdlib.h>     /* exit, atoi, malloc, free */
#include <unistd.h>     /* read, write, close */
#include <string.h>     /* memcpy, memset */
#include <sys/socket.h> /* socket, connect */
#include <netinet/in.h> /* struct sockaddr_in, struct sockaddr */
#include <netdb.h>      /* struct hostent, gethostbyname */
#include <arpa/inet.h>
#include "helpers.h"
#include "requests.h"

#define LOCALHOST_IPv4 "127.0.0.1"
#define SERVER_IPv4 "54.170.241.232"


int main(int argc, char *argv[])
{
    char *message;
    char *response;
    int sockfd;
  
     // Ex 1.1: GET dummy from main server
    sockfd = open_connection(LOCALHOST_IPv4, 8080, AF_INET, SOCK_STREAM, 0);
    message = compute_get_request(SERVER_IPv4, "/api/v1/dummy ", NULL, NULL, 0);
    send_to_server(sockfd, message);
    response = receive_from_server(sockfd);
    printf("GET Dummy Response: %s\n", response);
    close_connection(sockfd);
    
    // Ex 1.2: POST dummy and print response from main server
    sockfd = open_connection(LOCALHOST_IPv4, 8080, AF_INET, SOCK_STREAM, 0);
    message = compute_post_request(SERVER_IPv4, "/api/v1/dummy", "application/x-www-form-urlencoded", NULL, 0, NULL, 0);
    send_to_server(sockfd, message);
    response = receive_from_server(sockfd);
    printf("Response: %s\n", response);
    close_connection(sockfd);
    
    // Ex 2: Login into main server
    sockfd = open_connection(LOCALHOST_IPv4, 8080, AF_INET, SOCK_STREAM, 0);
    char *body_data[] = {"username=student", "password=student", NULL};
    message = compute_post_request(SERVER_IPv4, "/login", "application/x-www-form-urlencoded", body_data, 2, NULL, 0);
    send_to_server(sockfd, message);
    response = receive_from_server(sockfd);
    printf("Response: %s\n", response);
    close_connection(sockfd);



    // Ex 3: GET weather key from main server
    sockfd = open_connection(LOCALHOST_IPv4, 8080, AF_INET, SOCK_STREAM, 0);
    message = compute_get_request(SERVER_IPv4, "/weather_key", NULL, NULL, 0);
    send_to_server(sockfd, message);
    response = receive_from_server(sockfd);
    printf("GET Weather Key Response: %s\n", response);
    close_connection(sockfd);

    
    // Ex 4: GET weather data from OpenWeather API
    sockfd = open_connection("api.openweathermap.org", 80, AF_INET, SOCK_STREAM, 0);
    const char *api_key = "your_api_key_here";
    char query_params[BUFLEN];
    sprintf(query_params, "q=city&appid=%s", api_key);
    message = compute_get_request("api.openweathermap.org", "/data/2.5/weather", query_params, NULL, 0);
    send_to_server(sockfd, message);
    response = receive_from_server(sockfd);
    printf("GET Weather Data Response: %s\n", response);
    close_connection(sockfd);

    
    // Ex 5: POST weather data for verification to main server
    sockfd = open_connection(LOCALHOST_IPv4, 8080, AF_INET, SOCK_STREAM, 0);
    char *wheather_body_data[] = {"data=", response, NULL};
    message = compute_post_request(SERVER_IPv4, "/weather_verification", "application/x-www-form-urlencoded", wheather_body_data, 2, NULL, 0);
    send_to_server(sockfd, message);
    response = receive_from_server(sockfd);
    printf("Response: %s\n", response);
    close_connection(sockfd);



    // Ex 6: Logout from main server
    sockfd = open_connection(LOCALHOST_IPv4, 8080, AF_INET, SOCK_STREAM, 0);
    message = compute_get_request(SERVER_IPv4, "/logout", NULL, NULL, 0);
    send_to_server(sockfd, message);
    response = receive_from_server(sockfd);
    printf("Response: %s\n", response);
    close_connection(sockfd);

    

    close_connection(sockfd);
    free(response);
    return 0;
}
