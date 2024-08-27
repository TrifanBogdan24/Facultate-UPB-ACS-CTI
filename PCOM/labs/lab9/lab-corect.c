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

int main(int argc, char *argv[])
{
    char *message;
    char *response;
    int sockfd;

    sockfd = open_connection("54.170.241.232", 8080, AF_INET, SOCK_STREAM, 0);
    // Ex 1.1: GET dummy from main server
    message = "GET /api/v1/dummy HTTP/1.1\r\nHost: 54.170.241.232:8080\r\nCookie: connect.sid=s%3AP-dsyEqL6roGjrxGYW-aHun7R7kSlCBm.WXFIjjyFInT7a0aOXcglV8bumf2YO%2FCdEnRk%2FekYv4s\r\n\r\n";
    send_to_server(sockfd, message);
    response = receive_from_server(sockfd);
    printf("Response:\n%s\n", response);

    free(response);
    // Ex 1.2: POST dummy and print response from main server
    message = "POST /api/v1/dummy HTTP/1.1\r\nHost: 54.170.241.232:8080\r\nCookie: connect.sid=s%3AP-dsyEqL6roGjrxGYW-aHun7R7kSlCBm.WXFIjjyFInT7a0aOXcglV8bumf2YO%2FCdEnRk%2FekYv4s\r\n\r\n";
    send_to_server(sockfd, message);
    
    response = receive_from_server(sockfd);
    printf("Response:\n%s\n", response);

    free(response);
    // Ex 2: Login into main server
    message = "POST /api/v1/auth/login HTTP/1.1\r\nHost: 54.170.241.232:8080\r\nContent-Type: application/x-www-form-urlencoded\r\nCookie: connect.sid=s%3AP-dsyEqL6roGjrxGYW-aHun7R7kSlCBm.WXFIjjyFInT7a0aOXcglV8bumf2YO%2FCdEnRk%2FekYv4s\r\nContent-Length: 33\r\n\r\nusername=student&password=student\r\n\r\n";
    send_to_server(sockfd, message);
    
    response = receive_from_server(sockfd);
    printf("Response:\n%s\n", response);

    free(response);
    // Ex 3: GET weather key from main server
    message = "GET /api/v1/weather/key HTTP/1.1\r\nHost: 54.170.241.232:8080\r\nCookie: connect.sid=s%3AP-dsyEqL6roGjrxGYW-aHun7R7kSlCBm.WXFIjjyFInT7a0aOXcglV8bumf2YO%2FCdEnRk%2FekYv4s\r\n\r\n";
    send_to_server(sockfd, message);
    
    response = receive_from_server(sockfd);
    printf("Response:\n%s\n", response);

    free(response);
    // Ex 4: GET weather data from OpenWeather API
    message = "GET / HTTP/1.1\r\nHost: api.openweathermap.org:80\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: 62\r\n\r\nlat=44.4268&lon=26.1025&appid=b912dd495585fbf756dc6d8f415a7649\r\n\r\n";
    send_to_server(sockfd, message);
    
    response = receive_from_server(sockfd);
    printf("Response:\n%s\n", response);

    free(response);
    // Ex 5: POST weather data for verification to main server
    // Ex 6: Logout from main server

    // BONUS: make the main server return "Already logged in!"

    // free the allocated data at the end!

    return 0;
}
