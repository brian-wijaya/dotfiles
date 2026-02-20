// mosaic_prepare.cpp - Wind-up hook for mosaic skeleton animation
// Fires on: UserPromptSubmit
// Target latency: <5ms
//
// Sends a prepare call to mosaic MCP server at 127.0.0.1:8373.
// Shows skeleton shimmer that dissolves when add_slide fires.
// Never blocks — exits 0 always. No stdout output.

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <poll.h>
#include <errno.h>

// Fixed request — no dynamic allocation
static const char REQUEST_BODY[] =
    R"({"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"prepare","arguments":{"hint":"minimal"}}})";

static const char HTTP_REQUEST_FMT[] =
    "POST /mcp HTTP/1.1\r\n"
    "Host: 127.0.0.1:8373\r\n"
    "Content-Type: application/json\r\n"
    "Content-Length: %d\r\n"
    "Connection: close\r\n"
    "\r\n"
    "%s";

// Drain stdin (hooks receive JSON on stdin — must consume it)
static void drain_stdin() {
    char buf[4096];
    int flags = fcntl(STDIN_FILENO, F_GETFL, 0);
    fcntl(STDIN_FILENO, F_SETFL, flags | O_NONBLOCK);
    while (read(STDIN_FILENO, buf, sizeof(buf)) > 0) {}
}

// Connect with non-blocking + poll timeout
static int connect_with_timeout(int sockfd, struct sockaddr_in* addr, int timeout_ms) {
    int flags = fcntl(sockfd, F_GETFL, 0);
    fcntl(sockfd, F_SETFL, flags | O_NONBLOCK);

    int ret = connect(sockfd, (struct sockaddr*)addr, sizeof(*addr));
    if (ret == 0) {
        fcntl(sockfd, F_SETFL, flags);
        return 0;
    }
    if (errno != EINPROGRESS) return -1;

    struct pollfd pfd = { sockfd, POLLOUT, 0 };
    ret = poll(&pfd, 1, timeout_ms);
    if (ret <= 0) return -1;

    int err = 0;
    socklen_t len = sizeof(err);
    getsockopt(sockfd, SOL_SOCKET, SO_ERROR, &err, &len);
    if (err != 0) return -1;

    fcntl(sockfd, F_SETFL, flags);
    return 0;
}

int main() {
    drain_stdin();

    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) return 0;

    // Disable Nagle for speed
    int one = 1;
    setsockopt(sockfd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));

    struct sockaddr_in addr{};
    addr.sin_family = AF_INET;
    addr.sin_port = htons(8373);
    addr.sin_addr.s_addr = inet_addr("127.0.0.1");

    // Connect with 100ms timeout — if mosaic not running, exit silently
    if (connect_with_timeout(sockfd, &addr, 100) != 0) {
        close(sockfd);
        return 0;
    }

    // Build HTTP request on stack
    char request[512];
    int req_len = snprintf(request, sizeof(request), HTTP_REQUEST_FMT,
                           (int)sizeof(REQUEST_BODY) - 1, REQUEST_BODY);

    // Send — fire and forget, don't wait for response
    write(sockfd, request, req_len);

    // Brief read to avoid RST — 50ms max
    char response[1024];
    struct pollfd pfd = { sockfd, POLLIN, 0 };
    if (poll(&pfd, 1, 50) > 0) {
        read(sockfd, response, sizeof(response));
    }

    close(sockfd);

    // No stdout output — this hook is silent
    return 0;
}
