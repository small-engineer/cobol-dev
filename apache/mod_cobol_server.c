#include "httpd.h"
#include "http_config.h"
#include "http_protocol.h"
#include "http_log.h"
#include "ap_config.h"
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#define COBOL_SOCK_PATH "/var/run/cobol/cobol.sock"
#define BUFSIZE 4096

static void safe_json_escape(out, in, maxlen)
    char *out;
    const char *in;
    size_t maxlen;
{
    size_t i, j = 0;
    for (i = 0; in[i] && j < maxlen - 1; ++i) {
        unsigned char c = (unsigned char)in[i];
        if (j + 6 >= maxlen)
            break;
        switch (c) {
        case '\"': out[j++] = '\\'; out[j++] = '\"'; break;
        case '\\': out[j++] = '\\'; out[j++] = '\\'; break;
        case '\n': out[j++] = '\\'; out[j++] = 'n';  break;
        case '\r': out[j++] = '\\'; out[j++] = 'r';  break;
        case '\t': out[j++] = '\\'; out[j++] = 't';  break;
        default:
            if (c < 0x20)
                j += snprintf(&out[j], maxlen - j, "\\u%04x", c);
            else
                out[j++] = c;
        }
    }
    out[j] = '\0';
}

static int cobol_handler(r)
    request_rec *r;
{
    if (!r->handler || strcmp(r->handler, "cobol-server"))
        return DECLINED;

    r->content_type = "application/json";

    if (r->header_only)
        return OK;

    int sock = socket(AF_UNIX, SOCK_STREAM, 0);
    if (sock < 0) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, errno, r, "socket() failed");
        ap_rprintf(r, "{\"status\":\"error\",\"msg\":\"socket error\"}\n");
        return OK;
    }

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, COBOL_SOCK_PATH, sizeof(addr.sun_path) - 1);

    if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, errno, r, "connect(%s) failed", COBOL_SOCK_PATH);
        close(sock);
        ap_rprintf(r, "{\"status\":\"error\",\"msg\":\"connect failed\"}\n");
        return OK;
    }

    char buf[BUFSIZE] = {0};
    ssize_t len = read(sock, buf, BUFSIZE - 1);
    close(sock);

    if (len > 0) {
        buf[len] = '\0';
        char escaped[BUFSIZE * 2] = {0};
        safe_json_escape(escaped, buf, sizeof(escaped));
        ap_rprintf(r, "{\"status\":\"ok\",\"cobol\":\"%s\"}\n", escaped);
    } else {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, errno, r, "read() failed");
        ap_rprintf(r, "{\"status\":\"error\",\"msg\":\"read error\"}\n");
    }

    return OK;
}

static void cobol_register_hooks(p)
    apr_pool_t *p;
{
    ap_hook_handler(cobol_handler, NULL, NULL, APR_HOOK_MIDDLE);
}

module AP_MODULE_DECLARE_DATA cobol_server_module = {
    STANDARD20_MODULE_STUFF,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    cobol_register_hooks
};
