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
#include <utf8proc.h>

#define COBOL_SOCK_PATH "/var/run/cobol/cobol.sock"
#define BUFSIZE 4096

static void
json_escape_utf8(out, in, max)
    char *out;
    const char *in;
    size_t max;
{
    size_t j = 0;
    const uint8_t *p = (const uint8_t *)in;

    while (*p && j < max - 1) {
        utf8proc_int32_t cp;
        utf8proc_ssize_t len = utf8proc_iterate(p, -1, &cp);
        if (len <= 0)
            break;
        p += len;

        if (j + 6 >= max)
            break;

        switch (cp) {
        case '\"': out[j++] = '\\'; out[j++] = '\"'; break;
        case '\\': out[j++] = '\\'; out[j++] = '\\'; break;
        case '\n': out[j++] = '\\'; out[j++] = 'n';  break;
        case '\r': out[j++] = '\\'; out[j++] = 'r';  break;
        case '\t': out[j++] = '\\'; out[j++] = 't';  break;
        default:
            if (cp < 0x20 || cp == 0x7f)
                j += snprintf(out + j, max - j, "\\u%04x", cp);
            else
                j += utf8proc_encode_char(cp, (uint8_t *)(out + j));
        }
    }

    out[j] = '\0';
}

static int
cobol_handler(r)
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
        ap_rputs("{\"status\":\"error\",\"msg\":\"socket error\"}\n", r);
        return OK;
    }

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, COBOL_SOCK_PATH, sizeof(addr.sun_path) - 1);

    if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, errno, r, "connect(%s) failed", COBOL_SOCK_PATH);
        close(sock);
        ap_rputs("{\"status\":\"error\",\"msg\":\"connect failed\"}\n", r);
        return OK;
    }

    char buf[BUFSIZE] = {0};
    ssize_t len = read(sock, buf, BUFSIZE - 1);
    close(sock);

    if (len > 0) {
        buf[len] = '\0';
        char escaped[BUFSIZE * 2] = {0};
        json_escape_utf8(escaped, buf, sizeof(escaped));
        ap_rprintf(r, "{\"status\":\"ok\",\"cobol\":\"%s\"}\n", escaped);
    } else {
        ap_log_rerror(APLOG_MARK, APLOG_ERR, errno, r, "read() failed");
        ap_rputs("{\"status\":\"error\",\"msg\":\"read error\"}\n", r);
    }

    return OK;
}

static void
cobol_register_hooks(p)
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
